# ===== Pacotes =====
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(forecast)   # para forecast::forecast (SARIMA)
  library(stringr)
  library(readr)
})

# ===== Entrada =====
# 1) modelo(s) SARIMA
bundle_path <- "modelos_sarima_uf_produto_2010_set2024_all.rds"
bundle <- readRDS(bundle_path)
modelos <- if (!is.null(bundle$modelos)) bundle$modelos else bundle

# 2) dados: data.frame com colunas: Região, UF, Produto, Vendas, Data (mensal)
#    Assume-se que 'dados' já está no ambiente.
stopifnot(all(c("Região","UF","Produto","Vendas","Data") %in% names(dados)))

# Tipos mínimos
dados <- dados %>%
  mutate(
    UF      = as.character(UF),
    Produto = as.character(Produto),
    Data    = as.Date(Data)
  )

# Janela de avaliação fora do treino
test_start <- as.Date("2024-10-01")
test_end   <- as.Date("2025-09-01")
test_dates <- seq.Date(test_start, test_end, by = "month")

# ===== Funções de métricas =====
metricas_series <- function(y, yhat) {
  # Remove casos inválidos para métricas percentuais
  ok_mape <- is.finite(y) & is.finite(yhat) & y != 0
  
  err  <- yhat - y
  mae  <- mean(abs(err), na.rm = TRUE)
  rmse <- sqrt(mean(err^2, na.rm = TRUE))
  mape <- if (any(ok_mape)) mean(abs((yhat[ok_mape] - y[ok_mape]) / y[ok_mape])) * 100 else NA_real_
  smape <- mean(2 * abs(yhat - y) / (abs(yhat) + abs(y)), na.rm = TRUE) * 100
  
  # R² clássico contra a média do observado
  ss_res <- sum((y - yhat)^2, na.rm = TRUE)
  ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- if (ss_tot > 0) 1 - ss_res/ss_tot else NA_real_
  
  me <- mean(err, na.rm = TRUE)  # viés
  
  # U de Theil (comparação com naïve "random walk": yhat_naive_t = y_{t-1})
  # alinhando por lag:
  if (length(y) >= 2) {
    y_lag <- dplyr::lag(y, 1)
    # remover primeiro ponto sem lag
    idx <- !is.na(y_lag) & is.finite(y) & is.finite(yhat)
    if (any(idx)) {
      rmse_model <- sqrt(mean((yhat[idx] - y[idx])^2, na.rm = TRUE))
      rmse_naive <- sqrt(mean((y_lag[idx] - y[idx])^2, na.rm = TRUE))
      theil_u <- if (rmse_naive > 0) rmse_model / rmse_naive else NA_real_
    } else {
      theil_u <- NA_real_
    }
  } else {
    theil_u <- NA_real_
  }
  
  tibble(
    MAE = mae, RMSE = rmse, MAPE = mape, sMAPE = smape,
    R2 = r2, ME_bias = me, TheilU = theil_u
  )
}

# Utilitário: previsões mensais com datas
forecast_with_dates <- function(model, h, start_date) {
  fc <- forecast::forecast(model, h = h)
  tibble(
    Data = seq.Date(from = start_date, by = "month", length.out = h),
    .pred = as.numeric(fc$mean)
  )
}

# ===== Avaliação por UF × Produto =====
# Colete os pares que têm dados no período de teste
pares <- dados %>%
  filter(Data %in% test_dates) %>%
  distinct(UF, Produto)

# Função para obter a chave do modelo (AJUSTE AQUI, se necessário)
model_key_fn <- function(uf, produto) {
  # Exemplos de padrões comuns — escolha a linha que reflete como você salvou:
  # return(paste(uf, produto, sep = "|"))
  # return(paste(uf, produto, sep = "_"))
  # return(paste0(uf, "::", produto))
  # return(glue::glue("{uf}__{produto}"))
  paste(uf, produto, sep = "|")  # <- padrão adotado aqui
}

# Loop funcional nos pares
resultados <- pares %>%
  mutate(
    chave = pmap_chr(list(UF, Produto), model_key_fn),
    tem_modelo = chave %in% names(modelos)
  ) %>%
  rowwise() %>%
  mutate(
    metrics = list({
      uf   <- UF
      prod <- Produto
      
      if (!tem_modelo) {
        return(tibble(MAE=NA_real_, RMSE=NA_real_, MAPE=NA_real_, sMAPE=NA_real_,
                      R2=NA_real_, ME_bias=NA_real_, TheilU=NA_real_, n_obs=0))
      }
      
      mod <- modelos[[model_key_fn(uf, prod)]]
      h <- length(test_dates)
      
      preds <- forecast_with_dates(mod, h = h, start_date = test_start)
      
      obs <- dados %>%
        filter(UF == uf, Produto == prod, Data %in% test_dates) %>%
        arrange(Data) %>%
        select(Data, Vendas)
      
      joined <- obs %>%
        right_join(preds, by = "Data") %>%
        rename(.obs = Vendas) %>%
        arrange(Data)
      
      y    <- joined$.obs
      yhat <- joined$.pred
      
      err   <- yhat - y
      mae   <- mean(abs(err), na.rm = TRUE)
      rmse  <- sqrt(mean(err^2, na.rm = TRUE))
      ok_mape <- is.finite(y) & is.finite(yhat) & y != 0
      mape  <- if (any(ok_mape)) mean(abs((yhat[ok_mape] - y[ok_mape]) / y[ok_mape])) * 100 else NA_real_
      smape <- mean(2 * abs(yhat - y) / (abs(yhat) + abs(y)), na.rm = TRUE) * 100
      
      ss_res <- sum((y - yhat)^2, na.rm = TRUE)
      ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
      r2 <- if (ss_tot > 0) 1 - ss_res/ss_tot else NA_real_
      
      me <- mean(err, na.rm = TRUE)
      
      if (length(y) >= 2) {
        y_lag <- dplyr::lag(y, 1)
        idx <- !is.na(y_lag) & is.finite(y) & is.finite(yhat)
        if (any(idx)) {
          rmse_model <- sqrt(mean((yhat[idx] - y[idx])^2, na.rm = TRUE))
          rmse_naive <- sqrt(mean((y_lag[idx] - y[idx])^2, na.rm = TRUE))
          theil_u <- if (rmse_naive > 0) rmse_model / rmse_naive else NA_real_
        } else {
          theil_u <- NA_real_
        }
      } else {
        theil_u <- NA_real_
      }
      
      tibble(MAE = mae, RMSE = rmse, MAPE = mape, sMAPE = smape,
             R2 = r2, ME_bias = me, TheilU = theil_u,
             n_obs = sum(!is.na(y)))
    })
  ) %>%
  ungroup() %>%
  tidyr::unnest(metrics) %>%
  arrange(MAPE) %>%
  select(UF, Produto, n_obs, MAE, RMSE, MAPE, sMAPE, R2, ME_bias, TheilU, tem_modelo)

# ===== Saídas =====
# Top 10 melhores (menor MAPE)
print(head(resultados, 10))

# Salvar completo para o artigo
library(openxlsx)
openxlsx::write.xlsx(resultados, "metricas_sarima_uf_produto_out2024_set2025.xlsx", overwrite = TRUE)
cat("Arquivo salvo: metricas_sarima_uf_produto_out2024_set2025.xlsx\n")

# (Opcional) ranking por UF: melhor produto por UF
melhores_por_uf <- resultados %>%
  group_by(UF) %>%
  slice_min(order_by = MAPE, n = 1, with_ties = FALSE) %>%
  ungroup()

print(melhores_por_uf)
