# treinar_modelos_sarima.R
# (opcional) instalações:
# install.packages(c("dplyr","lubridate","forecast","tibble","tidyr"))

library(dplyr)
library(lubridate)
library(forecast)
library(tibble)
library(tidyr)

# ===== 1) Carregar e pré-processar =====
# dados <- dados_ap2_sudeste
dados <- vendas_combustiveis_m3_2010_2025
names(dados) <- c("Ano", "Mes", "Região", "UF", "Produto", "Vendas")

# fatores + numérico
dados$UF      <- as.factor(dados$UF)
dados$Produto <- as.factor(dados$Produto)
dados$Vendas  <- as.numeric(gsub(",", ".", dados$Vendas))

# limpar "REGIÃO "
dados <- dados %>%
  mutate(
    Região = Região %>%
      as.character() %>%
      gsub("^\\s*REGI(Ã|A)O\\s+", "", ., ignore.case = TRUE) %>%
      trimws() %>%
      toupper() %>%
      factor()
  )

# montar Data a partir de Ano + Mes ("JAN","FEV",...)
mes_map <- c("JAN"=1,"FEV"=2,"MAR"=3,"ABR"=4,"MAI"=5,"JUN"=6,
             "JUL"=7,"AGO"=8,"SET"=9,"OUT"=10,"NOV"=11,"DEZ"=12)

dados <- dados %>%
  mutate(
    Mes_num = mes_map[toupper(Mes)],
    Data    = make_date(year = Ano, month = Mes_num, day = 1)
  ) %>%
  select(-Ano, -Mes, -Mes_num) %>%
  arrange(UF, Produto, Data)

# tirar as linhas onde o produto é Óleo COmbustível ou Querosene Iluminante
dados <- dados %>%
  filter(!Produto %in% c("ÓLEO COMBUSTÍVEL", "QUEROSENE ILUMINANTE"))

# ===== 2) Parâmetros do treino (ajuste UMA vez) =====
# último mês existente nos dados (ex: 2025-09-01)
last_month <- floor_date(max(dados$Data, na.rm = TRUE), unit = "month")

# janela de teste = últimos 12 meses
test_end   <- last_month                         # 2025-09-01
test_start <- last_month %m-% months(11)         # 2024-10-01
test_dates <- seq.Date(test_start, test_end, by = "month")

# janela de treino = todo histórico até o mês anterior ao teste
train_start <- floor_date(min(dados$Data, na.rm = TRUE), unit = "month")
# train_end   <- test_start %m-% months(1)         # 2024-09-01
train_end   <- test_end                            # 2025-09-01

# helpers
completa_meses <- function(df) {
  df <- df %>% arrange(Data) %>% select(UF, Produto, Data, Vendas)
  ini <- as.Date(sprintf("%04d-%02d-01", year(min(df$Data)), month(min(df$Data))))
  fim <- as.Date(sprintf("%04d-%02d-01", year(max(df$Data)), month(max(df$Data))))
  cal <- tibble(Data = seq(ini, fim, by = "month"))
  left_join(cal, df, by = "Data") %>%
    fill(UF, Produto, .direction = "downup")
}

# ===== 3) Treinar SARIMA por UF × Produto =====
series <- dados %>%
  group_by(UF, Produto) %>%
  group_split()

fit_one <- function(df) {
  df <- completa_meses(df)
  df_tr <- df %>% filter(Data >= train_start, Data <= train_end)
  if (nrow(df_tr) < 24) return(NULL)  # mínimo de 24 meses só por segurança
  
  ts_tr <- ts(
    df_tr$Vendas,
    frequency = 12,
    start = c(year(min(df_tr$Data)), month(min(df_tr$Data)))
  )
  
  fit <- auto.arima(ts_tr, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  key <- paste0(df$UF[1], "|", df$Produto[1])
  list(key = key, model = fit)
}

res <- lapply(series, fit_one)
res <- Filter(Negate(is.null), res)

# lista nomeada de modelos
modelos <- setNames(lapply(res, `[[`, "model"), lapply(res, `[[`, "key"))

# metadados úteis
meta <- list(
  train_start = train_start,
  train_end   = train_end,
  freq        = "month",
  chaves      = tibble::tibble(
    key = names(modelos),
    UF = sub("\\|.*$", "", names(modelos)),
    Produto = sub("^.*\\|", "", names(modelos))
  )
)

# salvar para uso na API Shiny
saveRDS(list(modelos = modelos, meta = meta), file = "modelos_sarima_uf_produto_2010_2025_completo.rds")
