# app.R — Shiny com 4 abas e controles revisados

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(tibble)
library(scales)

# =========================
# Modelos SARIMA (já treinados)
# =========================
bundle  <- readRDS("modelos_sarima_uf_produto_2010_2025_completo.rds")
modelos <- bundle$modelos
meta    <- bundle$meta

# Garante Date
meta$train_end <- as.Date(meta$train_end)

# =========================
# Dados históricos (partimos de 'dados' no ambiente)
# Espera colunas: UF (chr), Produto (chr), Data (Date), Vendas (num)
# =========================
stopifnot(all(c("UF","Produto","Data","Vendas") %in% names(dados)))
dados <- dados %>%
  mutate(
    UF      = as.character(UF),
    Produto = as.character(Produto),
    Data    = as.Date(Data)
  )

# completa meses por UF × Produto
historico <- dados %>%
  arrange(UF, Produto, Data) %>%
  group_by(UF, Produto) %>%
  complete(Data = seq(min(Data), max(Data), by = "month")) %>%
  fill(UF, Produto, .direction = "downup") %>%
  ungroup()

# helpers
months_between <- function(d1, d2) 12*(year(d2)-year(d1)) + (month(d2)-month(d1))
fmt_num <- label_number(accuracy = 1, scale_cut = cut_short_scale())  # K, M, B
fmt_num_txt <- label_number(accuracy = 0.1, scale_cut = cut_short_scale())

ufs_all  <- sort(unique(historico$UF))
prod_all <- sort(unique(historico$Produto))

produtos_por_uf <- function(u) {
  historico %>%
    filter(UF == u) %>%
    pull(Produto) %>%
    unique() %>%
    sort()
}

# =========================
# UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* organiza checkboxes em colunas nas abas 2 e 3 */
      .cols-2 { column-count: 2; }
      .cols-3 { column-count: 3; }
      .shiny-options-group { break-inside: avoid; }
    "))
  ),
  titlePanel("Painel de Séries Temporais e Previsões"),
  sidebarLayout(
    # Painel esquerdo: controles apenas para Gráfico 1 (Histórico) e Gráfico 4 (Previsão)
    sidebarPanel(
      selectInput("uf_base", "UF:", choices = ufs_all, selected = ufs_all[1]),
      uiOutput("produto_base_ui"),
      selectInput("mes", "Mês:", choices = sprintf("%02d", 1:12), selected = "01"),
      numericInput("ano", "Ano:", value = year(Sys.Date()) + 1, min = 2010, step = 1),
      actionButton("go", "Prever")
    ),
    mainPanel(
      tabsetPanel(
        # ================== Aba 1 ==================
        tabPanel("1) Histórico (UF × Produto)",
                 h4("Histórico da combinação UF/Produto"),
                 plotOutput("plot_hist", height = 360),
                 verbatimTextOutput("txt_hist")
        ),
        
        # ================== Aba 2 ==================
        tabPanel("2) Comparar UFs (mesmo Produto)",
                 fluidRow(
                   column(
                     12,
                     wellPanel(
                       h5("Controles desta aba"),
                       selectInput("produto_global", "Produto:", choices = prod_all, selected = prod_all[1]),
                       tags$div(class = "cols-3",
                                checkboxGroupInput("ufs_compare", "UFs para comparar:",
                                                   choices = ufs_all, selected = ufs_all))
                     )
                   )
                 ),
                 plotOutput("plot_cmp_ufs", height = 360),
                 verbatimTextOutput("txt_cmp_ufs")
        ),
        
        # ================== Aba 3 ==================
        tabPanel("3) Comparar Produtos (mesma UF)",
                 fluidRow(
                   column(
                     12,
                     wellPanel(
                       h5("Controles desta aba"),
                       p("A UF usada aqui é a mesma escolhida no painel esquerdo."),
                       uiOutput("produtos_multi_ui")
                     )
                   )
                 ),
                 plotOutput("plot_cmp_prod", height = 360),
                 verbatimTextOutput("txt_cmp_prod")
        ),
        
        # ================== Aba 4 ==================
        tabPanel("4) Previsão (UF × Produto × Mês/Ano)",
                 h4("Resultado da Previsão"),
                 verbatimTextOutput("txt_pred"),
                 plotOutput("plot_pred", height = 360)
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  # Produtos dependem da UF base (para abas 1 e 4)
  output$produto_base_ui <- renderUI({
    prods <- produtos_por_uf(input$uf_base)
    selectInput("produto_base", "Produto:", choices = prods, selected = prods[1])
  })
  
  # Produtos múltiplos para a UF base (aba 3)
  output$produtos_multi_ui <- renderUI({
    prods <- produtos_por_uf(input$uf_base)
    # seleciona até 6 por padrão (ou todos se menos que isso)
    sel <- head(prods, min(6, length(prods)))
    tags$div(class = "cols-2",
             checkboxGroupInput("produtos_multi", "Produtos para comparar:", choices = prods, selected = sel)
    )
  })
  
  # ===== Aba 1 — Histórico =====
  output$plot_hist <- renderPlot({
    req(input$uf_base, input$produto_base)
    df <- historico %>%
      filter(UF == input$uf_base, Produto == input$produto_base) %>%
      arrange(Data)
    
    validate(need(nrow(df) > 0, "Sem histórico para essa combinação UF/Produto."))
    
    ggplot(df, aes(x = Data, y = Vendas)) +
      geom_line() +
      geom_point(size = 1) +
      scale_y_continuous(labels = fmt_num) +
      labs(title = paste0("Histórico — ", input$uf_base, " | ", input$produto_base),
           x = NULL, y = "Vendas (histórico)") +
      theme_minimal()
  })
  
  output$txt_hist <- renderText({
    req(input$uf_base, input$produto_base)
    df <- historico %>% filter(UF == input$uf_base, Produto == input$produto_base)
    if (nrow(df) == 0) return("")
    rng <- paste0(format(min(df$Data), "%Y-%m"), " … ", format(max(df$Data), "%Y-%m"))
    tot <- sum(df$Vendas, na.rm = TRUE)
    paste0("Período do histórico: ", rng,
           "\nObservações: ", nrow(df),
           "\nSoma de vendas no período: ", fmt_num_txt(tot))
  })
  
  # ===== Aba 2 — Comparar UFs (mesmo produto) =====
  output$plot_cmp_ufs <- renderPlot({
    req(input$produto_global, input$ufs_compare)
    df <- historico %>%
      filter(Produto == input$produto_global, UF %in% input$ufs_compare) %>%
      arrange(Data)
    
    validate(need(nrow(df) > 0, "Sem histórico para esse produto nas UFs escolhidas."))
    
    ggplot(df, aes(x = Data, y = Vendas, color = UF)) +
      geom_line() +
      scale_y_continuous(labels = fmt_num) +
      labs(title = paste0("Comparação entre UFs — Produto: ", input$produto_global),
           x = NULL, y = "Vendas (histórico)") +
      theme_minimal()
  })
  
  output$txt_cmp_ufs <- renderText({
    req(input$produto_global, input$ufs_compare)
    df <- historico %>% filter(Produto == input$produto_global, UF %in% input$ufs_compare)
    if (nrow(df) == 0) return("")
    rng <- paste0(format(min(df$Data), "%Y-%m"), " … ", format(max(df$Data), "%Y-%m"))
    paste0("UFs: ", paste(sort(unique(df$UF)), collapse = ", "),
           "\nPeríodo: ", rng,
           "\nSéries: ", length(unique(df$UF)))
  })
  
  # ===== Aba 3 — Comparar Produtos (mesma UF) =====
  output$plot_cmp_prod <- renderPlot({
    req(input$uf_base, input$produtos_multi)
    df <- historico %>%
      filter(UF == input$uf_base, Produto %in% input$produtos_multi) %>%
      arrange(Data)
    
    validate(need(nrow(df) > 0, "Sem histórico para os produtos escolhidos nessa UF."))
    
    ggplot(df, aes(x = Data, y = Vendas, color = Produto)) +
      geom_line() +
      scale_y_continuous(labels = fmt_num) +
      labs(title = paste0("Comparação de Produtos — UF: ", input$uf_base),
           x = NULL, y = "Vendas (histórico)") +
      theme_minimal()
  })
  
  output$txt_cmp_prod <- renderText({
    req(input$uf_base, input$produtos_multi)
    df <- historico %>% filter(UF == input$uf_base, Produto %in% input$produtos_multi)
    if (nrow(df) == 0) return("")
    rng <- paste0(format(min(df$Data), "%Y-%m"), " … ", format(max(df$Data), "%Y-%m"))
    paste0("UF: ", input$uf_base,
           "\nProdutos: ", paste(sort(unique(df$Produto)), collapse = ", "),
           "\nPeríodo: ", rng,
           "\nSéries: ", length(unique(df$Produto)))
  })

  # ===== Aba 4 — Previsão =====
  observeEvent(input$go, {
    req(input$uf_base, input$produto_base, input$mes, input$ano)
    
    key <- paste0(input$uf_base, "|", input$produto_base)
    
    # 1) Modelo existe?
    if (!key %in% names(modelos)) {
      output$txt_pred  <- renderText("Modelo não encontrado para essa UF × Produto.")
      output$plot_pred <- renderPlot(plot.new())
      return()
    }
    
    # 2) Datas seguras
    train_end <- meta$train_end
    if (is.na(train_end)) {
      output$txt_pred  <- renderText("train_end inválido nos metadados do modelo.")
      output$plot_pred <- renderPlot(plot.new())
      return()
    }
    
    start_pred <- train_end %m+% months(1)
    target <- tryCatch(
      as.Date(sprintf("%04d-%02d-01", as.integer(input$ano), as.integer(input$mes))),
      error = function(e) NA
    )
    
    if (is.na(target)) {
      output$txt_pred  <- renderText("Data alvo inválida.")
      output$plot_pred <- renderPlot(plot.new())
      return()
    }
    
    if (target <= train_end) {
      output$txt_pred  <- renderText(
        paste0("A data escolhida (", format(target, "%Y-%m"),
               ") é anterior/igual ao fim do treino (",
               format(train_end, "%Y-%m"), "). Selecione um mês após ",
               format(train_end, "%Y-%m"), ".")
      )
      output$plot_pred <- renderPlot(plot.new())
      return()
    }
    
    # 3) Horizonte h (precisa ser >= 1)
    h <- months_between(start_pred, target) + 1L
    if (!is.finite(h) || h < 1L) {
      output$txt_pred  <- renderText("Horizonte de previsão inválido (h).")
      output$plot_pred <- renderPlot(plot.new())
      return()
    }
    
    fit <- modelos[[key]]
    
    # 4) forecast() com tryCatch
    fc <- tryCatch(
      forecast::forecast(fit, h = h),
      error = function(e) {
        attr(NULL, "err") <- conditionMessage(e)
        NULL
      }
    )
    
    if (is.null(fc)) {
      msg <- attr(fc, "err")
      if (is.null(msg)) msg <- "Erro desconhecido ao gerar a previsão."
      output$txt_pred  <- renderText(paste("Falha na previsão:", msg))
      output$plot_pred <- renderPlot(plot.new())
      return()
    }
    
    # 5) Extrai passo alvo + janela estendida de previsão
    idx_target <- h
    yhat <- as.numeric(fc$mean[idx_target])
    lo80 <- as.numeric(fc$lower[idx_target, "80%"])
    hi80 <- as.numeric(fc$upper[idx_target, "80%"])
    lo95 <- as.numeric(fc$lower[idx_target, "95%"])
    hi95 <- as.numeric(fc$upper[idx_target, "95%"])
    
    output$txt_pred <- renderText(paste0(
      "UF: ", input$uf_base, " | Produto: ", input$produto_base, "\n",
      "Mês-alvo: ", format(target, "%Y-%m"), "\n",
      "Modelo: ", class(fit)[1], "\n",
      "Previsão: ", fmt_num_txt(yhat), " m³ (", fmt_num_txt(yhat * 1000), " L)", "\n",
      "IC 80%: [", fmt_num_txt(lo80), ", ", fmt_num_txt(hi80), "]  ",
      "IC 95%: [", fmt_num_txt(lo95), ", ", fmt_num_txt(hi95), "]"
    ))
    
    # ===== JANELA: 4,5 anos para trás e 3 meses para frente =====
    x_ini <- target %m-% months(54)     # 54 = 4,5 anos
    x_fim <- target %m+% months(3)
    
    # Previsão até x_fim
    horizon_plus3 <- months_between(start_pred, x_fim) + 1L
    validate(need(is.finite(horizon_plus3) && horizon_plus3 >= 1L,
                  "Horizonte estendido inválido."))
    
    fc_full <- tryCatch(
      forecast::forecast(fit, h = horizon_plus3),
      error = function(e) { attr(NULL, "err") <- conditionMessage(e); NULL }
    )
    if (is.null(fc_full)) {
      msg <- attr(fc_full, "err"); if (is.null(msg)) msg <- "Erro ao gerar a previsão estendida."
      output$plot_pred <- renderPlot(plot.new()); output$txt_pred <- renderText(paste("Falha na previsão:", msg))
      return()
    }
    
    dates_fc <- seq(start_pred, by = "month", length.out = horizon_plus3)
    df_fc <- tibble(
      Data = dates_fc,
      Pred = as.numeric(fc_full$mean),
      Lo95 = as.numeric(fc_full$lower[, "95%"]),
      Hi95 = as.numeric(fc_full$upper[, "95%"])
    ) %>% filter(Data >= start_pred, Data <= x_fim)
    
    # Histórico para a mesma combinação, limitado à janela
    df_hist <- historico %>%
      filter(UF == input$uf_base,
             Produto == input$produto_base,
             Data >= x_ini, Data <= train_end) %>%
      select(Data, Vendas)
    
    # ===== Grade mensal contínua da janela =====
    grid <- tibble(Data = seq(x_ini, x_fim, by = "month")) %>%
      left_join(df_hist, by = "Data") %>%
      left_join(df_fc,   by = "Data") %>%
      mutate(
        # usa histórico até o fim do treino; depois, previsão
        Valor = ifelse(Data <= train_end, Vendas, Pred),
        Tipo  = ifelse(Data <= train_end, "Histórico", "Previsto")
      )
    
    # Observação: se houver NA de vendas dentro do histórico (falhas na base),
    # você pode interpolar para não quebrar a linha. Descomente a próxima linha
    # se quiser isso:
    # grid$Valor <- ifelse(is.na(grid$Valor) & grid$Data <= train_end,
    #                      zoo::na.approx(grid$Valor, x = grid$Data, na.rm = FALSE),
    #                      grid$Valor)
    
    # ===== Plot único, contínuo mês a mês =====
    output$plot_pred <- renderPlot({
      ggplot() +
        # incerteza só sobre a parte prevista
        geom_ribbon(
          data = grid %>% filter(Tipo == "Previsto"),
          aes(x = Data, ymin = Lo95, ymax = Hi95),
          alpha = 0.2
        ) +
        # linha contínua (histórico → previsto)
        geom_line(data = grid, aes(Data, Valor)) +
        # pontinhos só na parte prevista (opcional)
        geom_point(data = grid %>% filter(Tipo == "Previsto"),
                   aes(Data, Valor), size = 1.5) +
        geom_vline(xintercept = target, linetype = "dotted") +
        scale_y_continuous(labels = fmt_num) +
        labs(
          title = paste0("Histórico e Previsão — Janela: ",
                         format(x_ini, "%Y-%m"), " a ", format(x_fim, "%Y-%m")),
          x = NULL, y = "Vendas"
        ) +
        theme_minimal()
    })
  })
}

shinyApp(ui = ui, server = server)
