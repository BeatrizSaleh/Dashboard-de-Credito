# app.R
# Dashboard de Crédito - versão rápida usando data/dados_bcb.rds
#
# Este app:
#  - NÃO chama a API do BCB diretamente
#  - Lê os dados prontos de data/dados_bcb.rds
#  - Usa os campos já calculados (var_mm, var_aa, etc.)
#
# Visão Geral Tab:
#  - Mostra dados consolidados de 12 meses para:
#    * Saldo Total, PF, PJ (valores mais recentes com variação 12m)
#    * Concessões Total, PF, PJ (acumulado dos últimos 12 meses)
#    * Juros Total, PF, PJ (média dos últimos 12 meses)
#    * Inadimplência Total, PF, PJ (média dos últimos 12 meses)
#
# Certifique-se de que:
#  - o arquivo data/dados_bcb.rds existe
#  - foi gerado pelo update_data.R

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

#----------------------------------------
# 0) Carrega dados pré-processados (.rds)
#----------------------------------------

if (file.exists("data/dados_bcb.rds")) {
  dados_bcb <- readRDS("data/dados_bcb.rds")
} else {
  warning("Arquivo data/dados_bcb.rds não encontrado. O app inicializará sem dados.")
  dados_bcb <- tibble()
}

# Metadados dos indicadores (mesma estrutura do update_data.R)
indicadores_meta <- tibble::tribble(
  ~grupo,           ~carteira,      ~segmento, ~subgrupo,       ~nome_curto,                                     ~id_sgs,
  "Saldo",          "Total",        "PF",      "Geral",         "Saldo total PF",                                20541,
  "Saldo",          "Total",        "PJ",      "Geral",         "Saldo total PJ",                                20540,
  "Saldo",          "Total",        "Total",   "Geral",         "Saldo total",                                   20539,
  "Saldo",          "Direcionado",  "PF",      "Geral",         "Saldo direc PF",                                20606,
  "Saldo",          "Direcionado",  "PJ",      "Geral",         "Saldo direc PJ",                                20594,
  "Saldo",          "Direcionado",  "Total",   "Geral",         "Saldo direc total",                             20593,
  "Saldo",          "Livre",        "PF",      "Geral",         "Saldo livre PF",                                20570,
  "Saldo",          "Livre",        "PJ",      "Geral",         "Saldo livre PJ",                                20543,
  "Saldo",          "Livre",        "Total",   "Geral",         "Saldo livre total",                             20542,
  
  "Concessões",     "Total",        "PF",      "Geral",         "Concessões totais PF",                          20633,
  "Concessões",     "Total",        "PJ",      "Geral",         "Concessões totais PJ",                          20632,
  "Concessões",     "Total",        "Total",   "Geral",         "Concessões totais",                             20631,
  "Concessões",     "Direcionado",  "PF",      "Geral",         "Concessões direc PF",                           20698,
  "Concessões",     "Direcionado",  "PJ",      "Geral",         "Concessões direc PJ",                           20686,
  "Concessões",     "Direcionado",  "Total",   "Geral",         "Concessões direc total",                        20685,
  "Concessões",     "Livre",        "PF",      "Geral",         "Concessões livres PF",                          20662,
  "Concessões",     "Livre",        "PJ",      "Geral",         "Concessões livres PJ",                          20635,
  "Concessões",     "Livre",        "Total",   "Geral",         "Concessões livres total",                       20634,
  
  "Juros",          "Livre",        "PF",      "Geral",         "Juros livre PF",                                20740,
  "Juros",          "Livre",        "PJ",      "Geral",         "Juros livre PJ",                                20718,
  "Juros",          "Livre",        "Total",   "Geral",         "Juros livre total",                             20717,
  "Juros",          "Direcionado",  "PF",      "Geral",         "Juros direc PF",                                20768,
  "Juros",          "Direcionado",  "PJ",      "Geral",         "Juros direc PJ",                                20757,
  "Juros",          "Direcionado",  "Total",   "Geral",         "Juros direc total",                             20756,
  "Juros",          "Total",        "PF",      "Geral",         "Juros total PF",                                20716,
  "Juros",          "Total",        "PJ",      "Geral",         "Juros total PJ",                                20715,
  "Juros",          "Total",        "Total",   "Geral",         "Juros total",                                   20714,
  
  "Inadimplência",  "Total",        "PF",      "Geral",         "Inadimplência PF",                              21084,
  "Inadimplência",  "Total",        "PJ",      "Geral",         "Inadimplência PJ",                              21083,
  "Inadimplência",  "Total",        "Total",   "Geral",         "Inadimplência total",                           21082,
  
  "Spread",         "Total",        "PF",      "Geral",         "Spread PF",                                     20785,
  "Spread",         "Total",        "PJ",      "Geral",         "Spread PJ",                                     20784,
  "Spread",         "Total",        "Total",   "Geral",         "Spread total",                                  20783,
  
  "Saldo",          "Livre",        "PF",      "Veículos",      "Saldo veículos PF",                             20581,
  "Saldo",          "Livre",        "PJ",      "Veículos",      "Saldo veículos PJ",                             20553,
  "Concessões",     "Livre",        "PF",      "Veículos",      "Concessões veículos PF",                        20673,
  "Concessões",     "Livre",        "PJ",      "Veículos",      "Concessões veículos PJ",                        20645,
  "Inadimplência",  "Total",        "PF",      "Veículos",      "Inadimplência veículos PF",                     21121,
  "Inadimplência",  "Total",        "PJ",      "Veículos",      "Inadimplência veículos PJ",                     21096,
  "Juros",          "Total",        "PF",      "Veículos",      "Juros veículos PF",                             20749,
  "Juros",          "Total",        "PJ",      "Veículos",      "Juros veículos PJ",                             20728,
  
  "Saldo",          "Direcionado",  "PF",      "Imobiliário",   "Saldo imobiliário PF",                          20612,
  "Saldo",          "Direcionado",  "PJ",      "Imobiliário",   "Saldo imobiliário PJ",                          20600,
  "Concessões",     "Direcionado",  "PF",      "Imobiliário",   "Concessões imobiliário PF",                     20704,
  "Concessões",     "Direcionado",  "PJ",      "Imobiliário",   "Concessões imobiliário PJ",                     20692,
  "Inadimplência",  "Total",        "PF",      "Imobiliário",   "Inadimplência imobiliário PF",                  21151,
  "Inadimplência",  "Total",        "PJ",      "Imobiliário",   "Inadimplência imobiliário PJ",                  21139,
  "Juros",          "Total",        "PF",      "Imobiliário",   "Juros imobiliário PF",                          20774,
  "Juros",          "Total",        "PJ",      "Imobiliário",   "Juros imobiliário PJ",                          20763,
  
  "Saldo",          "Livre",        "PF",      "Consignado",    "Saldo consignado PF",                           20580,
  "Concessões",     "Livre",        "PF",      "Consignado",    "Concessões consignado PF",                      20672,
  "Inadimplência",  "Total",        "PF",      "Consignado",    "Inadimplência consignado PF",                   21119,
  "Juros",          "Total",        "PF",      "Consignado",    "Juros consignado PF",                           20747
)

#----------------------------------------
# Funções auxiliares de formatação
#----------------------------------------

formatar_bilhoes <- function(x) {
  val <- x / 1e9
  paste0(
    "R$ ",
    format(round(val, 1), big.mark = ".", decimal.mark = ","),
    " bi"
  )
}

formatar_pct <- function(x) {
  if (is.na(x)) return("—")
  sinal <- ifelse(x > 0, "+", "")
  paste0(
    sinal,
    format(round(x, 1), big.mark = ".", decimal.mark = ","),
    "%"
  )
}

formatar_mes_ano <- function(d) format(d, "%m/%Y")

#----------------------------------------
# Definição de datas padrão do filtro
#----------------------------------------

if (nrow(dados_bcb) > 0) {
  data_inicio_default <- max(min(dados_bcb$data), as.Date("2015-01-01"))
  data_fim_default    <- max(dados_bcb$data)
} else {
  data_inicio_default <- as.Date("2015-01-01")
  data_fim_default    <- Sys.Date()
}

#----------------------------------------
# UI
#----------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de Crédito - BCB"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão geral", tabName = "visao_geral", icon = icon("tachometer-alt")),
      menuItem("Séries temporais", tabName = "series", icon = icon("chart-line")),
      menuItem("Quebras por tipo de crédito", tabName = "quebras", icon = icon("project-diagram"))
    ),
    dateRangeInput(
      inputId   = "periodo",
      label     = "Período:",
      start     = data_inicio_default,
      end       = data_fim_default,
      format    = "yyyy-mm-dd",
      startview = "year"
    )
  ),
  
  dashboardBody(
    tabItems(
      #-------------------------
      # Aba: Visão geral
      #-------------------------
      tabItem(
        tabName = "visao_geral",
        h3("Saldos (valores mais recentes - 12 meses)"),
        fluidRow(
          valueBoxOutput("kpi_saldo_total", width = 4),
          valueBoxOutput("kpi_saldo_pf",    width = 4),
          valueBoxOutput("kpi_saldo_pj",    width = 4)
        ),
        h3("Concessões (acumulado 12 meses)"),
        fluidRow(
          valueBoxOutput("kpi_concessao_total", width = 4),
          valueBoxOutput("kpi_concessao_pf",    width = 4),
          valueBoxOutput("kpi_concessao_pj",    width = 4)
        ),
        h3("Taxas de Juros (média 12 meses)"),
        fluidRow(
          valueBoxOutput("kpi_juros_total", width = 4),
          valueBoxOutput("kpi_juros_pf",    width = 4),
          valueBoxOutput("kpi_juros_pj",    width = 4)
        ),
        h3("Inadimplência (média 12 meses)"),
        fluidRow(
          valueBoxOutput("kpi_inad_total",  width = 4),
          valueBoxOutput("kpi_inad_pf",     width = 4),
          valueBoxOutput("kpi_inad_pj",     width = 4)
        )
      ),
      
      #-------------------------
      # Aba: Séries temporais
      #-------------------------
      tabItem(
        tabName = "series",
        fluidRow(
          box(
            width = 12,
            title = "Filtros",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            column(
              width = 4,
              selectInput(
                inputId = "ts_grupo",
                label   = "Categoria do indicador:",
                choices = sort(unique(indicadores_meta$grupo)),
                selected = "Saldo"
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "ts_serie",
                label   = "Série:",
                choices = sort(indicadores_meta$nome_curto),
                selected = "Saldo total PF"
              )
            ),
            column(
              width = 4,
              br(),
              downloadButton("download_csv", "Baixar dados (CSV)"),
              downloadButton("download_png", "Baixar gráfico (PNG)")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = textOutput("titulo_grafico_ts"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("plot_ts", height = "350px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Tabela de dados da série selecionada",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("tabela_ts")
          )
        )
      ),
      
      #-------------------------
      # Aba: Quebras por tipo de crédito
      #-------------------------
      tabItem(
        tabName = "quebras",
        fluidRow(
          box(
            width = 12,
            title = "Filtros - Quebras por tipo de crédito",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            column(
              width = 4,
              selectInput(
                "qb_subgrupo",
                "Tipo de crédito:",
                choices = c("Veículos", "Imobiliário", "Consignado"),
                selected = "Veículos"
              )
            ),
            column(
              width = 4,
              selectInput(
                "qb_grupo",
                "Indicador:",
                choices = c("Saldo", "Concessões", "Juros", "Inadimplência"),
                selected = "Saldo"
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = textOutput("titulo_grafico_quebras"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("plot_quebras", height = "350px")
          )
        )
      )
    )
  )
)

#----------------------------------------
# Server
#----------------------------------------

server <- function(input, output, session) {
  
  # Atualiza as opções de série quando muda o grupo (Saldo, Concessões, etc.)
  observeEvent(input$ts_grupo, {
    opcoes <- indicadores_meta %>%
      filter(grupo == input$ts_grupo) %>%
      arrange(subgrupo, segmento, carteira, nome_curto) %>%
      pull(nome_curto)
    
    updateSelectInput(
      session, "ts_serie",
      choices  = opcoes,
      selected = opcoes[1]
    )
  }, ignoreInit = TRUE)
  
  #-------------------------
  # Dados filtrados por período
  #-------------------------
  dados_todos <- reactive({
    validate(
      need(nrow(dados_bcb) > 0, "Nenhum dado disponível. Verifique se data/dados_bcb.rds existe."),
      need(!is.null(input$periodo), "Selecione um período.")
    )
    
    dados_bcb %>%
      filter(
        data >= input$periodo[1],
        data <= input$periodo[2]
      )
  })
  
  #--------------------------------
  # VISÃO GERAL - KPIs (12 MESES)
  #--------------------------------
  
  # Função para KPIs de Saldo (acumulado 12 meses - valor mais recente)
  kpi_saldo_12m <- function(nome_serie, titulo, cor = "blue") {
    renderValueBox({
      df <- dados_todos() %>% filter(series.name == nome_serie)
      
      validate(
        need(nrow(df) > 0,
             paste("Sem dados para", nome_serie, "no período selecionado."))
      )
      
      df <- df %>% arrange(data)
      
      # Pega os últimos 12 meses
      ultimos_12 <- df %>% tail(12)
      
      # Para Saldo, mostramos o valor mais recente
      ultimo <- dplyr::last(df)
      
      valor_txt <- formatar_bilhoes(ultimo$valor)
      data_final <- formatar_mes_ano(ultimo$data)
      
      # Calcula variação nos últimos 12 meses
      if (nrow(ultimos_12) >= 2) {
        valor_inicial <- ultimos_12$valor[1]
        valor_final <- ultimo$valor
        # Verifica divisão por zero
        if (!is.na(valor_inicial) && valor_inicial != 0) {
          var_12m <- ((valor_final / valor_inicial) - 1) * 100
          var_12m_txt <- formatar_pct(var_12m)
        } else {
          var_12m_txt <- "—"
        }
      } else {
        var_12m_txt <- "—"
      }
      
      subtitle_html <- HTML(
        paste0(
          titulo, " (", data_final, ")",
          "<br>Variação 12 meses: ", var_12m_txt
        )
      )
      
      valueBox(
        value    = valor_txt,
        subtitle = subtitle_html,
        icon     = icon("wallet"),
        color    = cor
      )
    })
  }
  
  # Função para KPIs de Concessões (acumulado 12 meses)
  kpi_concessao_12m <- function(nome_serie, titulo, cor = "green") {
    renderValueBox({
      df <- dados_todos() %>% filter(series.name == nome_serie)
      
      validate(
        need(nrow(df) > 0,
             paste("Sem dados para", nome_serie, "no período selecionado."))
      )
      
      df <- df %>% arrange(data)
      
      # Pega os últimos 12 meses e soma
      ultimos_12 <- df %>% tail(12)
      valor_acum_12m <- sum(ultimos_12$valor, na.rm = TRUE)
      
      valor_txt <- formatar_bilhoes(valor_acum_12m)
      
      # Período de referência
      data_inicial <- formatar_mes_ano(ultimos_12$data[1])
      data_final <- formatar_mes_ano(dplyr::last(ultimos_12)$data)
      
      subtitle_html <- HTML(
        paste0(
          titulo,
          "<br>Acumulado 12 meses (", data_inicial, " a ", data_final, ")"
        )
      )
      
      valueBox(
        value    = valor_txt,
        subtitle = subtitle_html,
        icon     = icon("hand-holding-usd"),
        color    = cor
      )
    })
  }
  
  # Função para KPIs de Juros (média 12 meses)
  kpi_juros_12m <- function(nome_serie, titulo, cor = "yellow") {
    renderValueBox({
      df <- dados_todos() %>% filter(series.name == nome_serie)
      
      validate(
        need(nrow(df) > 0,
             paste("Sem dados para", nome_serie, "no período selecionado."))
      )
      
      df <- df %>% arrange(data)
      
      # Pega os últimos 12 meses e calcula média
      ultimos_12 <- df %>% tail(12)
      media_12m <- mean(ultimos_12$valor, na.rm = TRUE)
      
      valor_txt <- formatar_pct(media_12m)
      
      # Período de referência
      data_inicial <- formatar_mes_ano(ultimos_12$data[1])
      data_final <- formatar_mes_ano(dplyr::last(ultimos_12)$data)
      
      subtitle_html <- HTML(
        paste0(
          titulo,
          "<br>Média 12 meses (", data_inicial, " a ", data_final, ")"
        )
      )
      
      valueBox(
        value    = valor_txt,
        subtitle = subtitle_html,
        icon     = icon("percent"),
        color    = cor
      )
    })
  }
  
  # Função para KPIs de Inadimplência (média 12 meses)
  kpi_inadimplencia_12m <- function(nome_serie, titulo, cor = "red") {
    renderValueBox({
      df <- dados_todos() %>% filter(series.name == nome_serie)
      
      validate(
        need(nrow(df) > 0,
             paste("Sem dados para", nome_serie, "no período selecionado."))
      )
      
      df <- df %>% arrange(data)
      
      # Pega os últimos 12 meses e calcula média
      ultimos_12 <- df %>% tail(12)
      media_12m <- mean(ultimos_12$valor, na.rm = TRUE)
      
      valor_txt <- formatar_pct(media_12m)
      
      # Período de referência
      data_inicial <- formatar_mes_ano(ultimos_12$data[1])
      data_final <- formatar_mes_ano(dplyr::last(ultimos_12)$data)
      
      subtitle_html <- HTML(
        paste0(
          titulo,
          "<br>Média 12 meses (", data_inicial, " a ", data_final, ")"
        )
      )
      
      valueBox(
        value    = valor_txt,
        subtitle = subtitle_html,
        icon     = icon("exclamation-triangle"),
        color    = cor
      )
    })
  }
  
  # Saldo (valor mais recente)
  output$kpi_saldo_total <- kpi_saldo_12m("Saldo total",    "Saldo Total",    "blue")
  output$kpi_saldo_pf    <- kpi_saldo_12m("Saldo total PF", "Saldo PF",       "blue")
  output$kpi_saldo_pj    <- kpi_saldo_12m("Saldo total PJ", "Saldo PJ",       "blue")
  
  # Concessões (acumulado 12 meses)
  output$kpi_concessao_total <- kpi_concessao_12m("Concessões totais",    "Concessão Total", "green")
  output$kpi_concessao_pf    <- kpi_concessao_12m("Concessões totais PF", "Concessão PF",    "green")
  output$kpi_concessao_pj    <- kpi_concessao_12m("Concessões totais PJ", "Concessão PJ",    "green")
  
  # Juros (média 12 meses)
  output$kpi_juros_total <- kpi_juros_12m("Juros total",    "Taxa de Juros Média Total", "yellow")
  output$kpi_juros_pf    <- kpi_juros_12m("Juros total PF", "Taxa de Juros Média PF",    "yellow")
  output$kpi_juros_pj    <- kpi_juros_12m("Juros total PJ", "Taxa de Juros Média PJ",    "yellow")
  
  # Inadimplência (média 12 meses)
  output$kpi_inad_total <- kpi_inadimplencia_12m("Inadimplência total", "Inadimplência Média Total", "red")
  output$kpi_inad_pf    <- kpi_inadimplencia_12m("Inadimplência PF",    "Inadimplência Média PF",    "red")
  output$kpi_inad_pj    <- kpi_inadimplencia_12m("Inadimplência PJ",    "Inadimplência Média PJ",    "red")
  
  #--------------------------------
  # SÉRIES TEMPORAIS
  #--------------------------------
  output$titulo_grafico_ts <- renderText({
    df_meta <- indicadores_meta %>%
      filter(nome_curto == input$ts_serie)
    if (nrow(df_meta) == 0) return(input$ts_serie)
    
    paste0(
      df_meta$grupo, " - ",
      df_meta$subgrupo, " - ",
      df_meta$segmento, " - ",
      df_meta$carteira
    )
  })
  
  output$plot_ts <- renderPlot({
    df <- dados_todos() %>%
      filter(series.name == input$ts_serie)
    
    validate(
      need(nrow(df) > 0,
           "Sem dados retornados para essa série e período. Tente ampliar o intervalo.")
    )
    
    df <- df %>% arrange(data)
    
    grupo_serie   <- unique(df$grupo)
    eh_percentual <- grupo_serie %in% c("Juros", "Inadimplência", "Spread")
    
    df <- df %>%
      mutate(
        valor_plot = ifelse(eh_percentual, valor, valor / 1e9)
      )
    
    y_lab <- if (eh_percentual) "% a.a." else "R$ bilhões"
    
    ggplot(df, aes(x = data, y = valor_plot)) +
      geom_line() +
      labs(x = "", y = y_lab) +
      theme_minimal()
  })
  
  output$tabela_ts <- renderDT({
    df <- dados_todos() %>%
      filter(series.name == input$ts_serie) %>%
      arrange(data)
    
    validate(
      need(nrow(df) > 0,
           "Sem dados para exibir na tabela.")
    )
    
    df <- df %>%
      mutate(
        data   = as.Date(data),
        var_mm = round(var_mm, 2),
        var_aa = round(var_aa, 2)
      ) %>%
      select(
        data, series.name, grupo, carteira, segmento, subgrupo,
        valor, var_mm, var_aa
      )
    
    datatable(
      df,
      options = list(
        pageLength = 12,
        scrollX    = TRUE
      )
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("serie_", gsub(" ", "_", input$ts_serie), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- dados_todos() %>%
        filter(series.name == input$ts_serie) %>%
        arrange(data)
      readr::write_csv(df, file)
    }
  )
  
  output$download_png <- downloadHandler(
    filename = function() {
      paste0("serie_", gsub(" ", "_", input$ts_serie), "_", Sys.Date(), ".png")
    },
    content = function(file) {
      df <- dados_todos() %>%
        filter(series.name == input$ts_serie) %>%
        arrange(data)
      
      validate(
        need(nrow(df) > 0,
             "Sem dados para gerar o gráfico.")
      )
      
      grupo_serie   <- unique(df$grupo)
      eh_percentual <- grupo_serie %in% c("Juros", "Inadimplência", "Spread")
      
      df <- df %>%
        mutate(
          valor_plot = ifelse(eh_percentual, valor, valor / 1e9)
        )
      
      y_lab <- if (eh_percentual) "% a.a." else "R$ bilhões"
      
      p <- ggplot(df, aes(x = data, y = valor_plot)) +
        geom_line() +
        labs(
          title = input$ts_serie,
          x = "",
          y = y_lab
        ) +
        theme_minimal()
      
      ggplot2::ggsave(
        filename = file,
        plot     = p,
        device   = "png",
        width    = 8,
        height   = 4,
        dpi      = 150
      )
    }
  )
  
  #--------------------------------
  # QUEBRAS POR TIPO DE CRÉDITO
  #--------------------------------
  output$titulo_grafico_quebras <- renderText({
    paste(
      "Indicador:",
      input$qb_grupo,
      "- Tipo de crédito:",
      input$qb_subgrupo
    )
  })
  
  output$plot_quebras <- renderPlot({
    df <- dados_todos() %>%
      filter(
        grupo    == input$qb_grupo,
        subgrupo == input$qb_subgrupo,
        segmento %in% c("PF", "PJ")
      ) %>%
      arrange(data)
    
    validate(
      need(nrow(df) > 0,
           "Sem dados para essas quebras no período selecionado.")
    )
    
    eh_percentual <- input$qb_grupo %in% c("Juros", "Inadimplência", "Spread")
    
    df <- df %>%
      mutate(
        valor_plot = ifelse(eh_percentual, valor, valor / 1e9)
      )
    
    y_lab <- if (eh_percentual) "% a.a." else "R$ bilhões"
    
    ggplot(df, aes(x = data, y = valor_plot, color = segmento)) +
      geom_line() +
      labs(
        x = "",
        y = y_lab,
        color = "Segmento"
      ) +
      theme_minimal()
  })
}

#----------------------------------------
# Inicializa o app
#----------------------------------------
shinyApp(ui = ui, server = server)
