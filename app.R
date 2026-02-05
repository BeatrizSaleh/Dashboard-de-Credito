# app.R

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(scales)
library(lubridate)

#----------------------------------------
# 0) Carrega dados pré-processados (.rds)
#----------------------------------------
if (file.exists("data/dados_bcb.rds")) {
  dados_bcb <- readRDS("data/dados_bcb.rds")
} else {
  warning("Arquivo data/dados_bcb.rds não encontrado. O app inicializará sem dados.")
  dados_bcb <- tibble()
}

#----------------------------------------
# Metadados dos indicadores (mesma estrutura do update_data.R)
#----------------------------------------
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
# Formatação (pt-BR)
#----------------------------------------
fmt_bi_brl <- function(x, digitos = 2) {
  if (is.na(x)) return("—")
  paste0(
    "R$ ",
    scales::number(x, accuracy = 10^(-digitos), big.mark = ".", decimal.mark = ","),
    " bi"
  )
}

fmt_pct <- function(x, digitos = 1) {
  if (is.na(x)) return("—")
  sinal <- ifelse(x > 0, "+", "")
  paste0(
    sinal,
    scales::number(x, accuracy = 10^(-digitos), big.mark = ".", decimal.mark = ","),
    "%"
  )
}

formatar_mes_ano <- function(d) format(d, "%m/%Y")

#----------------------------------------
# Datas padrão (ancoradas no arquivo)
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
      menuItem("Visão geral (último mês)", tabName = "visao_geral_mes", icon = icon("calendar-check")),
      menuItem("Visão geral (12m)", tabName = "visao_geral_12m", icon = icon("tachometer-alt")),
      menuItem("Séries temporais", tabName = "series", icon = icon("chart-line")),
      menuItem("Comparação", tabName = "comparacao", icon = icon("balance-scale"))
    ),
    dateRangeInput(
      inputId   = "periodo",
      label     = "Período (mês de referência):",
      start     = max(data_fim_default - 365 * 2, data_inicio_default),
      end       = data_fim_default,
      format    = "yyyy-mm-dd",
      startview = "year"
    ),
    selectInput(
      inputId = "x_freq",
      label   = "Frequência de datas no eixo X:",
      choices = c(
        "Mensal"     = "1 month",
        "Bimestral"  = "2 months",
        "Trimestral" = "3 months",
        "Semestral"  = "6 months",
        "Anual"      = "12 months"
      ),
      selected = "3 months"
    ),
    tags$small(style = "display:block; margin-top:6px; color:#666;", textOutput("txt_ultima_atualizacao")),
    tags$small(style = "display:block; margin-top:4px; color:#666;", "Obs.: valores monetários exibidos em R$ bilhões (bi)."),
    actionButton("btn_12m", "Últimos 12 meses"),
    actionButton("btn_24m", "Últimos 24 meses"),
    actionButton("btn_todo", "Desde 2015")
  ),
  
  dashboardBody(
    tabItems(
      #-------------------------
      # Aba: Visão geral (último mês)
      #-------------------------
      tabItem(
        tabName = "visao_geral_mes",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE, title = "Como ler estes números",
            HTML(
              "<b>Referência:</b> último mês disponível no arquivo.<br>",
              "<b>Saldo e Concessões:</b> valor do mês em <b>R$ bilhões (bi)</b>.<br>",
              "<b>Taxa de juros e Inadimplência:</b> valor do mês em <b>%</b>.<br>"
            )
          )
        ),
        fluidRow(
          valueBoxOutput("kpi_saldo_mes_pf",    width = 4),
          valueBoxOutput("kpi_saldo_mes_pj",    width = 4),
          valueBoxOutput("kpi_saldo_mes_total", width = 4)
        ),
        fluidRow(
          valueBoxOutput("kpi_conc_mes_pf",    width = 4),
          valueBoxOutput("kpi_conc_mes_pj",    width = 4),
          valueBoxOutput("kpi_conc_mes_total", width = 4)
        ),
        fluidRow(
          valueBoxOutput("kpi_juros_mes_pf",    width = 4),
          valueBoxOutput("kpi_juros_mes_pj",    width = 4),
          valueBoxOutput("kpi_juros_mes_total", width = 4)
        ),
        fluidRow(
          valueBoxOutput("kpi_inad_mes_pf",    width = 4),
          valueBoxOutput("kpi_inad_mes_pj",    width = 4),
          valueBoxOutput("kpi_inad_mes_total", width = 4)
        )
      ),
      
      #-------------------------
      # Aba: Visão geral (12m) - como era antes (acumulado/média)
      #-------------------------
      tabItem(
        tabName = "visao_geral_12m",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE, title = "Como ler estes números",
            HTML(
              "<b>Janela:</b> últimos 12 meses disponíveis no arquivo (terminando no último mês exibido).<br>",
              "<b>Saldo e Concessões:</b> <u>acumulado</u> em 12 meses (soma mensal) em <b>R$ bilhões (bi)</b>.<br>",
              "<b>Taxa de juros e Inadimplência:</b> <u>média</u> em 12 meses em <b>%</b>.<br>"
            )
          )
        ),
        fluidRow(
          valueBoxOutput("kpi_saldo12_pf",    width = 4),
          valueBoxOutput("kpi_saldo12_pj",    width = 4),
          valueBoxOutput("kpi_saldo12_total", width = 4)
        ),
        fluidRow(
          valueBoxOutput("kpi_conc12_pf",    width = 4),
          valueBoxOutput("kpi_conc12_pj",    width = 4),
          valueBoxOutput("kpi_conc12_total", width = 4)
        ),
        fluidRow(
          valueBoxOutput("kpi_juros12_pf",    width = 4),
          valueBoxOutput("kpi_juros12_pj",    width = 4),
          valueBoxOutput("kpi_juros12_total", width = 4)
        ),
        fluidRow(
          valueBoxOutput("kpi_inad12_pf",    width = 4),
          valueBoxOutput("kpi_inad12_pj",    width = 4),
          valueBoxOutput("kpi_inad12_total", width = 4)
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
        column(
          width = 4,
          selectInput(
            inputId = "ts_tipo",
            label   = "Tipo de visualização:",
            choices = c(
              "Nível (R$ bilhões)"               = "nivel",
              "Desvio desde o início (Δ R$ bi)"  = "desvio",
              "Índice (base 100)"                = "indice",
              "Variação m/m (%)"                 = "mm",
              "Variação a/a (%)"                 = "aa"
            ),
            selected = "nivel"
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
      # Aba: Comparação
      #-------------------------
      tabItem(
        tabName = "comparacao",
        fluidRow(
          box(
            width = 12,
            title = "Selecione séries para comparar (mesmo tipo de unidade)",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            column(
              width = 6,
              selectizeInput(
                inputId = "cmp_series",
                label   = "Séries (pode selecionar várias):",
                choices = indicadores_meta %>%
                  arrange(grupo, subgrupo, segmento, carteira, nome_curto) %>%
                  pull(nome_curto),
                selected = c("Saldo total PF", "Saldo total PJ", "Saldo total"),
                multiple = TRUE,
                options  = list(placeholder = "Digite para buscar...")
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = "cmp_tipo",
                label   = "Tipo de visualização:",
                choices = c(
                  "Nível (R$ bilhões ou %)"        = "nivel",
                  "Índice (base 100)"              = "indice",
                  "Variação m/m (%)"               = "mm",
                  "Variação a/a (%)"               = "aa"
                ),
                selected = "nivel"
              )
            ),
            column(
              width = 3,
              checkboxInput(
                inputId = "cmp_legenda",
                label = "Mostrar legenda",
                value = TRUE
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = textOutput("titulo_grafico_cmp"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("plot_cmp", height = "420px")
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
  
  output$txt_ultima_atualizacao <- renderText({
    if (nrow(dados_bcb) == 0) return("Última atualização: —")
    paste0("Último mês no arquivo: ", format(max(dados_bcb$data), "%m/%Y"))
  })
  
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
  
  # Botões de período (ancorados no último mês do arquivo)
  observeEvent(input$btn_12m, {
    fim    <- if (nrow(dados_bcb) > 0) max(dados_bcb$data) else Sys.Date()
    inicio <- fim - 365
    updateDateRangeInput(session, "periodo", start = inicio, end = fim)
  })
  
  observeEvent(input$btn_24m, {
    fim    <- if (nrow(dados_bcb) > 0) max(dados_bcb$data) else Sys.Date()
    inicio <- fim - 365 * 2
    updateDateRangeInput(session, "periodo", start = inicio, end = fim)
  })
  
  observeEvent(input$btn_todo, {
    fim    <- if (nrow(dados_bcb) > 0) max(dados_bcb$data) else Sys.Date()
    inicio <- as.Date("2015-01-01")
    updateDateRangeInput(session, "periodo", start = inicio, end = fim)
  })
  
  #-------------------------
  # Dados filtrados por período (global)
  #-------------------------
  dados_todos <- reactive({
    validate(
      need(nrow(dados_bcb) > 0, "Nenhum dado disponível. Verifique se data/dados_bcb.rds existe."),
      need(!is.null(input$periodo), "Selecione um período.")
    )
    
    dados_bcb %>%
      filter(data >= input$periodo[1], data <= input$periodo[2])
  })
  
  #-------------------------
  # Helper: breaks do eixo X (garante incluir o último mês disponível)
  #-------------------------
  gerar_breaks_x <- function(datas, by = "3 months") {
    datas <- datas[!is.na(datas)]
    if (length(datas) == 0) return(NULL)
    
    dmin <- lubridate::floor_date(min(datas), "month")
    dmax <- max(datas)
    
    brks <- seq.Date(from = dmin, to = lubridate::ceiling_date(dmax, "month"), by = by)
    brks <- sort(unique(c(brks, dmax))) # garante o último mês como tick
    brks
  }
  
  add_x_scale <- function(p, df) {
    p + scale_x_date(
      limits = c(min(df$data, na.rm = TRUE), max(df$data, na.rm = TRUE)),
      breaks = gerar_breaks_x(df$data, by = input$x_freq),
      date_labels = "%m/%Y",
      expand = expansion(mult = c(0.01, 0.01))
    )
  }
  
  #-------------------------
  # Último mês (sempre o último mês do arquivo)
  #-------------------------
  dados_ultimo_mes <- reactive({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    fim <- max(dados_bcb$data)
    dados_bcb %>% filter(data == fim)
  })
  
  txt_ref_mes <- reactive({
    if (nrow(dados_bcb) == 0) return("—")
    fim <- max(dados_bcb$data)
    formatar_mes_ano(fim)
  })
  
  #-------------------------
  # Janela "últimos 12 meses" (sempre a partir do último mês do arquivo)
  #-------------------------
  dados_12m <- reactive({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    
    fim <- max(dados_bcb$data)
    inicio <- fim %m-% months(11) # 12 pontos mensais
    
    dados_bcb %>%
      filter(data >= inicio, data <= fim)
  })
  
  txt_janela_12m <- reactive({
    if (nrow(dados_bcb) == 0) return("—")
    fim <- max(dados_bcb$data)
    inicio <- fim %m-% months(11)
    paste0(formatar_mes_ano(inicio), " a ", formatar_mes_ano(fim))
  })
  
  #--------------------------------
  # KPIs - ÚLTIMO MÊS
  #--------------------------------
  kpi_last_monetario <- function(nome_serie, titulo, cor = "blue", icon_name = "") {
    renderValueBox({
      df <- dados_ultimo_mes() %>% filter(series.name == nome_serie) %>% arrange(data)
      validate(need(nrow(df) > 0, paste("Sem dados para", nome_serie)))
      
      valor_bi <- df$valor[1] / 1000
      ref_txt  <- txt_ref_mes()
      
      valueBox(
        value    = fmt_bi_brl(valor_bi),
        subtitle = HTML(paste0(titulo, "<br><span style='color:#0000000;'>Referência: ", ref_txt, " • R$ bi</span>")),
        icon     = icon(icon_name),
        color    = cor
      )
    })
  }
  
  kpi_last_pct <- function(nome_serie, titulo, cor = "red", icon_name = "") {
    renderValueBox({
      df <- dados_ultimo_mes() %>% filter(series.name == nome_serie) %>% arrange(data)
      validate(need(nrow(df) > 0, paste("Sem dados para", nome_serie)))
      
      valor <- df$valor[1]
      ref_txt <- txt_ref_mes()
      
      valueBox(
        value    = fmt_pct(valor),
        subtitle = HTML(paste0(titulo, "<br><span style='color:#0000000;'>Referência: ", ref_txt, " • %</span>")),
        icon     = icon(icon_name),
        color    = cor
      )
    })
  }
  
  # Saldo (último mês) — PF/PJ/Total
  output$kpi_saldo_mes_pf    <- kpi_last_monetario("Saldo total PF", "Saldo PF", "blue")
  output$kpi_saldo_mes_pj    <- kpi_last_monetario("Saldo total PJ", "Saldo PJ", "blue")
  output$kpi_saldo_mes_total <- kpi_last_monetario("Saldo total",    "Saldo Total", "blue")
  
  # Concessões (último mês) — PF/PJ/Total
  output$kpi_conc_mes_pf    <- kpi_last_monetario("Concessões totais PF", "Concessões PF", "aqua")
  output$kpi_conc_mes_pj    <- kpi_last_monetario("Concessões totais PJ", "Concessões PJ", "aqua")
  output$kpi_conc_mes_total <- kpi_last_monetario("Concessões totais",    "Concessões Total", "aqua")
  
  # Juros (último mês) — PF/PJ/Total
  output$kpi_juros_mes_pf    <- kpi_last_pct("Juros total PF", "Taxa de juros PF", "yellow")
  output$kpi_juros_mes_pj    <- kpi_last_pct("Juros total PJ", "Taxa de juros PJ", "yellow")
  output$kpi_juros_mes_total <- kpi_last_pct("Juros total",    "Taxa de juros Total", "yellow")
  
  # Inadimplência (último mês) — PF/PJ/Total
  output$kpi_inad_mes_pf    <- kpi_last_pct("Inadimplência PF",    "Inadimplência PF", "red")
  output$kpi_inad_mes_pj    <- kpi_last_pct("Inadimplência PJ",    "Inadimplência PJ", "red")
  output$kpi_inad_mes_total <- kpi_last_pct("Inadimplência total", "Inadimplência Total", "red")
  
  #--------------------------------
  # KPIs - 12 MESES (acumulado/média)
  #--------------------------------
  kpi_sum_12m_monetario <- function(nome_serie, titulo, cor = "blue", icon_name = "") {
    renderValueBox({
      df <- dados_12m() %>% filter(series.name == nome_serie) %>% arrange(data)
      validate(need(nrow(df) > 0, paste("Sem dados para", nome_serie)))
      
      valor_12m_bi <- sum(df$valor, na.rm = TRUE) / 1000
      janela_txt <- txt_janela_12m()
      
      valueBox(
        value    = fmt_bi_brl(valor_12m_bi),
        subtitle = HTML(paste0(titulo, "<br><span style='color:#0000000;'>Acumulado 12m (", janela_txt, ") • R$ bi</span>")),
        icon     = icon(icon_name),
        color    = cor
      )
    })
  }
  
  kpi_mean_12m_pct <- function(nome_serie, titulo, cor = "red", icon_name = "") {
    renderValueBox({
      df <- dados_12m() %>% filter(series.name == nome_serie) %>% arrange(data)
      validate(need(nrow(df) > 0, paste("Sem dados para", nome_serie)))
      
      valor_12m <- mean(df$valor, na.rm = TRUE)
      janela_txt <- txt_janela_12m()
      
      valueBox(
        value    = fmt_pct(valor_12m),
        subtitle = HTML(paste0(titulo, "<br><span style='color:#0000000;'>Média 12m (", janela_txt, ") • %</span>")),
        icon     = icon(icon_name),
        color    = cor
      )
    })
  }
  
  # Saldo (acumulado 12m) — PF/PJ/Total
  output$kpi_saldo12_pf    <- kpi_sum_12m_monetario("Saldo total PF", "Saldo PF", "blue")
  output$kpi_saldo12_pj    <- kpi_sum_12m_monetario("Saldo total PJ", "Saldo PJ", "blue")
  output$kpi_saldo12_total <- kpi_sum_12m_monetario("Saldo total",    "Saldo Total", "blue")
  
  # Concessões (acumulado 12m) — PF/PJ/Total
  output$kpi_conc12_pf    <- kpi_sum_12m_monetario("Concessões totais PF", "Concessões PF", "aqua")
  output$kpi_conc12_pj    <- kpi_sum_12m_monetario("Concessões totais PJ", "Concessões PJ", "aqua")
  output$kpi_conc12_total <- kpi_sum_12m_monetario("Concessões totais",    "Concessões Total", "aqua")
  
  # Juros (média 12m) — PF/PJ/Total
  output$kpi_juros12_pf    <- kpi_mean_12m_pct("Juros total PF", "Taxa de juros PF", "yellow")
  output$kpi_juros12_pj    <- kpi_mean_12m_pct("Juros total PJ", "Taxa de juros PJ", "yellow")
  output$kpi_juros12_total <- kpi_mean_12m_pct("Juros total",    "Taxa de juros Total", "yellow")
  
  # Inadimplência (média 12m) — PF/PJ/Total
  output$kpi_inad12_pf    <- kpi_mean_12m_pct("Inadimplência PF",    "Inadimplência PF", "red")
  output$kpi_inad12_pj    <- kpi_mean_12m_pct("Inadimplência PJ",    "Inadimplência PJ", "red")
  output$kpi_inad12_total <- kpi_mean_12m_pct("Inadimplência total", "Inadimplência Total", "red")
  
  #--------------------------------
  # SÉRIES TEMPORAIS
  #--------------------------------
  output$titulo_grafico_ts <- renderText({
    df_meta <- indicadores_meta %>% filter(nome_curto == input$ts_serie)
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
      filter(series.name == input$ts_serie) %>%
      arrange(data)
    
    validate(need(nrow(df) > 0, "Sem dados retornados para essa série e período. Tente ampliar o intervalo."))
    
    grupo_serie   <- unique(df$grupo)
    eh_percentual <- grupo_serie %in% c("Juros", "Inadimplência", "Spread")
    tipo <- if (is.null(input$ts_tipo)) "nivel" else input$ts_tipo
    
    if (tipo == "nivel") {
      if (eh_percentual) {
        df <- df %>% mutate(valor_plot = valor)
        y_lab <- "% a.a."
      } else {
        df <- df %>% mutate(valor_plot = valor / 1000)
        y_lab <- "R$ bilhões (bi)"
      }
      
      y_min <- min(df$valor_plot, na.rm = TRUE)
      y_max <- max(df$valor_plot, na.rm = TRUE)
      if (y_min == y_max) {
        eps <- if (y_min == 0) 0.1 else abs(y_min) * 0.01
        y_min <- y_min - eps
        y_max <- y_max + eps
      }
      
      p <- ggplot(df, aes(x = data, y = valor_plot)) +
        geom_line(size = 1.1) +
        labs(x = "", y = y_lab) +
        scale_y_continuous(limits = c(y_min, y_max)) +
        theme_minimal()
      
      add_x_scale(p, df)
      
    } else if (tipo == "desvio") {
      df <- df %>%
        mutate(
          nivel_base = ifelse(eh_percentual, valor, valor / 1000),
          valor_base = dplyr::first(nivel_base),
          valor_plot = nivel_base - valor_base
        )
      
      y_lab <- if (eh_percentual) "Desvio em p.p. desde o início" else "Δ R$ bilhões (bi) desde o início"
      
      p <- ggplot(df, aes(x = data, y = valor_plot)) +
        geom_line(size = 1.1) +
        labs(x = "", y = y_lab) +
        theme_minimal()
      
      add_x_scale(p, df)
      
    } else if (tipo == "indice") {
      df <- df %>% mutate(valor_base = dplyr::first(valor), valor_plot = (valor / valor_base) * 100)
      
      p <- ggplot(df, aes(x = data, y = valor_plot)) +
        geom_line(size = 1.1) +
        labs(x = "", y = "Índice (base 100)") +
        theme_minimal()
      
      add_x_scale(p, df)
      
    } else if (tipo == "mm") {
      df <- df %>% mutate(valor_plot = var_mm)
      
      p <- ggplot(df, aes(x = data, y = valor_plot)) +
        geom_line(size = 1.1) +
        labs(x = "", y = "Variação m/m (%)") +
        theme_minimal()
      
      add_x_scale(p, df)
      
    } else if (tipo == "aa") {
      df <- df %>% mutate(valor_plot = var_aa)
      
      p <- ggplot(df, aes(x = data, y = valor_plot)) +
        geom_line(size = 1.1) +
        labs(x = "", y = "Variação a/a (%)") +
        theme_minimal()
      
      add_x_scale(p, df)
    }
  })
  
  output$tabela_ts <- renderDT({
    df <- dados_todos() %>%
      filter(series.name == input$ts_serie) %>%
      arrange(data)
    
    validate(need(nrow(df) > 0, "Sem dados para exibir na tabela."))
    
    df <- df %>%
      mutate(
        data = as.Date(data),
        eh_pct = grupo %in% c("Juros", "Inadimplência", "Spread"),
        valor_exib = ifelse(eh_pct, valor, valor / 1000),
        unidade_valor = ifelse(eh_pct, "%", "R$ bi"),
        var_mm = round(var_mm, 2),
        var_aa = round(var_aa, 2)
      ) %>%
      select(data, series.name, grupo, carteira, segmento, subgrupo, valor_exib, unidade_valor, var_mm, var_aa)
    
    datatable(
      df,
      options = list(
        pageLength = 12,
        scrollX    = TRUE,
        language   = list(decimal = ",", thousands = ".")
      )
    ) %>%
      DT::formatRound(columns = c("valor_exib"), digits = 3, dec.mark = ",", mark = ".") %>%
      DT::formatRound(columns = c("var_mm", "var_aa"), digits = 2, dec.mark = ",", mark = ".")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("serie_", gsub(" ", "_", input$ts_serie), "_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- dados_todos() %>% filter(series.name == input$ts_serie) %>% arrange(data)
      readr::write_csv(df, file)
    }
  )
  
  output$download_png <- downloadHandler(
    filename = function() paste0("serie_", gsub(" ", "_", input$ts_serie), "_", Sys.Date(), ".png"),
    content = function(file) {
      df <- dados_todos() %>% filter(series.name == input$ts_serie) %>% arrange(data)
      validate(need(nrow(df) > 0, "Sem dados para gerar o gráfico."))
      
      grupo_serie   <- unique(df$grupo)
      eh_percentual <- grupo_serie %in% c("Juros", "Inadimplência", "Spread")
      
      if (eh_percentual) {
        df <- df %>% mutate(valor_plot = valor)
        y_lab <- "% a.a."
      } else {
        df <- df %>% mutate(valor_plot = valor / 1000)
        y_lab <- "R$ bilhões (bi)"
      }
      
      p <- ggplot(df, aes(x = data, y = valor_plot)) +
        geom_line() +
        labs(title = input$ts_serie, x = "", y = y_lab) +
        scale_x_date(
          limits = c(min(df$data, na.rm = TRUE), max(df$data, na.rm = TRUE)),
          breaks = gerar_breaks_x(df$data, by = input$x_freq),
          date_labels = "%m/%Y",
          expand = expansion(mult = c(0.01, 0.01))
        ) +
        theme_minimal()
      
      ggplot2::ggsave(filename = file, plot = p, device = "png", width = 8, height = 4, dpi = 150)
    }
  )
  
  #--------------------------------
  # COMPARAÇÃO (múltiplas séries)
  #--------------------------------
  output$titulo_grafico_cmp <- renderText({
    req(input$cmp_series)
    paste0("Comparação: ", length(input$cmp_series), " série(s) • tipo: ", input$cmp_tipo)
  })
  
  output$plot_cmp <- renderPlot({
    req(input$cmp_series)
    
    df <- dados_todos() %>%
      filter(series.name %in% input$cmp_series) %>%
      arrange(series.name, data)
    
    validate(need(nrow(df) > 0, "Sem dados para as séries selecionadas no período."))
    
    # Regra simples: não misturar unidades (percentual vs monetário) no mesmo gráfico em "nível"
    grupos_sel <- df %>% distinct(series.name, grupo)
    eh_pct_por_serie <- grupos_sel %>%
      mutate(eh_pct = grupo %in% c("Juros", "Inadimplência", "Spread")) %>%
      distinct(series.name, eh_pct)
    
    if (input$cmp_tipo == "nivel") {
      validate(
        need(length(unique(eh_pct_por_serie$eh_pct)) == 1,
             "Você selecionou séries com unidades diferentes (R$ e %). Para comparar em 'Nível', escolha séries do mesmo tipo (todas monetárias ou todas percentuais), ou use 'Índice' / 'Variações'.")
      )
    }
    
    if (input$cmp_tipo == "nivel") {
      eh_percentual <- unique(eh_pct_por_serie$eh_pct)
      if (eh_percentual) {
        df <- df %>% mutate(valor_plot = valor)
        y_lab <- "% a.a."
      } else {
        df <- df %>% mutate(valor_plot = valor / 1000)
        y_lab <- "R$ bilhões (bi)"
      }
      
    } else if (input$cmp_tipo == "indice") {
      df <- df %>%
        group_by(series.name) %>%
        mutate(valor_base = first(valor), valor_plot = (valor / valor_base) * 100) %>%
        ungroup()
      y_lab <- "Índice (base 100)"
      
    } else if (input$cmp_tipo == "mm") {
      df <- df %>% mutate(valor_plot = var_mm)
      y_lab <- "Variação m/m (%)"
      
    } else if (input$cmp_tipo == "aa") {
      df <- df %>% mutate(valor_plot = var_aa)
      y_lab <- "Variação a/a (%)"
    }
    
    p <- ggplot(df, aes(x = data, y = valor_plot, color = series.name)) +
      geom_line(size = 1.5) +
      labs(x = "", y = y_lab, color = "Série") +
      theme_minimal() +
      theme(
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
      )
    
    p <- add_x_scale(p, df)
    
    if (!isTRUE(input$cmp_legenda)) {
      p <- p + theme(legend.position = "none")
    }
    
    p
  })

}

#----------------------------------------
# Inicializa o app
#----------------------------------------
shinyApp(ui = ui, server = server)
