# app.R — Dashboard Crédito (BCB/SGS)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(lubridate)
library(plotly)

# ----------------------------------------
# 0) Carrega dados pré-processados (.rds)
# ----------------------------------------
if (file.exists("data/dados_bcb.rds")) {
  dados_bcb <- readRDS("data/dados_bcb.rds")
} else {
  warning("Arquivo data/dados_bcb.rds não encontrado. O app inicializará sem dados.")
  dados_bcb <- tibble()
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ----------------------------------------
# 1) Metadados
# ----------------------------------------
indicadores_meta <- tibble::tribble(
  ~grupo,          ~carteira,      ~segmento, ~nivel1,         ~nivel2,               ~nome_curto,                                                     ~id_sgs,
  
  # ----------------------------
  # LIVRE - PF
  # ----------------------------
  "Saldo",         "Livre",       "PF",      "Total",         NA,                    "Saldo | Livre | PF | Total",                                     20570,
  "Saldo",         "Livre",       "PF",      "Veículos",      NA,                    "Saldo | Livre | PF | Veículos",                                  20581,
  "Saldo",         "Livre",       "PF",      "Consignado",    "Total",               "Saldo | Livre | PF | Consignado | Total",                        20579,
  "Saldo",         "Livre",       "PF",      "Consignado",    "Servidor público",    "Saldo | Livre | PF | Consignado | Servidor público",             20577,
  "Saldo",         "Livre",       "PF",      "Consignado",    "Trabalhador Privado", "Saldo | Livre | PF | Consignado | Trabalhador Privado",          20576,
  "Saldo",         "Livre",       "PF",      "Consignado",    "INSS",                "Saldo | Livre | PF | Consignado | INSS",                         20578,
  "Saldo",         "Livre",       "PF",      "Não consignado","Total",               "Saldo | Livre | PF | Não consignado | Total",                    20574,
  
  "Saldo",         "Livre",       "PF",      "Cartão",        "Total",               "Saldo | Livre | PF | Cartão | Total",                             20590,
  "Saldo",         "Livre",       "PF",      "Cartão",        "Rotativo",            "Saldo | Livre | PF | Cartão | Rotativo",                          20587,
  "Saldo",         "Livre",       "PF",      "Cartão",        "Parcelado",           "Saldo | Livre | PF | Cartão | Parcelado",                         20588,
  "Saldo",         "Livre",       "PF",      "Cartão",        "À vista",             "Saldo | Livre | PF | Cartão | À vista",                           20589,
  
  # Concessões
  "Concessões",    "Livre",       "PF",      "Total",         NA,                    "Concessões | Livre | PF | Total",                                 20662,
  "Concessões",    "Livre",       "PF",      "Veículos",      NA,                    "Concessões | Livre | PF | Veículos",                              20673,
  "Concessões",    "Livre",       "PF",      "Consignado",    "Total",               "Concessões | Livre | PF | Consignado | Total",                    20671,
  "Concessões",    "Livre",       "PF",      "Consignado",    "Servidor público",    "Concessões | Livre | PF | Consignado | Servidor público",         20669,
  "Concessões",    "Livre",       "PF",      "Consignado",    "Privado",             "Concessões | Livre | PF | Consignado | Privado",                  20668,
  "Concessões",    "Livre",       "PF",      "Consignado",    "INSS",                "Concessões | Livre | PF | Consignado | INSS",                     20670,
  "Concessões",    "Livre",       "PF",      "Não consignado","Total",               "Concessões | Livre | PF | Não consignado | Total",                20666,
  
  "Concessões",    "Livre",       "PF",      "Cartão",        "Total",               "Concessões | Livre | PF | Cartão | Total",                        20682,
  "Concessões",    "Livre",       "PF",      "Cartão",        "Rotativo",            "Concessões | Livre | PF | Cartão | Rotativo",                     20679,
  "Concessões",    "Livre",       "PF",      "Cartão",        "Parcelado",           "Concessões | Livre | PF | Cartão | Parcelado",                    20680,
  "Concessões",    "Livre",       "PF",      "Cartão",        "À vista",             "Concessões | Livre | PF | Cartão | À vista",                      20681,
  
  # Juros - Livre PF
  "Juros",         "Livre",       "PF",      "Total",         NA,                    "Juros | Livre | PF | Total",                                      20740,
  "Juros",         "Livre",       "PF",      "Veículos",      NA,                    "Juros | Livre | PF | Veículos",                                   20742,
  "Juros",         "Livre",       "PF",      "Consignado",    "Total",               "Juros | Livre | PF | Consignado | Total",                         20748,
  "Juros",         "Livre",       "PF",      "Consignado",    "Servidor público",    "Juros | Livre | PF | Consignado | Servidor público",              20746,
  "Juros",         "Livre",       "PF",      "Consignado",    "Trabalhador Privado", "Juros | Livre | PF | Consignado | Trabalhador Privado",           20745,
  "Juros",         "Livre",       "PF",      "Consignado",    "INSS",                "Juros | Livre | PF | Consignado | INSS",                          20747,
  "Juros",         "Livre",       "PF",      "Não consignado","Total",               "Juros | Livre | PF | Não consignado | Total",                     20743,
  "Juros",         "Livre",       "PF",      "Cartão",        "Total",               "Juros | Livre | PF | Cartão | Total",                             20754,
  "Juros",         "Livre",       "PF",      "Cartão",        "Rotativo",            "Juros | Livre | PF | Cartão | Rotativo",                          20751,
  "Juros",         "Livre",       "PF",      "Cartão",        "Parcelado",           "Juros | Livre | PF | Cartão | Parcelado",                         20752,
  
  # Inadimplência - Livre PF
  "Inadimplência", "Livre",       "PF",      "Total",         NA,                    "Inadimplência | Livre | PF | Total",                              21117,
  "Inadimplência", "Livre",       "PF",      "Veículos",      NA,                    "Inadimplência | Livre | PF | Veículos",                           21118,
  "Inadimplência", "Livre",       "PF",      "Consignado",    "Total",               "Inadimplência | Livre | PF | Consignado | Total",                 21119,
  "Inadimplência", "Livre",       "PF",      "Não consignado","Total",               "Inadimplência | Livre | PF | Não consignado | Total",             21120,
  "Inadimplência", "Livre",       "PF",      "Cartão",        "Total",               "Inadimplência | Livre | PF | Cartão | Total",                     21124,
  "Inadimplência", "Livre",       "PF",      "Cartão",        "Rotativo",            "Inadimplência | Livre | PF | Cartão | Rotativo",                  21122,
  "Inadimplência", "Livre",       "PF",      "Cartão",        "Parcelado",           "Inadimplência | Livre | PF | Cartão | Parcelado",                 21123,
  
  # ----------------------------
  # LIVRE - PJ
  # ----------------------------
  "Saldo",         "Livre",       "PJ",      "Total",         NA,                    "Saldo | Livre | PJ | Total",                                      20543,
  "Saldo",         "Livre",       "PJ",      "Veículos",      NA,                    "Saldo | Livre | PJ | Veículos",                                   20553,
  "Concessões",    "Livre",       "PJ",      "Total",         NA,                    "Concessões | Livre | PJ | Total",                                 20635,
  "Concessões",    "Livre",       "PJ",      "Veículos",      NA,                    "Concessões | Livre | PJ | Veículos",                              20645,
  
  # Juros - Livre PJ
  "Juros",         "Livre",       "PJ",      "Total",         NA,                    "Juros | Livre | PJ | Total",                                      20718,
  "Juros",         "Livre",       "PJ",      "Veículos",      NA,                    "Juros | Livre | PJ | Veículos",                                   20728,
  "Juros",         "Livre",       "PJ",      "Capital de giro","Total",              "Juros | Livre | PJ | Capital de giro | Total",                    20719,
  "Juros",         "Livre",       "PJ",      "Capital de giro","Até 365 dias",       "Juros | Livre | PJ | Capital de giro | Até 365 dias",             20720,
  "Juros",         "Livre",       "PJ",      "Capital de giro","Acima de 365 dias",  "Juros | Livre | PJ | Capital de giro | Acima de 365 dias",        20721,
  
  # Inadimplência - Livre PJ
  "Inadimplência", "Livre",       "PJ",      "Total",         NA,                    "Inadimplência | Livre | PJ | Total",                              21094,
  "Inadimplência", "Livre",       "PJ",      "Veículos",      NA,                    "Inadimplência | Livre | PJ | Veículos",                           21096,
  "Inadimplência", "Livre",       "PJ",      "Capital de giro","Total",              "Inadimplência | Livre | PJ | Capital de giro | Total",             21095,
  
  # ----------------------------
  # DIRECIONADO - PF
  # ----------------------------
  "Saldo",         "Direcionado", "PF",      "Total",         NA,                    "Saldo | Direcionado | PF | Total",                                20606,
  "Saldo",         "Direcionado", "PF",      "Imobiliário",   NA,                    "Saldo | Direcionado | PF | Imobiliário",                          20612,
  "Saldo",         "Direcionado", "PF",      "Rural",         NA,                    "Saldo | Direcionado | PF | Rural",                                20609,
  "Concessões",    "Direcionado", "PF",      "Total",         NA,                    "Concessões | Direcionado | PF | Total",                           20698,
  "Concessões",    "Direcionado", "PF",      "Imobiliário",   NA,                    "Concessões | Direcionado | PF | Imobiliário",                     20704,
  "Concessões",    "Direcionado", "PF",      "Rural",         NA,                    "Concessões | Direcionado | PF | Rural",                           20701,
  
  # Juros - Direcionado PF
  "Juros",         "Direcionado", "PF",      "Total",         NA,                    "Juros | Direcionado | PF | Total",                                20768,
  "Juros",         "Direcionado", "PF",      "Imobiliário",   NA,                    "Juros | Direcionado | PF | Imobiliário",                          20774,
  "Juros",         "Direcionado", "PF",      "Rural",         NA,                    "Juros | Direcionado | PF | Rural",                                20771,
  
  # Inadimplência - Direcionado PF
  "Inadimplência", "Direcionado", "PF",      "Total",         NA,                    "Inadimplência | Direcionado | PF | Total",                        21149,
  "Inadimplência", "Direcionado", "PF",      "Imobiliário",   NA,                    "Inadimplência | Direcionado | PF | Imobiliário",                  21151,
  "Inadimplência", "Direcionado", "PF",      "Rural",         NA,                    "Inadimplência | Direcionado | PF | Rural",                        21150,
  
  # ----------------------------
  # DIRECIONADO - PJ
  # ----------------------------
  "Saldo",         "Direcionado", "PJ",      "Total",         NA,                    "Saldo | Direcionado | PJ | Total",                                20594,
  "Saldo",         "Direcionado", "PJ",      "Imobiliário",   NA,                    "Saldo | Direcionado | PJ | Imobiliário",                          20600,
  "Saldo",         "Direcionado", "PJ",      "Rural",         NA,                    "Saldo | Direcionado | PJ | Rural",                                20597,
  "Concessões",    "Direcionado", "PJ",      "Total",         NA,                    "Concessões | Direcionado | PJ | Total",                           20686,
  "Concessões",    "Direcionado", "PJ",      "Imobiliário",   NA,                    "Concessões | Direcionado | PJ | Imobiliário",                     20692,
  "Concessões",    "Direcionado", "PJ",      "Rural",         NA,                    "Concessões | Direcionado | PJ | Rural",                           20689,
  
  # Juros - Direcionado PJ
  "Juros",         "Direcionado", "PJ",      "Total",         NA,                    "Juros | Direcionado | PJ | Total",                                20757,
  "Juros",         "Direcionado", "PJ",      "Imobiliário",   NA,                    "Juros | Direcionado | PJ | Imobiliário",                          20763,
  "Juros",         "Direcionado", "PJ",      "Rural",         NA,                    "Juros | Direcionado | PJ | Rural",                                20760,
  
  # Inadimplência - Direcionado PJ
  "Inadimplência", "Direcionado", "PJ",      "Total",         NA,                    "Inadimplência | Direcionado | PJ | Total",                        21137,
  "Inadimplência", "Direcionado", "PJ",      "Imobiliário",   NA,                    "Inadimplência | Direcionado | PJ | Imobiliário",                  21139,
  "Inadimplência", "Direcionado", "PJ",      "Rural",         NA,                    "Inadimplência | Direcionado | PJ | Rural",                        21138
) %>%
  mutate(
    nivel2 = ifelse(is.na(nivel2), "", nivel2),
    chave_ui = paste(grupo, carteira, segmento, nivel1, nivel2, sep = " | ")
  )

adicionais_total <- tibble::tribble(
  ~grupo,          ~carteira,      ~segmento, ~nivel1,  ~nivel2, ~nome_curto,                                  ~id_sgs,
  # total por carteira
  "Saldo",         "Livre",        "Total",   "Total",  "",      "Saldo | Livre | Total | Total",               20542,
  "Concessões",    "Livre",        "Total",   "Total",  "",      "Concessões | Livre | Total | Total",          20634,
  "Juros",         "Livre",        "Total",   "Total",  "",      "Juros | Livre | Total | Total",               20717,
  "Inadimplência", "Livre",        "Total",   "Total",  "",      "Inadimplência | Livre | Total | Total",       21085,
  
  "Saldo",         "Direcionado",  "Total",   "Total",  "",      "Saldo | Direcionado | Total | Total",         20593,
  "Concessões",    "Direcionado",  "Total",   "Total",  "",      "Concessões | Direcionado | Total | Total",    20685,
  "Juros",         "Direcionado",  "Total",   "Total",  "",      "Juros | Direcionado | Total | Total",         20756,
  "Inadimplência", "Direcionado",  "Total",   "Total",  "",      "Inadimplência | Direcionado | Total | Total", 21136,
  
  "Saldo",         "Total",        "Total",   "Total",  "",      "Saldo | Total | Total | Total",               20539,
  "Concessões",    "Total",        "Total",   "Total",  "",      "Concessões | Total | Total | Total",          20631,
  "Juros",         "Total",        "Total",   "Total",  "",      "Juros | Total | Total | Total",               20714,
  "Inadimplência", "Total",        "Total",   "Total",  "",      "Inadimplência | Total | Total | Total",       21082,
  
  # total por segmento (PF/PJ) — derivadas (NA)
  "Saldo",         "Total",        "PF",      "Total",  "",      "Saldo | Total | PF | Total",                  NA,
  "Saldo",         "Total",        "PJ",      "Total",  "",      "Saldo | Total | PJ | Total",                  NA,
  "Concessões",    "Total",        "PF",      "Total",  "",      "Concessões | Total | PF | Total",             NA,
  "Concessões",    "Total",        "PJ",      "Total",  "",      "Concessões | Total | PJ | Total",             NA,
  "Juros",         "Total",        "PF",      "Total",  "",      "Juros | Total | PF | Total",                  NA,
  "Juros",         "Total",        "PJ",      "Total",  "",      "Juros | Total | PJ | Total",                  NA,
  
  # inadimplência PF/PJ
  "Inadimplência", "Total",        "PF",      "Total",  "",      "Inadimplência | Total | PF | Total",          21084,
  "Inadimplência", "Total",        "PJ",      "Total",  "",      "Inadimplência | Total | PJ | Total",          21083
) %>%
  mutate(
    nivel2 = ifelse(is.na(nivel2), "", nivel2),
    chave_ui = paste(grupo, carteira, segmento, nivel1, nivel2, sep = " | ")
  )

indicadores_meta <- bind_rows(indicadores_meta, adicionais_total) %>%
  distinct(grupo, carteira, segmento, nivel1, nivel2, nome_curto, .keep_all = TRUE)

# ----------------------------------------
# 2) Formatação (pt-BR)
# ----------------------------------------
fmt_tri_brl <- function(x, digitos = 2) {
  if (is.na(x)) return("—")
  paste0("R$ ", scales::number(x, accuracy = 10^(-digitos), big.mark = ".", decimal.mark = ","), " tri")
}
fmt_bi_brl <- function(x, digitos = 1) {
  if (is.na(x)) return("—")
  paste0("R$ ", scales::number(x, accuracy = 10^(-digitos), big.mark = ".", decimal.mark = ","), " bi")
}
fmt_pct <- function(x, digitos = 1) {
  if (is.na(x)) return("—")
  sinal <- ifelse(x > 0, "+", "")
  paste0(sinal, scales::number(x, accuracy = 10^(-digitos), big.mark = ".", decimal.mark = ","), "%")
}
formatar_mes_ano <- function(d) format(d, "%m/%Y")

eh_monetario <- function(grupo) grupo %in% c("Saldo", "Concessões")
to_bi <- function(valor, grupo) ifelse(eh_monetario(grupo), valor / 1000, valor)   # milhões -> bilhões
to_tri <- function(valor, grupo) ifelse(eh_monetario(grupo), valor / 1e6, valor)    # milhões -> trilhões

# ----------------------------------------
# 3) Datas padrão
# ----------------------------------------
if (nrow(dados_bcb) > 0) {
  data_inicio_default <- max(min(dados_bcb$data), as.Date("2015-01-01"))
  data_fim_default    <- max(dados_bcb$data)
} else {
  data_inicio_default <- as.Date("2015-01-01")
  data_fim_default    <- Sys.Date()
}

# ----------------------------------------
# 4) UI
# ----------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de Crédito - BCB"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gerencial", tabName = "gerencial", icon = icon("clipboard-list")),
      menuItem("Visão geral (último mês)", tabName = "visao_geral_mes", icon = icon("calendar-check")),
      menuItem("Visão geral (12m)", tabName = "visao_geral_12m", icon = icon("tachometer-alt")),
      menuItem("Consulta", tabName = "consulta", icon = icon("sliders-h")),
      menuItem("Séries temporais", tabName = "series", icon = icon("chart-line")),
      menuItem("Comparação", tabName = "comparacao", icon = icon("balance-scale"))
    ),
    dateRangeInput(
      "periodo",
      "Período (mês de referência):",
      start = max(data_fim_default - 365 * 2, data_inicio_default),
      end   = data_fim_default,
      format = "yyyy-mm-dd",
      startview = "year"
    ),
    selectInput(
      "x_freq",
      "Frequência de datas no eixo X:",
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
    tags$small(style = "display:block; margin-top:4px; color:#666;", "KPIs monetários em R$ trilhões (tri). Gráficos monetários em R$ bilhões (bi)."),
    tags$small(style = "display:block; margin-top:4px; color:#666;", "Fonte: Banco Central do Brasil (SGS)."),
    actionButton("btn_12m", "Últimos 12 meses"),
    actionButton("btn_24m", "Últimos 24 meses"),
    actionButton("btn_todo", "Desde 2015")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .kpi-grid{display:grid; grid-template-columns:repeat(3,1fr); gap:12px;}
        @media (max-width: 992px){.kpi-grid{grid-template-columns:repeat(1,1fr);}}
        .kpi-card{background:#F7F8FA; border:1px solid #EEF0F3; border-radius:14px; padding:14px 16px;}
        .kpi-title{font-size:16px; font-weight:800; color:#111827; margin-bottom:6px;}
        .kpi-value{font-size:28px; font-weight:800; color:#111827; line-height:1.1;}
        .kpi-sub{font-size:12px; color:#6B7280; margin-top:6px;}
        .kpi-var{font-size:12px; color:#374151; margin-top:6px;}

        /* --- VISÕES GERAIS --- */
        .visao-geral .kpi-title{font-size:22px; font-weight:900;}
        .visao-geral .kpi-value{font-size:42px; font-weight:900; line-height:1.05;}
        .visao-geral .kpi-sub{font-size:15px;}
        .visao-geral .kpi-var{font-size:18px; font-weight:800;}
        .visao-geral .box .box-title{font-size:22px; font-weight:900;}
        .visao-geral .box .box-body{font-size:16px; line-height:1.45;}

        /* --- GERENCIAL --- */
        .gerencial .box .box-title{font-size:18px; font-weight:900;}
        .gerencial table{font-size:14px;}
        .gerencial .hint{color:#6B7280; font-size:12px; margin-top:6px;}

        .plotly .xtick text, .plotly .ytick text{font-size:12px !important; fill:#111827 !important;}
        .plotly .g-xtitle text, .plotly .g-ytitle text{font-size:13px !important; fill:#111827 !important;}
      "))
    ),
    tabItems(
      
      # ------------------------------------------------------------
      # ABA GERENCIAL (NOVA)
      # ------------------------------------------------------------
      tabItem(
        tabName = "gerencial",
        tags$div(
          class = "gerencial",
          fluidRow(
            box(
              width = 12, status = "primary", solidHeader = TRUE,
              title = "Resumo gerencial (mês de referência = último mês no arquivo)",
              tags$div(class = "hint",
                       "Tabela em R$ bi (monetários) e % (juros/inad). m/m e a/a são calculados sobre a série original."
              ),
              tableOutput("ger_tabela")
            )
          ),
          fluidRow(
            box(
              width = 6, status = "info", solidHeader = TRUE,
              title = "Participação no saldo total — Livre vs Direcionado",
              plotlyOutput("ger_share_rlrd", height = "320px")
            ),
            box(
              width = 6, status = "info", solidHeader = TRUE,
              title = "Participação no saldo total — PF vs PJ",
              plotlyOutput("ger_share_pfpj", height = "320px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "visao_geral_mes",
        tags$div(
          class = "visao-geral",
          fluidRow(
            box(
              width = 12, status = "info", solidHeader = TRUE, title = "Como ler estes números",
              HTML(
                "<b>Referência:</b> último mês disponível no arquivo.<br>",
                "<b>Saldos e Concessões:</b> em <b>R$ trilhões (tri)</b>.<br>",
                "<b>Taxa de juros e Inadimplência:</b> em <b>%</b>."
              )
            )
          ),
          uiOutput("kpis_mes")
        )
      ),
      
      tabItem(
        tabName = "visao_geral_12m",
        tags$div(
          class = "visao-geral",
          fluidRow(
            box(
              width = 12, status = "info", solidHeader = TRUE, title = "Como ler estes números",
              HTML(
                "<b>Janela:</b> últimos 12 meses disponíveis (terminando no último mês).<br>",
                "<b>Saldos e Concessões:</b> <u>acumulado</u> em 12 meses (soma mensal) em <b>R$ trilhões (tri)</b>.<br>",
                "<b>Taxa de juros e Inadimplência:</b> <u>média</u> em 12 meses em <b>%</b>."
              )
            )
          ),
          uiOutput("kpis_12m")
        )
      ),
      
      tabItem(
        tabName = "consulta",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE, title = "Consulta (qualquer indicador e período)",
            column(
              width = 3,
              selectInput(
                "q_grupo",
                "Tipo de indicador:",
                choices = c("Saldo", "Concessões", "Inadimplência", "Juros"),
                selected = "Saldo",
                width = "100%"
              )
            ),
            column(
              width = 5,
              selectizeInput(
                "q_indicador",
                "Série:",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "Selecione a série..."),
                width = "100%"
              )
            ),
            column(
              width = 4,
              dateRangeInput(
                "q_periodo",
                "Período:",
                start = max(data_fim_default - 365, data_inicio_default),
                end = data_fim_default,
                format = "yyyy-mm-dd"
              )
            ),
            tags$small(style = "display:block; margin-top:6px; color:#666;", "KPIs em R$ bilhões (bi, monetários) e % (juros/inadimplência). Gráfico em bi (monetários).")
          )
        ),
        uiOutput("q_card"),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE, title = textOutput("q_titulo_grafico"),
              plotlyOutput("q_grafico", height = "420px"))
        )
      ),
      
      tabItem(
        tabName = "series",
        fluidRow(
          box(
            width = 12,
            title = "Filtros",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            column(width = 3, selectInput("ts_grupo", "Tipo de indicador:", choices = c("Saldo", "Concessões", "Inadimplência", "Juros"), selected = "Saldo")),
            column(width = 3, selectInput("ts_carteira", "Carteira:", choices = c("Todos", "Livre", "Direcionado", "Total"), selected = "Todos")),
            column(width = 3, selectInput("ts_segmento", "Segmento:", choices = c("Todos", "Total", "PF", "PJ"), selected = "Todos")),
            column(width = 3, selectInput("ts_nivel1", "Categoria:", choices = c("Todos", sort(unique(indicadores_meta$nivel1))), selected = "Todos")),
            column(width = 3, selectInput("ts_nivel2", "Subcategoria:", choices = c("Todos"), selected = "Todos")),
            column(
              width = 9,
              selectizeInput(
                "ts_serie", "Série:",
                choices = NULL, selected = NULL, multiple = FALSE,
                options = list(placeholder = "Selecione a série...")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = textOutput("titulo_grafico_ts"),
            plotlyOutput("plot_ts", height = "420px")
          )
        )
      ),
      
      tabItem(
        tabName = "comparacao",
        fluidRow(
          box(
            width = 12,
            title = "Comparação (qualquer série com qualquer série)",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            column(
              width = 7,
              selectizeInput(
                "cmp_add_serie",
                "Buscar e adicionar (livre):",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "Digite para buscar qualquer série...")
              )
            ),
            column(width = 2, br(), actionButton("cmp_btn_add", "Adicionar", icon = icon("plus"))),
            column(width = 2, br(), actionButton("cmp_btn_clear", "Limpar seleção", icon = icon("trash"))),
            column(
              width = 1,
              selectInput(
                "cmp_tipo",
                "Tipo:",
                choices = c(
                  "Nível"             = "nivel",
                  "Índice (base 100)" = "indice",
                  "Variação m/m (%)"  = "mm",
                  "Variação a/a (%)"  = "aa"
                ),
                selected = "nivel"
              )
            ),
            tags$hr(),
            selectizeInput(
              "cmp_series",
              "Séries selecionadas:",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "As séries adicionadas aparecem aqui (você pode remover itens).")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = textOutput("titulo_grafico_cmp"),
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_cmp", height = "450px")
          )
        )
      )
    )
  )
)

# ----------------------------------------
# 5) Server
# ----------------------------------------
server <- function(input, output, session) {
  
  output$txt_ultima_atualizacao <- renderText({
    if (nrow(dados_bcb) == 0) return("Último mês no arquivo: —")
    paste0("Último mês no arquivo: ", format(max(dados_bcb$data), "%m/%Y"))
  })
  
  meta_ativo <- reactive({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível. Verifique data/dados_bcb.rds."))
    validate(need("id_sgs" %in% names(dados_bcb), "Seu dados_bcb.rds não tem a coluna id_sgs. Refaça o update_data.R."))
    
    ids_existentes <- unique(as.integer(dados_bcb$id_sgs))
    indicadores_meta %>%
      mutate(id_sgs = suppressWarnings(as.integer(id_sgs))) %>%
      mutate(
        nivel2 = ifelse(is.na(nivel2), "", nivel2),
        eh_derivada = is.na(id_sgs)
      ) %>%
      filter(eh_derivada | id_sgs %in% ids_existentes) %>%
      arrange(grupo, carteira, segmento, nivel1, nivel2, nome_curto)
  })
  
  observeEvent(input$btn_12m, {
    fim <- if (nrow(dados_bcb) > 0) max(dados_bcb$data) else Sys.Date()
    updateDateRangeInput(session, "periodo", start = fim - 365, end = fim)
  })
  observeEvent(input$btn_24m, {
    fim <- if (nrow(dados_bcb) > 0) max(dados_bcb$data) else Sys.Date()
    updateDateRangeInput(session, "periodo", start = fim - 365 * 2, end = fim)
  })
  observeEvent(input$btn_todo, {
    fim <- if (nrow(dados_bcb) > 0) max(dados_bcb$data) else Sys.Date()
    updateDateRangeInput(session, "periodo", start = as.Date("2015-01-01"), end = fim)
  })
  
  dados_todos <- reactive({
    validate(
      need(nrow(dados_bcb) > 0, "Nenhum dado disponível."),
      need(!is.null(input$periodo), "Selecione um período.")
    )
    dados_bcb %>% filter(data >= input$periodo[1], data <= input$periodo[2])
  })
  
  gerar_breaks_x <- function(datas, by = "3 months") {
    datas <- datas[!is.na(datas)]
    if (length(datas) == 0) return(NULL)
    dmin <- lubridate::floor_date(min(datas), "month")
    dmax <- max(datas)
    brks <- seq.Date(from = dmin, to = lubridate::ceiling_date(dmax, "month"), by = by)
    sort(unique(c(brks, dmax)))
  }
  
  aplicar_filtros <- function(meta, grupo, carteira, segmento, nivel1, nivel2) {
    out <- meta
    if (!is.null(grupo)    && grupo    != "Todos") out <- out %>% filter(grupo == !!grupo)
    if (!is.null(carteira) && carteira != "Todos") out <- out %>% filter(carteira == !!carteira)
    if (!is.null(segmento) && segmento != "Todos") out <- out %>% filter(segmento == !!segmento)
    if (!is.null(nivel1)   && nivel1   != "Todos") out <- out %>% filter(nivel1 == !!nivel1)
    if (!is.null(nivel2)   && nivel2   != "Todos") out <- out %>% filter(nivel2 == !!nivel2)
    out
  }
  
  observeEvent(meta_ativo(), {
    all_series <- meta_ativo() %>% pull(nome_curto)
    updateSelectizeInput(session, "cmp_add_serie", choices = all_series, selected = NULL, server = TRUE)
    updateSelectizeInput(session, "cmp_series", choices = all_series, selected = character(0), server = TRUE)
    # ts_serie e q_indicador são inicializados pelos observers de grupo abaixo
  }, ignoreInit = FALSE)
  
  # Consulta: quando q_grupo muda, atualiza q_indicador filtrado
  observeEvent(list(meta_ativo(), input$q_grupo), {
    req(input$q_grupo)
    series_grupo <- meta_ativo() %>% filter(grupo == input$q_grupo) %>% arrange(nome_curto) %>% pull(nome_curto)
    updateSelectizeInput(session, "q_indicador", choices = series_grupo, selected = series_grupo[1] %||% NULL, server = TRUE)
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$ts_grupo, input$ts_carteira, input$ts_segmento), {
    m <- aplicar_filtros(meta_ativo(), input$ts_grupo, input$ts_carteira, input$ts_segmento, "Todos", "Todos")
    op_n1 <- m %>% distinct(nivel1) %>% arrange(nivel1) %>% pull(nivel1)
    updateSelectInput(session, "ts_nivel1", choices = unique(c("Todos", op_n1)), selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$ts_grupo, input$ts_carteira, input$ts_segmento, input$ts_nivel1), {
    m <- aplicar_filtros(meta_ativo(), input$ts_grupo, input$ts_carteira, input$ts_segmento, input$ts_nivel1, "Todos")
    op_n2 <- m %>% distinct(nivel2) %>% arrange(nivel2) %>% pull(nivel2)
    op_n2 <- op_n2[op_n2 != ""]
    updateSelectInput(session, "ts_nivel2", choices = unique(c("Todos", op_n2)), selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$ts_grupo, input$ts_carteira, input$ts_segmento, input$ts_nivel1, input$ts_nivel2), {
    m <- aplicar_filtros(meta_ativo(), input$ts_grupo, input$ts_carteira, input$ts_segmento, input$ts_nivel1, input$ts_nivel2)
    op_serie <- m %>% arrange(nome_curto) %>% pull(nome_curto)
    updateSelectizeInput(session, "ts_serie", choices = op_serie, selected = op_serie[1] %||% NULL, server = TRUE)
  }, ignoreInit = FALSE)
  
  observeEvent(input$cmp_btn_add, {
    req(input$cmp_add_serie)
    atual <- input$cmp_series %||% character(0)
    updateSelectizeInput(session, "cmp_series", selected = unique(c(atual, input$cmp_add_serie)))
  })
  observeEvent(input$cmp_btn_clear, {
    updateSelectizeInput(session, "cmp_series", selected = character(0))
  })
  
  meta_row <- function(nome_curto) {
    m <- meta_ativo() %>% filter(nome_curto == !!nome_curto)
    validate(need(nrow(m) > 0, paste0("Série não disponível: ", nome_curto)))
    m[1, ]
  }
  
  serie_valores <- function(nome_curto, df_base) {
    m <- meta_row(nome_curto)
    id <- suppressWarnings(as.integer(m$id_sgs))
    
    if (!is.na(id)) {
      return(df_base %>% filter(id_sgs == id) %>% arrange(data) %>% select(data, valor))
    }
    
    # derivada: TOTAL PF/PJ (para Saldo/Concessões/Juros)
    if (!(m$carteira == "Total" && m$segmento %in% c("PF", "PJ") && m$nivel1 == "Total" && m$nivel2 == "")) {
      return(tibble())
    }
    
    nome_livre <- paste(m$grupo, "| Livre |", m$segmento, "| Total")
    nome_dir   <- paste(m$grupo, "| Direcionado |", m$segmento, "| Total")
    
    id_livre <- meta_ativo() %>% filter(nome_curto == nome_livre) %>% pull(id_sgs) %>% suppressWarnings(as.integer(.)) %>% .[1]
    id_dir   <- meta_ativo() %>% filter(nome_curto == nome_dir)   %>% pull(id_sgs) %>% suppressWarnings(as.integer(.)) %>% .[1]
    
    if (is.na(id_livre) || is.na(id_dir)) return(tibble())
    
    dl <- df_base %>% filter(id_sgs == id_livre) %>% select(data, valor_livre = valor)
    dd <- df_base %>% filter(id_sgs == id_dir)   %>% select(data, valor_dir = valor)
    
    out <- full_join(dl, dd, by = "data") %>%
      arrange(data) %>%
      mutate(
        valor = dplyr::case_when(
          m$grupo %in% c("Saldo", "Concessões") ~ coalesce(valor_livre, 0) + coalesce(valor_dir, 0),
          m$grupo == "Juros" ~ (coalesce(valor_livre, NA_real_) + coalesce(valor_dir, NA_real_)) / 2,
          TRUE ~ NA_real_
        )
      ) %>%
      select(data, valor)
    out
  }
  
  dados_ultimo_mes <- reactive({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    fim <- max(dados_bcb$data)
    dados_bcb %>% filter(data == fim)
  })
  txt_ref_mes <- reactive({
    if (nrow(dados_bcb) == 0) return("—")
    formatar_mes_ano(max(dados_bcb$data))
  })
  dados_12m <- reactive({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    fim <- max(dados_bcb$data)
    inicio <- fim %m-% months(11)
    dados_bcb %>% filter(data >= inicio, data <= fim)
  })
  txt_janela_12m <- reactive({
    if (nrow(dados_bcb) == 0) return("—")
    fim <- max(dados_bcb$data)
    inicio <- fim %m-% months(11)
    paste0(formatar_mes_ano(inicio), " a ", formatar_mes_ano(fim))
  })
  
  calc_var <- function(df) {
    df <- df %>% arrange(data)
    v <- df$valor
    mm <- if (length(v) >= 2) (dplyr::last(v) / dplyr::nth(v, length(v) - 1) - 1) * 100 else NA_real_
    aa <- if (length(v) >= 13) (dplyr::last(v) / dplyr::nth(v, length(v) - 12) - 1) * 100 else NA_real_
    list(mm = mm, aa = aa)
  }
  
  kpi_card <- function(titulo, valor_fmt, ref_txt, mm_fmt = "—", aa_fmt = "—") {
    tags$div(class = "kpi-card",
             tags$div(class = "kpi-title", titulo),
             tags$div(class = "kpi-value", valor_fmt),
             tags$div(class = "kpi-sub", ref_txt),
             tags$div(class = "kpi-var", paste0("m/m: ", mm_fmt, "  •  a/a: ", aa_fmt))
    )
  }
  
  kpi_last <- function(nome_curto, titulo) {
    m <- meta_row(nome_curto)
    eh_pct <- m$grupo %in% c("Juros", "Inadimplência", "Spread")
    base <- serie_valores(nome_curto, dados_bcb)
    df <- base %>% filter(data == max(dados_bcb$data))
    validate(need(nrow(df) > 0, paste0("Sem dados para ", nome_curto)))
    val <- df$valor[1]
    vars <- calc_var(base)
    mm_fmt <- if (is.finite(vars$mm)) fmt_pct(vars$mm, 1) else "—"
    aa_fmt <- if (is.finite(vars$aa)) fmt_pct(vars$aa, 1) else "—"
    if (eh_pct) {
      kpi_card(titulo, fmt_pct(val, 1), paste0("Referência: ", txt_ref_mes(), " • %"), mm_fmt, aa_fmt)
    } else {
      kpi_card(titulo, fmt_tri_brl(val / 1e6, 2), paste0("Referência: ", txt_ref_mes(), " • R$ tri"), mm_fmt, aa_fmt)
    }
  }
  
  kpi_12m <- function(nome_curto, titulo) {
    m <- meta_row(nome_curto)
    eh_pct <- m$grupo %in% c("Juros", "Inadimplência", "Spread")
    df <- serie_valores(nome_curto, dados_12m())
    validate(need(nrow(df) > 0, paste0("Sem dados para ", nome_curto)))
    base <- serie_valores(nome_curto, dados_bcb)
    vars <- calc_var(base)
    mm_fmt <- if (is.finite(vars$mm)) fmt_pct(vars$mm, 1) else "—"
    aa_fmt <- if (is.finite(vars$aa)) fmt_pct(vars$aa, 1) else "—"
    if (eh_pct) {
      val <- mean(df$valor, na.rm = TRUE)
      kpi_card(titulo, fmt_pct(val, 1), paste0("Média 12m (", txt_janela_12m(), ") • %"), mm_fmt, aa_fmt)
    } else {
      val <- sum(df$valor, na.rm = TRUE)
      kpi_card(titulo, fmt_tri_brl(val / 1e6, 2), paste0("Acumulado 12m (", txt_janela_12m(), ") • R$ tri"), mm_fmt, aa_fmt)
    }
  }
  
  output$kpis_mes <- renderUI({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    tags$div(class = "kpi-grid",
             kpi_last("Saldo | Total | PF | Total", "Saldo PF"),
             kpi_last("Saldo | Total | PJ | Total", "Saldo PJ"),
             kpi_last("Saldo | Total | Total | Total", "Saldo Total"),
             
             kpi_last("Concessões | Total | PF | Total", "Concessões PF"),
             kpi_last("Concessões | Total | PJ | Total", "Concessões PJ"),
             kpi_last("Concessões | Total | Total | Total", "Concessões Total"),
             
             kpi_last("Juros | Total | PF | Total", "Taxa de juros PF"),
             kpi_last("Juros | Total | PJ | Total", "Taxa de juros PJ"),
             kpi_last("Juros | Total | Total | Total", "Taxa de juros Total"),
             
             kpi_last("Inadimplência | Total | PF | Total", "Inadimplência PF"),
             kpi_last("Inadimplência | Total | PJ | Total", "Inadimplência PJ"),
             kpi_last("Inadimplência | Total | Total | Total", "Inadimplência Total")
    )
  })
  
  output$kpis_12m <- renderUI({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    tags$div(class = "kpi-grid",
             kpi_12m("Saldo | Total | PF | Total", "Saldo PF"),
             kpi_12m("Saldo | Total | PJ | Total", "Saldo PJ"),
             kpi_12m("Saldo | Total | Total | Total", "Saldo Total"),
             
             kpi_12m("Concessões | Total | PF | Total", "Concessões PF"),
             kpi_12m("Concessões | Total | PJ | Total", "Concessões PJ"),
             kpi_12m("Concessões | Total | Total | Total", "Concessões Total"),
             
             kpi_12m("Juros | Total | PF | Total", "Taxa de juros PF"),
             kpi_12m("Juros | Total | PJ | Total", "Taxa de juros PJ"),
             kpi_12m("Juros | Total | Total | Total", "Taxa de juros Total"),
             
             kpi_12m("Inadimplência | Total | PF | Total", "Inadimplência PF"),
             kpi_12m("Inadimplência | Total | PJ | Total", "Inadimplência PJ"),
             kpi_12m("Inadimplência | Total | Total | Total", "Inadimplência Total")
    )
  })
  
  output$q_card <- renderUI({
    req(input$q_indicador, input$q_periodo)
    df <- serie_valores(input$q_indicador, dados_bcb) %>%
      filter(data >= input$q_periodo[1], data <= input$q_periodo[2]) %>%
      arrange(data)
    validate(need(nrow(df) > 0, "Sem dados para esse indicador no período."))
    
    m <- meta_row(input$q_indicador)
    eh_pct <- m$grupo %in% c("Juros", "Inadimplência", "Spread")
    val <- dplyr::last(df$valor)
    ref <- formatar_mes_ano(max(df$data))
    vars <- calc_var(df)
    mm_fmt <- if (is.finite(vars$mm)) fmt_pct(vars$mm, 1) else "—"
    aa_fmt <- if (is.finite(vars$aa)) fmt_pct(vars$aa, 1) else "—"
    
    tags$div(class = "kpi-grid",
             if (eh_pct) kpi_card("Seleção", fmt_pct(val, 1), paste0("Referência: ", ref, " • %"), mm_fmt, aa_fmt)
             else kpi_card("Seleção", fmt_bi_brl(val / 1000, 1), paste0("Referência: ", ref, " • R$ bi"), mm_fmt, aa_fmt)
    )
  })
  
  output$q_titulo_grafico <- renderText({ req(input$q_indicador); input$q_indicador })
  
  # ----------------------------------------
  # Plot (Consulta / Séries) — legenda no eixo Y
  # ----------------------------------------
  plot_serie <- function(df, eh_pct, x_by = "3 months") {
    df <- df %>% arrange(data)
    
    if (!eh_pct) df <- df %>% mutate(valor_plot = valor / 1000) else df <- df %>% mutate(valor_plot = valor)
    
    df <- df %>%
      filter(!is.na(data), !is.na(valor_plot)) %>%
      mutate(
        tooltip = paste0(
          "Data: ", formatar_mes_ano(data), "<br>",
          if (eh_pct) paste0("Valor: ", scales::number(valor_plot, accuracy = 0.01, big.mark='.', decimal.mark=','), "%")
          else paste0("Valor: R$ ", scales::number(valor_plot, accuracy = 0.01, big.mark='.', decimal.mark=','), " bi")
        )
      )
    
    validate(need(nrow(df) > 0, "Sem dados válidos para plotar."))
    
    tickvals <- gerar_breaks_x(df$data, by = x_by)
    
    y_title <- if (eh_pct) "Valor (%)" else "Valor (R$ bilhões — bi)"
    
    plotly::plot_ly(
      data = df,
      x = ~data,
      y = ~valor_plot,
      type = "scatter",
      mode = "lines+markers",
      text = ~tooltip,
      hoverinfo = "text",
      line = list(width = 2),
      marker = list(size = 5)
    ) %>%
      layout(
        hovermode = "x unified",
        xaxis = list(
          title = "Data",
          tickformat = "%m/%Y",
          tickmode = "array",
          tickvals = tickvals
        ),
        yaxis = list(
          title = y_title,
          rangemode = "tozero"
        ),
        margin = list(l = 75, r = 20, b = 60, t = 10)
      )
  }
  
  output$q_grafico <- renderPlotly({
    req(input$q_indicador, input$q_periodo)
    df <- serie_valores(input$q_indicador, dados_bcb) %>%
      filter(data >= input$q_periodo[1], data <= input$q_periodo[2])
    validate(need(nrow(df) > 0, "Sem dados para esse indicador no período."))
    m <- meta_row(input$q_indicador)
    plot_serie(df, m$grupo %in% c("Juros", "Inadimplência", "Spread"), x_by = input$x_freq)
  })
  
  output$titulo_grafico_ts <- renderText({ req(input$ts_serie); input$ts_serie })
  
  output$plot_ts <- renderPlotly({
    req(input$ts_serie)
    df <- serie_valores(input$ts_serie, dados_todos())
    validate(need(nrow(df) > 0, "Sem dados retornados para essa série e período."))
    m <- meta_row(input$ts_serie)
    plot_serie(df, m$grupo %in% c("Juros", "Inadimplência", "Spread"), x_by = input$x_freq)
  })
  
  # ----------------------------------------
  # Comparação — legenda no eixo Y
  # ----------------------------------------
  output$titulo_grafico_cmp <- renderText({
    req(input$cmp_series)
    paste0("Comparação: ", length(input$cmp_series), " série(s) • tipo: ", input$cmp_tipo)
  })
  
  output$plot_cmp <- renderPlotly({
    req(input$cmp_series)
    tipo <- input$cmp_tipo %||% "nivel"
    
    df <- purrr::map_dfr(input$cmp_series, function(nm) {
      s <- serie_valores(nm, dados_todos())
      if (nrow(s) == 0) return(tibble())
      m <- meta_row(nm)
      eh_pct <- m$grupo %in% c("Juros", "Inadimplência", "Spread")
      s %>% mutate(series = nm, eh_pct = eh_pct)
    })
    
    validate(need(nrow(df) > 0, "Sem dados para as séries selecionadas no período."))
    
    df <- df %>%
      group_by(series) %>%
      arrange(data, .by_group = TRUE) %>%
      mutate(
        nivel = ifelse(eh_pct, valor, valor / 1000), # monetário em R$ bi; % mantém
        base  = dplyr::first(nivel[!is.na(nivel) & is.finite(nivel)]),
        indice = ifelse(is.na(base) | base == 0, NA_real_, (nivel / base) * 100),
        mm = (nivel / lag(nivel) - 1) * 100,
        aa = (nivel / lag(nivel, 12) - 1) * 100
      ) %>%
      ungroup()
    
    df <- df %>%
      mutate(valor_plot = dplyr::case_when(
        tipo == "nivel"  ~ nivel,
        tipo == "indice" ~ indice,
        tipo == "mm"     ~ mm,
        tipo == "aa"     ~ aa,
        TRUE             ~ nivel
      )) %>%
      filter(!is.na(data), !is.na(valor_plot), is.finite(valor_plot))
    
    validate(need(nrow(df) > 0, "Sem valores válidos para plotar (verifique o período e o tipo)."))
    
    df <- df %>%
      mutate(
        tooltip = paste0(
          "Série: ", series, "<br>",
          "Data: ", formatar_mes_ano(data), "<br>",
          dplyr::case_when(
            tipo == "nivel" & eh_pct ~ paste0("Valor: ", scales::number(valor_plot, accuracy = 0.01, big.mark='.', decimal.mark=','), "%"),
            tipo == "nivel" & !eh_pct ~ paste0("Valor: R$ ", scales::number(valor_plot, accuracy = 0.01, big.mark='.', decimal.mark=','), " bi"),
            tipo == "indice" ~ paste0("Índice: ", scales::number(valor_plot, accuracy = 0.1, big.mark='.', decimal.mark=',')),
            TRUE ~ paste0("Variação: ", scales::number(valor_plot, accuracy = 0.01, big.mark='.', decimal.mark=','), "%")
          )
        )
      )
    
    tickvals <- gerar_breaks_x(df$data, by = input$x_freq)
    
    tem_pct <- any(df$eh_pct == TRUE, na.rm = TRUE)
    tem_mon <- any(df$eh_pct == FALSE, na.rm = TRUE)
    
    y_title <- dplyr::case_when(
      tipo == "nivel"  & tem_pct & tem_mon ~ "Valor (R$ bi / %)",
      tipo == "nivel"  & tem_mon & !tem_pct ~ "Valor (R$ bilhões — bi)",
      tipo == "nivel"  & tem_pct & !tem_mon ~ "Valor (%)",
      tipo == "indice" ~ "Índice (base 100)",
      tipo %in% c("mm", "aa") ~ "Variação (%)",
      TRUE ~ ""
    )
    
    plotly::plot_ly(
      data = df,
      x = ~data,
      y = ~valor_plot,
      type = "scatter",
      mode = "lines+markers",
      split = ~series,
      color = ~series,
      text = ~tooltip,
      hoverinfo = "text",
      line = list(width = 2),
      marker = list(size = 5)
    ) %>%
      layout(
        hovermode = "x unified",
        xaxis = list(
          title = "Data",
          tickformat = "%m/%Y",
          tickmode = "array",
          tickvals = tickvals
        ),
        yaxis = list(
          title = y_title,
          rangemode = "tozero"
        ),
        legend = list(orientation = "h", x = 0, y = -0.2),
        margin = list(l = 75, r = 20, b = 80, t = 10)
      )
  })
  
  # ============================================================
  # GERENCIAL (NOVA ABA) — helpers + outputs
  # ============================================================
  
  fmt_bi_txt <- function(x, digitos = 1) {
    if (!is.finite(x) || is.na(x)) return("—")
    paste0("R$ ", scales::number(x, accuracy = 10^(-digitos), big.mark = ".", decimal.mark = ","), " bi")
  }
  
  ger_resumo_series <- function(nome_curto, rotulo = NULL) {
    m <- meta_row(nome_curto)
    eh_pct <- m$grupo %in% c("Juros", "Inadimplência", "Spread")
    
    base <- serie_valores(nome_curto, dados_bcb) %>% arrange(data)
    validate(need(nrow(base) > 0, paste0("Sem dados para ", nome_curto)))
    
    val_last <- dplyr::last(base$valor)
    vars <- calc_var(base)
    
    # nível: monetário em R$ bi (no gerencial), % mantém
    nivel_txt <- if (eh_pct) {
      paste0(scales::number(val_last, accuracy = 0.01, big.mark = ".", decimal.mark = ","), "%")
    } else {
      fmt_bi_txt(val_last / 1000, 1)
    }
    
    tibble(
      Indicador = rotulo %||% nome_curto,
      `Nível`   = nivel_txt,
      `m/m`     = if (is.finite(vars$mm)) fmt_pct(vars$mm, 1) else "—",
      `a/a`     = if (is.finite(vars$aa)) fmt_pct(vars$aa, 1) else "—"
    )
  }
  
  
  
  
  output$ger_tabela <- renderTable({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    
    # bloco “core” (bem estilo relatório)
    bind_rows(
      ger_resumo_series("Saldo | Total | Total | Total", "Saldo total (SFN)"),
      ger_resumo_series("Saldo | Livre | Total | Total", "Saldo livre"),
      ger_resumo_series("Saldo | Direcionado | Total | Total", "Saldo direcionado"),
      ger_resumo_series("Saldo | Total | PF | Total", "Saldo PF"),
      ger_resumo_series("Saldo | Total | PJ | Total", "Saldo PJ"),
      
      ger_resumo_series("Concessões | Total | Total | Total", "Concessões (total)"),
      ger_resumo_series("Concessões | Livre | Total | Total", "Concessões (livre)"),
      ger_resumo_series("Concessões | Direcionado | Total | Total", "Concessões (direcionado)"),
      
      ger_resumo_series("Juros | Total | Total | Total", "Taxa média de juros (total)"),
      ger_resumo_series("Inadimplência | Total | Total | Total", "Inadimplência (total)")
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")
  
  output$ger_share_rlrd <- renderPlotly({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    
    fim <- max(dados_bcb$data)
    s_livre <- serie_valores("Saldo | Livre | Total | Total", dados_bcb) %>% filter(data == fim) %>% pull(valor)
    s_dir   <- serie_valores("Saldo | Direcionado | Total | Total", dados_bcb) %>% filter(data == fim) %>% pull(valor)
    
    validate(need(length(s_livre) > 0 && length(s_dir) > 0, "Sem dados para participação Livre/Direcionado."))
    
    df <- tibble(
      grupo = c("Livre", "Direcionado"),
      valor = c(s_livre[1], s_dir[1])
    ) %>% mutate(share = valor / sum(valor))
    
    plot_ly(
      df,
      labels = ~grupo,
      values = ~valor,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent"
    ) %>%
      layout(
        margin = list(l = 10, r = 10, b = 10, t = 10),
        showlegend = FALSE
      )
  })
  
  output$ger_share_pfpj <- renderPlotly({
    validate(need(nrow(dados_bcb) > 0, "Nenhum dado disponível."))
    
    fim <- max(dados_bcb$data)
    s_pf <- serie_valores("Saldo | Total | PF | Total", dados_bcb) %>% filter(data == fim) %>% pull(valor)
    s_pj <- serie_valores("Saldo | Total | PJ | Total", dados_bcb) %>% filter(data == fim) %>% pull(valor)
    
    validate(need(length(s_pf) > 0 && length(s_pj) > 0, "Sem dados para participação PF/PJ."))
    
    df <- tibble(
      grupo = c("PF", "PJ"),
      valor = c(s_pf[1], s_pj[1])
    ) %>% mutate(share = valor / sum(valor))
    
    plot_ly(
      df,
      labels = ~grupo,
      values = ~valor,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent"
    ) %>%
      layout(
        margin = list(l = 10, r = 10, b = 10, t = 10),
        showlegend = FALSE
      )
  })
}

shinyApp(ui, server)
