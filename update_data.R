# update_data.R
# Atualiza data/dados_bcb.rds a partir das séries SGS do Banco Central (via GetBCBData)
# - Baixa todas as séries com id_sgs válido no indicadores_meta
# - Corta no "último mês comum disponível" (m-1 se possível; senão m-2)
# - Calcula variações m/m e a/a por id_sgs
# - Salva em data/dados_bcb.rds

library(tidyverse)
library(GetBCBData)
library(lubridate)

# ------------------------------------------------------------
# 1) METADADOS (cole exatamente este bloco no update também)
# ------------------------------------------------------------
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
  
  # total por segmento (PF/PJ)
  "Saldo",         "Total",        "PF",      "Total",  "",      "Saldo | Total | PF | Total",                  20541,
  "Saldo",         "Total",        "PJ",      "Total",  "",      "Saldo | Total | PJ | Total",                  20540,
  "Concessões",    "Total",        "PF",      "Total",  "",      "Concessões | Total | PF | Total",             20633,
  "Concessões",    "Total",        "PJ",      "Total",  "",      "Concessões | Total | PJ | Total",             20632,
  "Juros",         "Total",        "PF",      "Total",  "",      "Juros | Total | PF | Total",                  20716,
  "Juros",         "Total",        "PJ",      "Total",  "",      "Juros | Total | PJ | Total",                  20715,
  "Inadimplência", "Total",        "PF",      "Total",  "",      "Inadimplência | Total | PF | Total",          21084,
  "Inadimplência", "Total",        "PJ",      "Total",  "",      "Inadimplência | Total | PJ | Total",          21083,
) %>%
  mutate(
    nivel2 = ifelse(is.na(nivel2), "", nivel2),
    chave_ui = paste(grupo, carteira, segmento, nivel1, nivel2, sep = " | ")
  )

indicadores_meta <- bind_rows(indicadores_meta, adicionais_total) %>%
  distinct(grupo, carteira, segmento, nivel1, nivel2, nome_curto, .keep_all = TRUE) %>%
  mutate(id_sgs = as.integer(id_sgs))

#-------------------------------
# Funções auxiliares
#-------------------------------
fim_mes <- function(x) ceiling_date(as.Date(x), "month") - days(1)

definir_cutoff_mensal <- function(df, hoje = Sys.Date()) {
  mes_menos_1 <- fim_mes(floor_date(hoje, "month") - months(1))
  mes_menos_2 <- fim_mes(floor_date(hoje, "month") - months(2))
  
  ult_por_serie <- df %>%
    group_by(id_sgs) %>%
    summarise(ult_data = max(data, na.rm = TRUE), .groups = "drop")
  
  ult_comum <- min(ult_por_serie$ult_data, na.rm = TRUE)
  alvo <- if (ult_comum >= mes_menos_1) mes_menos_1 else mes_menos_2
  cutoff <- min(ult_comum, alvo)
  
  list(cutoff = cutoff, ult_comum = ult_comum, mes_menos_1 = mes_menos_1, mes_menos_2 = mes_menos_2)
}

baixar_series_sgs <- function(meta, data_inicial, data_final) {
  stopifnot(all(c("nome_curto", "id_sgs") %in% names(meta)))
  
  meta_valid <- meta %>%
    mutate(id_sgs = as.integer(id_sgs)) %>%
    filter(!is.na(id_sgs)) %>%
    distinct(nome_curto, id_sgs, .keep_all = TRUE)
  
  if (anyDuplicated(meta_valid$nome_curto)) {
    dup <- unique(meta_valid$nome_curto[duplicated(meta_valid$nome_curto)])
    stop("Erro: nome_curto duplicado: ", paste(dup, collapse = ", "))
  }
  if (anyDuplicated(meta_valid$id_sgs)) {
    dup <- unique(meta_valid$id_sgs[duplicated(meta_valid$id_sgs)])
    stop("Erro: id_sgs duplicado: ", paste(dup, collapse = ", "))
  }
  
  id_vec <- meta_valid$id_sgs
  names(id_vec) <- meta_valid$nome_curto
  
  message("Baixando ", length(id_vec), " série(s) de ", data_inicial, " até ", data_final)
  
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  cache_dir <- file.path("data", "cache_gbcbd")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  dados <- GetBCBData::gbcbd_get_series(
    id          = id_vec,
    first.date  = data_inicial,
    last.date   = data_final,
    format.data = "long",
    be.quiet    = FALSE,
    use.memoise = TRUE,
    cache.path  = cache_dir,
    do.parallel = FALSE
  )
  
  dados %>%
    rename(
      data  = ref.date,
      valor = value
    ) %>%
    left_join(meta_valid, by = c("series.name" = "nome_curto")) %>%
    mutate(id_sgs = as.integer(id_sgs)) %>%
    arrange(id_sgs, data)
}

calcular_variacoes <- function(df) {
  df %>%
    group_by(id_sgs) %>%
    arrange(data, .by_group = TRUE) %>%
    mutate(
      var_mm = (valor / lag(valor) - 1) * 100,
      var_aa = (valor / lag(valor, 12) - 1) * 100
    ) %>%
    ungroup()
}

#-------------------------------
# Executar atualização
#-------------------------------
data_inicial <- as.Date("2015-01-01")
data_final_download <- Sys.Date()

dados_raw <- baixar_series_sgs(indicadores_meta, data_inicial, data_final_download)

stopifnot("id_sgs" %in% names(dados_raw))

cut <- definir_cutoff_mensal(dados_raw, hoje = Sys.Date())

message(
  "Corte escolhido: ", cut$cutoff,
  " | (m-1=", cut$mes_menos_1, ", m-2=", cut$mes_menos_2, ", ultimo_comum=", cut$ult_comum, ")"
)

dados <- dados_raw %>%
  filter(data <= cut$cutoff) %>%
  calcular_variacoes()

if (!dir.exists("data")) dir.create("data", recursive = TRUE)
saveRDS(dados, file = "data/dados_bcb.rds")

message("Arquivo salvo em data/dados_bcb.rds com ", nrow(dados), " linhas.")

d <- readRDS("data/dados_bcb.rds")
range(d$data, na.rm = TRUE)
dplyr::count(d, dplyr::if_else(d$data < as.Date("2015-01-01"), "antes_2015", "2015_ou_depois"))
