# update_data.R
# Script para:
#  - baixar todas as séries do BCB via GetBCBData
#  - calcular variações m/m e a/a
#  - salvar em data/dados_bcb.rds
#
# Esse script será rodado:
#  - manualmente (se você quiser)
#  - automaticamente pelo GitHub Actions

library(tidyverse)
library(GetBCBData)

#-------------------------------
# 1) Metadados (copie os mesmos do app)
#-------------------------------
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

#-------------------------------
# 2) Funções auxiliares
#-------------------------------

baixar_series_sgs <- function(meta, data_inicial, data_final) {
  id_vec <- meta$id_sgs
  names(id_vec) <- meta$nome_curto
  
  message("Baixando séries de ", data_inicial, " até ", data_final)
  
  dados <- GetBCBData::gbcbd_get_series(
    id          = id_vec,
    first.date  = data_inicial,
    last.date   = data_final,
    format.data = "long",
    be.quiet    = FALSE,
    use.memoise = FALSE,   # em ambiente CI, cache não faz diferença
    cache.path  = tempdir(),
    do.parallel = FALSE
  )
  
  dados %>%
    rename(
      data  = ref.date,
      valor = value
    ) %>%
    left_join(meta, by = c("series.name" = "nome_curto")) %>%
    arrange(data)
}

calcular_variacoes <- function(df) {
  df %>%
    group_by(series.name) %>%
    arrange(data, .by_group = TRUE) %>%
    mutate(
      var_mm = (valor / lag(valor) - 1) * 100,
      var_aa = (valor / lag(valor, 12) - 1) * 100
    ) %>%
    ungroup()
}

#-------------------------------
# 3) Executar atualização
#-------------------------------
data_inicial <- as.Date("2015-01-01")
data_final   <- Sys.Date()

dados <- baixar_series_sgs(indicadores_meta, data_inicial, data_final)
dados <- calcular_variacoes(dados)

# Garante que a pasta data/ existe
if (!dir.exists("data")) dir.create("data")

saveRDS(dados, file = "data/dados_bcb.rds")

message("Arquivo salvo em data/dados_bcb.rds com ", nrow(dados), " linhas.")
