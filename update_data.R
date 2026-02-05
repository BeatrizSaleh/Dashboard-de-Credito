library(tidyverse)
library(GetBCBData)
library(lubridate)

#-------------------------------
# Funções auxiliares
#-------------------------------

fim_mes <- function(x) ceiling_date(as.Date(x), "month") - days(1)

# Regra: tentar mês anterior; se não der, mês -2; sempre respeitando o "último mês comum disponível"
definir_cutoff_mensal <- function(df, hoje = Sys.Date()) {
  mes_menos_1 <- fim_mes(floor_date(hoje, "month") - months(1))
  mes_menos_2 <- fim_mes(floor_date(hoje, "month") - months(2))
  
  # último mês disponível por série
  ult_por_serie <- df %>%
    group_by(series.name) %>%
    summarise(ult_data = max(data, na.rm = TRUE), .groups = "drop")
  
  # mês comum = menor "última data" entre as séries
  ult_comum <- min(ult_por_serie$ult_data, na.rm = TRUE)
  
  # tenta mês -1 se já existe para todas as séries; senão tenta mês -2
  alvo <- if (ult_comum >= mes_menos_1) mes_menos_1 else mes_menos_2
  
  # nunca passa do que realmente existe
  cutoff <- min(ult_comum, alvo)
  
  list(cutoff = cutoff, ult_comum = ult_comum, mes_menos_1 = mes_menos_1, mes_menos_2 = mes_menos_2)
}

baixar_series_sgs <- function(meta, data_inicial, data_final) {
  stopifnot(!anyDuplicated(meta$nome_curto))
  stopifnot(!anyDuplicated(meta$id_sgs))
  
  id_vec <- meta$id_sgs
  names(id_vec) <- meta$nome_curto
  
  message("Baixando séries de ", data_inicial, " até ", data_final)
  
  dados <- GetBCBData::gbcbd_get_series(
    id          = id_vec,
    first.date  = data_inicial,
    last.date   = data_final,
    format.data = "long",
    be.quiet    = FALSE,
    use.memoise = FALSE,
    cache.path  = tempdir(),
    do.parallel = FALSE
  )
  
  dados %>%
    rename(
      data  = ref.date,
      valor = value
    ) %>%
    left_join(meta, by = c("series.name" = "nome_curto")) %>%
    arrange(series.name, data)
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
# Executar atualização
#-------------------------------
data_inicial <- as.Date("2015-01-01")

# IMPORTANTE: não corte em "mês fechado" aqui; deixa baixar até hoje e corta depois com lógica
data_final_download <- Sys.Date()

dados_raw <- baixar_series_sgs(indicadores_meta, data_inicial, data_final_download)

cut <- definir_cutoff_mensal(dados_raw, hoje = Sys.Date())

message(
  "Corte escolhido: ", cut$cutoff,
  " | (m-1=", cut$mes_menos_1, ", m-2=", cut$mes_menos_2, ", ultimo_comum=", cut$ult_comum, ")"
)

dados <- dados_raw %>%
  filter(data <= cut$cutoff) %>%
  calcular_variacoes()

if (!dir.exists("data")) dir.create("data")
saveRDS(dados, file = "data/dados_bcb.rds")

message("Arquivo salvo em data/dados_bcb.rds com ", nrow(dados), " linhas.")
