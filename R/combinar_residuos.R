# Função para combinar resíduos de vários modelos
combinar_residuos <- function(lista_modelos) {
  residuos_df <- bind_rows(lapply(seq_along(lista_modelos), function(i) {
    extrair_residuos(lista_modelos[[i]], paste("Modelo", i))
  }))
  return(residuos_df)
}
