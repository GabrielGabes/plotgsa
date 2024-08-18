# Função para extrair resíduos de diferentes tipos de modelos
extrair_residuos <- function(modelo, nome_modelo) {
  model_diag <- augment(modelo)
  model_diag <- model_diag %>% mutate(modelo_nome = nome_modelo)
  res = model_diag[c('.resid', 'modelo_nome')]
  return(res)
}
