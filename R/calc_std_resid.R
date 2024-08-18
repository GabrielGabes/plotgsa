#para lidar com objetos lmerMod.
pacman::p_load(
  broom,
  broom.mixed,
  lmerTest,
  lmtest
)

# Função para calcular resíduos padronizados
calc_std_resid <- function(modelo, model_diag) {
  resid <- residuals(modelo)
  hat_values <- model_diag$.hat
  sigma <- sigma(modelo)
  std_resid <- resid / (sigma * sqrt(1 - hat_values))
  return(std_resid)
}
