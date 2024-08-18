#' glm_diagnostic
#'
#' grafico de analise de residuos para modelos generalizados
#'
#' @param modelo
#'
#' @return
#' @export
#' @import stats
#' @import rlang
#' @import dplyr
#' @import magrittr
#' @import ggplot
#' @import ggthemes
#' @import scales
#' @import patchwork
#' @import gridExtra
#' @import broom
#' @import broom.mixed
#' @import lmerTest
#'
#' @examples
#' modelo = glm(am ~ vs, data = mtcars, family = binomial())
#' glm_diagnostic(modelo)

glm_diagnostic <- function(modelo) {
  # Obter resíduos e valores ajustados
  if (inherits(modelo, "glm")) {
    model_diag <- augment(modelo)
    model_diag <- model_diag %>%
      mutate(.std.resid = rstandard(modelo))
  } else if (inherits(modelo, "glmerMod")) {
    model_diag <- augment(modelo)
    model_diag <- model_diag %>%
      mutate(.std.resid = calc_std_resid(modelo, model_diag))
  } else {
    stop("O modelo fornecido não é suportado. Use um modelo 'lm' ou 'lmerMod'.")
  }

  # Gráfico 4: Residuals vs Leverage - Outliers/Pontos de Alavancagem
  p4 <- ggplot(model_diag, aes(.hat, .std.resid)) +
    geom_point(shape=21, size=2, alpha=0.5) +
    geom_smooth(se = FALSE, color = "red") +
    labs(title = "Residuals vs Leverage",
         x = "Leverage",
         y = "Standardized Residuals")

  # Exibir os gráficos em um layout 2x2
  return(p4)
}
