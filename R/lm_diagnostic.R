#' lm_diagnostic
#'
#' grafico de analise de residuos para modelos lineares
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
#' modelo_lm = lm(mpg ~ vs, data = mtcars, family = binomial())
#' lm_diagnostic(modelo_lm)
#' # modelo_lmer <- lmerTest::lmer(response ~ fixed_effects + (1|group), data = dff)
#' # lm_diagnostic(modelo_lmer)

lm_diagnostic <- function(modelo) {
  # Obter resíduos e valores ajustados
  if (inherits(modelo, "lm") | inherits(modelo, "glm")) {
    model_diag <- augment(modelo)
    model_diag <- model_diag %>%
      mutate(.std.resid = rstandard(modelo))
  } else if (inherits(modelo, "lmerMod") | inherits(modelo, "glmerMod")) {
    model_diag <- augment(modelo)
    model_diag <- model_diag %>%
      mutate(.std.resid = calc_std_resid(modelo, model_diag))
  } else {
    stop("O modelo fornecido não é suportado. Use um modelo 'lm' ou 'lmerMod'.")
  }

  # Gráfico 1: Residuals vs Fitted - Homogeneidade dos Resíduos
  p1 <- ggplot(model_diag, aes(.fitted, .resid)) +
    geom_point(shape=21, size=2, alpha=0.5) +
    geom_hline(aes(yintercept = 0), linetype="dashed", size=1) +
    geom_smooth(se = FALSE, color = "red") +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values",
         y = "Residuals")

  # Gráfico 2: Normal Q-Q - Normalidade dos Resíduos
  ## teste de hipotese: Normalidade (Shapiro Wilk):
  p_valor_Shapiro = shapiro.test(model_diag$.std.resid)$p.value %>% pval_string() %>% retorne_p_ajust()

  p2 <- ggplot(model_diag, aes(sample = .std.resid)) +
    stat_qq(shape=21, size=2) +
    stat_qq_line() +
    labs(title = "Normal Q-Q",
         x = "Theoretical Quantiles",
         y = "Standardized Residuals",
         subtitle = paste0('Shapiro-wilk (',p_valor_Shapiro,')'))

  # Gráfico 3: Scale-Location - Homocedasticidade dos Residuos
  ## teste de hipotese: Homocedasticidade (Breusch-Pagan):
  p_valor_Breusch = lmtest::bptest(modelo_lm)$p.value %>% pval_string() %>% retorne_p_ajust()
  p3 <- ggplot(model_diag, aes(.fitted, sqrt(abs(.std.resid)))) +
    geom_point(shape=21, size=2, alpha=0.5) +
    geom_smooth(se = FALSE, color = "red") +
    labs(title = "Scale-Location",
         x = "Fitted values",
         y = "sqrt(|Standardized Residuals|)",
         subtitle = paste0('Breusch-Pagan (',p_valor_Breusch,')'))

  # Gráfico 4: Residuals vs Leverage - Outliers/Pontos de Alavancagem
  p4 <- ggplot(model_diag, aes(.hat, .std.resid)) +
    geom_point(shape=21, size=2, alpha=0.5) +
    geom_smooth(se = FALSE, color = "red") +
    labs(title = "Residuals vs Leverage",
         x = "Leverage",
         y = "Standardized Residuals")

  # Exibir os gráficos em um layout 2x2
  return(grid.arrange(p1, p2, p3, p4, ncol = 2))
}
