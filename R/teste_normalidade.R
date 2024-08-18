#' teste_normalidade
#'
#' grafico de normalidade mais teste de hipotese
#'
#' @param df
#' @param coluna
#' @param qtd_bins
#' @param cor_esc
#' @param plot_qqplot
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
#'
#' @examples
#' teste_normalidade(mtcars, 'mpg', cor_esc = 2)
teste_normalidade = function(df, coluna, qtd_bins=20, cor_esc=1, plot_qqplot = T){
  media = mean(df[[coluna]], na.rm=T)
  desvpad = sd(df[[coluna]], na.rm=T)

  if (nrow(df) > 3 & nrow(df) < 5000){
    p_valor = shapiro.test(df[[coluna]])$p.value %>% pval_string() %>% retorne_p_ajust()
    subtitulo = paste(p_valor, '(Shapiro-wilk)')
  } else {
    p_valor = ks.test(df[[coluna]], "pnorm", mean=media, sd=desvpad)$p.value %>% pval_string() %>% retorne_p_ajust()
    subtitulo = paste(p_valor, '(Kolmogorov-Smirnov)')
  }

  lista_cor0 = c('tomato','#40BCD8','#fdf0d5','#fefee3','#bee9e8','#f6aa1c','grey75') # COR DA BARRA
  lista_cor1 = c('red','black','black','black','black','black','black') # COR DA BARRA
  lista_cor2 = c('red','blue','#c1121f','#d68c45','#1b4965','#941b0c','grey45') # COR DA LINHA

  p1 = ggplot(df, aes(x=!!sym(coluna)))+
    geom_histogram(aes(y=..density..), bins=qtd_bins,
                   fill = lista_cor0[cor_esc], alpha = 0.7,
                   colour = lista_cor1[cor_esc]) +
    geom_density(lwd = 1.2, linetype = 2, colour = lista_cor2[cor_esc]) +
    geom_function(fun= dnorm, args=list(mean=media,sd=desvpad), col='black', lwd=1, lty=4) +
    labs(x=NULL, y='Probability Density', subtitle = subtitulo) +
    theme_minimal()

  if (plot_qqplot == T){
    p2 = ggplot(data = df, aes(sample = !!sym(coluna))) +
      stat_qq(shape=21, size=2.5, fill=lista_cor0[cor_esc], alpha = 0.5) +
      stat_qq_line() +
      labs(subtitle = '', x = 'Theoretical Quantiles', y = NULL) + #'Standardized Residuals'
      theme_minimal()
    return((p1+p2))
  } else {
    return(p1)
  }
}
