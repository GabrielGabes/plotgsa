#' cont_grafi
#'
#' @param df
#' @param coluna
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
#' cont_grafi(mtcars, 'am')
#' cont_grafi(mtcars, 'gear')
#' # ggsave("nome_grafico.png", height=15, width=20, units="cm", dpi= 600)
#'
cont_grafi = function(df, coluna){
  # Criando tabela de contagem
  tabela = df %>% filter(!is.na(!!sym(coluna))) %>%
    group_by(!!sym(coluna)) %>%
    summarise(qtd =n()) %>%
    mutate(frequencia = round(qtd/sum(qtd)*100, 2)) %>%
    ungroup()

  ggplot(tabela, aes(x=as.factor(!!sym(coluna)), y=frequencia, label=frequencia, fill=as.factor(!!sym(coluna)))) +
    geom_col(show.legend=FALSE, color="black") +
    geom_text(aes(y=frequencia, label = sprintf("%0.1f%%", frequencia), vjust=-0.5)) +
    # geom_label(aes(y=frequencia, label = sprintf("%0.1f%%", frequencia)), vjust=-0.5, fill='white') +
    theme(legend.position = "bottom") +
    scale_y_continuous(limits = c(0, 100), breaks=seq(from = 0, to = 100, by = 20)) +
    labs(x=NULL, y="frequenciauency (%)", title =NULL)
}
