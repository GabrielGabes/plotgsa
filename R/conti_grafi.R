#' conti_grafi
#'
#' para representar uma tabela de contingencia (categorica vs categorica)
#'
#' @param df
#' @param coluna_x
#' @param coluna_y
#' @param sentido_percent
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
#' conti_grafi(mtcars, 'vs', 'am')
#' # #ggsave("nome_grafico.png", height=15, width=20, units="cm", dpi= 600)
conti_grafi = function(df, coluna_x, coluna_y, sentido_percent='col'){

  if (sentido_percent == 'row'){ #algoritmo de troca troca #la_ele #kapa_pride
    temp = coluna_x
    coluna_x = coluna_y
    coluna_y = temp
  }

  # Criando tabela de contagem
  grafi = df %>% filter(!is.na(!!sym(coluna_x)) & !is.na(!!sym(coluna_y))) %>%
    group_by(!!sym(coluna_y), !!sym(coluna_x)) %>%
    summarise(qtd = n()) %>%
    mutate(frequencia = round(qtd/sum(qtd)*100, 2)) %>%
    ungroup()

  ggplot(grafi, aes(x=as.factor(!!sym(coluna_y)), y=frequencia, fill=as.factor(!!sym(coluna_x)))) +
    geom_bar(stat="identity", position=position_dodge2(preserve = 'single'), color='black') +
    geom_text(aes(y=frequencia, label = sprintf("%0.1f%%", frequencia)),
              position=position_dodge2(width = 0.9, preserve = 'single'), vjust=-0.5, hjust=0.5) +
    # geom_label(aes(y=frequencia, label = sprintf("%0.1f%%", frequencia)),
    #            position=position_dodge2(width = 0.9, preserve = 'single'), vjust=-0.5, hjust=0.5, fill='white') +
    theme(legend.position = "bottom") +
    scale_y_continuous(labels = scales::percent) +
    scale_y_continuous(limits = c(0, 100), breaks=seq(from=0, to=100, by=10)) +
    labs(x=NULL, y='Frequency (%)', title=NULL, fill=NULL)
}
