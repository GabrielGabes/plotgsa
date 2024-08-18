#' densidade_grafi
#'
#' variavel: numerica vs categorica
#'
#' @param df
#' @param col_num
#' @param col_cat
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
densidade_grafi = function(df, col_num, col_cat){

  medidas = df %>% group_by(!!sym(col_cat)) %>% summarize(medida = mean(!!sym(col_num)))

  ggplot(df, aes(x=!!sym(col_num), fill=as.factor(!!sym(col_cat)))) +
    geom_density(position='identity', alpha =0.5)+
    geom_vline(data = medidas, aes(xintercept = medida, color = as.factor(!!sym(col_cat))),
               linetype="dashed", size=1, show.legend=F) +
    geom_text(data = medidas, aes(x = medida, label = round(medida, 0), y = 0),
              color="black", vjust = -0.5, hjust = 1.5) +
    theme(legend.position = "bottom") +
    labs(x=NULL, y='Probability Density', fill=NULL)
}
# densidade_grafi(dff, 'momento_3', 'desfecho')
# densidade_grafi(dff, 'momento_3', 'desfecho') + facet_grid(~desfecho)
# densidade_grafi(dff, 'momento_3', 'tratamentos') + facet_grid(~tratamentos)
