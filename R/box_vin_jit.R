# para representar analise númerica por grupo (variavel: numerica vs categorica)

box_vin_jit = function(df, col_num, col_cat, arredondamento = 0){

  # Calcular medianas e quartis
  quartis = df %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      mediana = median(!!sym(col_num), na.rm = TRUE) %>% round(arredondamento),
      Q1 = quantile(!!sym(col_num), probs = 0.25, na.rm = TRUE) %>% round(arredondamento),
      Q3 = quantile(!!sym(col_num), probs = 0.75, na.rm = TRUE) %>% round(arredondamento)
    )

  ggplot(df, aes(x=as.factor(!!sym(col_cat)), y=!!sym(col_num), fill=as.factor(!!sym(col_cat)))) +
    # Graficos
    #geom_jitter(alpha=0.5, show.legend=F, size=2.5, position=position_jitter(0.25)) +
    geom_violin(alpha=0.2, show.legend=F, fill='white') +
    geom_boxplot(alpha=0.8, show.legend=F, width=0.5) + #outlier.shape = NA
    # Medias extras
    geom_errorbar(stat = "summary", fun.data = "mean_se", width= 0.3, color="white") +
    geom_point(stat = "summary", fun = "mean", show.legend=F,
               shape=21, fill='red', color="black", size=3) +
    # Adicionar labels de texto
    geom_text(data = quartis, aes(x =!!sym(col_cat), y = mediana,
                                  label = paste0(mediana, " [",Q1, ' - ', Q3,']')),
              vjust = -1.5, hjust = 0.5) +
    # Outros
    theme(legend.position = "bottom") +
    labs(x=NULL, y=NULL, title=NULL)
}

# box_vin_jit(dff, 'var_num', 'tratamentos')
# box_vin_jit(dff, 'var_num', 'desfecho') #+ coord_flip()
# ggsave("nome_grafico.png", height=10, width=10.5, units="cm", dpi= 600)
