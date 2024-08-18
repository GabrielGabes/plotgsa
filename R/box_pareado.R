box_pareado = function(df_selecionado, palavra_padrao_da_coluna_analisada){
  df_filter = na.omit(df_selecionado)

  # Transformando para o formato longo
  df_filter$ID = 1:nrow(df_filter)
  dados_long = pivot_longer(df_filter, cols = starts_with(palavra_padrao_da_coluna_analisada),
                            names_to = "momentos", values_to = "medida_numerica")

  # Grafico
  ggplot() +
    geom_boxplot(data=dados_long, aes(x=as.factor(momentos), y=medida_numerica, color=as.factor(momentos)),
                 alpha=0.5, fill = 'white', show.legend = F) +
    geom_point(data=dados_long, aes(x=as.factor(momentos), y=medida_numerica, color=as.factor(momentos)),
               alpha=0.5, size=2.5, show.legend = F) +
    geom_line(data=dados_long, aes(x=as.factor(momentos), y=medida_numerica, color=as.factor(momentos), group = ID),
              alpha=0.5, show.legend = F) +
    labs(x=NULL, y=NULL, title=NULL) +
    theme(legend.position = "none")
}

#box_pareado(dff[c('momento_1', 'momento_2', 'momento_3')], 'momento')
#box_pareado(dff[c('momento_1', 'momento_2', 'momento_3', 'tratamentos')], 'momento') + facet_grid(~tratamentos)
# para representar amostras pareadas

