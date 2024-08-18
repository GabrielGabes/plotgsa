grafico_de_erro = function(df, col_num, col_cat){
  # Tabela com medidas
  tabela = df %>% filter(!is.na(!!sym(col_cat))) %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      media = mean(!!sym(col_num), na.rm = TRUE),
      desvio_padrao = sd(!!sym(col_num), na.rm = TRUE)) %>%
    mutate(ymin= media - desvio_padrao,
           ymax= media + desvio_padrao,
           media = media %>% round(2),
           desvio_padrao = desvio_padrao %>% round(2))
  tabela$ymin = ifelse(tabela$ymin <= 0, 0.01, tabela$ymin)

  ggplot(tabela, aes(x=as.factor(!!sym(col_cat)), y=media, color=as.factor(!!sym(col_cat)))) +
    geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.3, position=position_dodge(0.75)) +
    geom_point(stat="summary", fun="mean", position=position_dodge(0.75),
               shape=21, fill='grey70', color="black", size=2) +
    geom_text(aes(label = paste0(media, " (",desvio_padrao,')')), color='black',
              position= position_dodge2(0.75), hjust=-0.05) +
    theme(legend.position= "none") +
    labs(x=NULL, y= "Mean", title=NULL)
}

# grafico_de_erro(dff, 'momento_3', 'desfecho')
# grafico_de_erro(dff, 'momento_3', 'tratamentos')
