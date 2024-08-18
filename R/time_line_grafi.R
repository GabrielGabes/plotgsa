# (data ou numerica vs categorica)
time_line_grafi = function(df, col_num, col_cat){
  tabela = df %>%
    group_by(!!sym(col_num), !!sym(col_cat)) %>%
    summarise(qtd =n()) %>% mutate(frequencia = round(n/sum(n), 2)) %>% ungroup()
  #filter(variavel_categorica == 1) %>% ungroup()

  ggplot(tabela, aes(x=!!sym(col_num), n, label=n, y=n)) +
    geom_line() + geom_point(size=2) +
    geom_text(aes(label = n), nudge_y = 1.5) +
    theme_classic()
}

#time_line_grafi(de, 'idade', 'desfecho')
