fore_plot = function(tabela, titulo = NULL){
  xminimo = min(tabela$`2.5 %`, na.rm = T)
  xmaximo = max(tabela$`97.5 %`, na.rm = T)

  if ( xmaximo >= 100){
    escala = 'log10'
  } else{
    escala = 'log2'
  }

  plot1 = ggplot(tabela, aes(y = variavel, x = OR)) +
    geom_point(shape = 18, size = 5, color = 'navyblue') +
    geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0.25) +
    geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
    xlab("Odds Ratio (95% CI)") +
    ylab(" ") +
    labs(title=titulo) +
    theme_classic() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.text.x.bottom = element_text(size = 12, colour = "black"),
          axis.title.x = element_text(size = 12, colour = "black")) +
    scale_x_continuous(trans='log10') #+ xlim(c(xminimo-2, xmaximo+2)) #+ geom_text(aes(label = pvalor))

  table_base = ggplot(tabela, aes(y=variavel)) +
    ylab(NULL) + xlab("  ") +
    theme(plot.title = element_text(hjust = 0.5, size=12),
          axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
          axis.line = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank())

  tab1 = table_base +
    geom_text(aes(x = 1, label = pvalor), size = 4) +
    ggtitle("P-valor")

  tab2 = table_base +
    labs(title = "space") +
    geom_text(aes(x = 1, label = OR_IC), size = 4) +
    ggtitle("OR(IC)")

  lay =  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
  #return(grid.arrange(plot1, tab1, tab2, layout_matrix = lay))
  return(plot1)
}
