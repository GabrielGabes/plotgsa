# Função para plotar o boxplot dos resíduos
plotar_boxplot_residuos <- function(lista_modelos, arredondamento = 2) {
  residuos_df <- combinar_residuos(lista_modelos)
  df = residuos_df
  col_cat = 'modelo_nome'
  col_num = '.resid'
  # Calcular medianas e quartis
  quartis = df %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      mediana = median(!!sym(col_num), na.rm = TRUE) %>% round(arredondamento),
      Q1 = quantile(!!sym(col_num), probs = 0.25, na.rm = TRUE) %>% round(arredondamento),
      Q3 = quantile(!!sym(col_num), probs = 0.75, na.rm = TRUE) %>% round(arredondamento)
    ) %>%
    mutate(IQR = (Q3-Q1) %>% round(arredondamento))

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
                                  label = paste0(mediana, " [",Q1, ' - ', Q3,'][',IQR,']')),
              vjust = -1.5, hjust = 0.5) +
    # Outros
    theme(legend.position = "bottom") +
    labs(x=NULL, y=NULL, title=NULL) + coord_flip()
}

# # Exemplo de uso
# modelo_lm0 <- lm(response ~ fixed_effects, data = dff)
# modelo_lm <- lm(response ~ fixed_effects + group, data = dff)
# modelo <- lmer(response ~ fixed_effects + (1|group), data = dff)
#
# plotar_boxplot_residuos(list(modelo_lm0, modelo_lm, modelo))
