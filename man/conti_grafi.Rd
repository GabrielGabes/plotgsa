% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conti_grafi.R
\name{conti_grafi}
\alias{conti_grafi}
\title{conti_grafi}
\usage{
conti_grafi(df, coluna_x, coluna_y, sentido_percent = "col")
}
\arguments{
\item{sentido_percent}{}
}
\description{
para representar uma tabela de contingencia (categorica vs categorica)
}
\examples{
conti_grafi(mtcars, 'vs', 'am')
# #ggsave("nome_grafico.png", height=15, width=20, units="cm", dpi= 600)
}
