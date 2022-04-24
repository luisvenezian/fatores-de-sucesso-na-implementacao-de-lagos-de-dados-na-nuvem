install.packages("GGally")
library(GGally)

# Create data 
data <- read.csv("dataset/refined.tsv", sep="\t")
data$bool_cert <- factor(data$bool_cert,
                         labels = c(
  "0" = "",
  "1" = "não certificado"
))

# Check correlations (as scatterplots), distribution and print corrleation coefficient 
ggpairs(data[, 
              c('avaliacao', 
                'qt_alteracoes', 
                'qt_falhas'
                )],
        title="Correlação de Pearson"
        )

cor(data[, c('avaliacao', 
           'qt_alteracoes', 
           'qt_falhas')])

library(psych)
pairs.panels(data[, 
                  c('avaliacao', 
                    'qt_alteracoes', 
                    'qt_falhas'
                  )],)
