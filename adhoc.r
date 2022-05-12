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

# Colorblind pallete friendly
# The palette with grey:
cbp1 <- c("#0072B2", "#D55E00", "#009E73",  "#E69F00", "#56B4E9")

# Provedores
provedores <- ggplot(data, aes(x = provedor, fill=provedor)) +
  scale_fill_manual(values = cbp1) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  xlab("Provedores") + 
  ylab("") + 
  theme(legend.position="none")
provedores


tam_equipe <- ggplot(data, aes(x = tam_equipe, fill=tam_equipe)) +
  scale_fill_manual(values = cbp1) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  xlab("Tamanho da equipe em quantidade de pessoas") + 
  ylab("") + 
  theme(legend.position="none")
tam_equipe

segmentacao <- ggplot(data, aes(x = segmentacao, fill=segmentacao)) +
  scale_fill_manual(values = cbp1) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  xlab("Segmentação") + 
  ylab("") + 
  theme(legend.position="none")
segmentacao


tempo <- ggplot(data, aes(x = tempo, fill=tempo)) +
  scale_fill_manual(values = cbp1) +
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  xlab("Tempo em meses") + 
  ylab("") + 
  theme(legend.position="none")
tempo

library(ggpubr)
ggarrange(
  provedores,
  tam_equipe,
  segmentacao,
  tempo,
  ncol = 2, nrow = 2)

