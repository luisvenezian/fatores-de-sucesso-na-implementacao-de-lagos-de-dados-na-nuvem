pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


data <- read.csv("dataset/refined.tsv", sep="\t")


# dummizando variáveis qualitativas
data <- dummy_columns(.data = data,
                      select_columns =c(
                        "provedor", 
                        "tam_equipe",
                        "maior_desafio",
                        "porte",
                        "tam_investimento",
                        "segmentacao",
                        "tempo"
                      ),
                      remove_selected_columns = T,
                      remove_most_frequent_dummy = T)

# modelo de regressão linear em função da avaliação do projeto
data <- subset(data, select = -c(
  bool_falha_gov,
  bool_falha_dev,
  bool_falha_aus_info,
  bool_falha_pln_custo,
  bool_falha_gst_lid,
  bool_falha_usu,
  avaliacao))

summary(data)
modelo_qt_falhas <- lm(qt_falhas ~ . , data)

step_modelo_qt_falhas <- step(modelo_qt_falhas, k = 3.841459)

export_summs(step_modelo_qt_falhas, scale = F, digits = 5)

# box-cox
lambda_bc <- powerTransform(data$qt_falhas) #função powerTransform do pacote car#

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
data$qt_falhas_bc <- (((data$qt_falhas ^ lambda_bc$lambda) - 1) / 
                        lambda_bc$lambda)

modelo_qt_falhas_bc <- lm(qt_falhas_bc ~ . - qt_falhas, data)

step_modelo_qt_falhas_bc <- step(modelo_qt_falhas_bc, k = 3.841459)


# Resumo dos dois modelos obtidos pelo procedimento Stepwise (linear e com Box-Cox)
# Função export_summs do pacote jtools
export_summs(step_modelo_qt_falhas_bc, step_modelo_qt_falhas, scale = F, digits = 6)



data$yhat_step_modelo_qt_falhas <- step_modelo_qt_falhas$fitted.values
data$yhat_step_modelo_qt_falhas_bc <- (((step_modelo_qt_falhas_bc$fitted.values*(lambda_bc$lambda))+
                                          1))^(1/(lambda_bc$lambda))


#Visualizando os dois fitted values no dataset
#modelos step_empresas e step_modelo_bc
data %>%
  select(qt_falhas, yhat_step_modelo_qt_falhas, yhat_step_modelo_qt_falhas_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

# plot
data %>%
  ggplot() +
  geom_smooth(aes(x = qt_falhas, y = yhat_step_modelo_qt_falhas, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = qt_falhas, y = yhat_step_modelo_qt_falhas),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = qt_falhas, y = yhat_step_modelo_qt_falhas_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = qt_falhas, y = yhat_step_modelo_qt_falhas_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = qt_falhas, y = qt_falhas), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Quantidade de falhas", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


summary(step_modelo_qt_falhas_bc)


install.packages("relaimpo")
library(relaimpo)

relImportance <- calc.relimp(step_modelo_qt_falhas_bc, type = "lmg", rela = TRUE)
importances <- sort(relImportance$lmg, decreasing=TRUE)
sort(relImportance$lmg, decreasing=TRUE)


relative_importance <- calc.relimp(step_modelo_qt_falhas_bc, type="lmg")$lmg

df = data.frame(
  variable=names(relative_importance),
  importance=round(c(relative_importance) * 100,2)
)

ggplot(df, aes(x = reorder(variable, -importance), y = importance)) +
  geom_col(fill = "deepskyblue4") + 
  geom_text(aes(label=importance), vjust=.3, hjust=1.2, size=3, color="white")+
  coord_flip() +
  labs(title = "Relative importance of variables",
       subtitle = "Key factors that influence the fails quantity of this \ndata lake projects survey") +
  theme_classic(base_size = 16)


# Análise da avaliação em função das variáveis que mais explicam o R² 
data$bool_cert <-factor(data$bool_cert, levels=c(0,1), labels=c("Não", "Sim"))
data$bool_mtd_scrum <- factor(data$bool_mtd_scrum, levels=c(0,1), labels=c("Não","Sim"))
data$`tam_equipe_]10,20]` <- factor(data$`tam_equipe_]10,20]`, levels=c(0,1), labels=c("Não","Sim"))
data$bool_experiencia <- factor(data$bool_experiencia, levels=c(0,1), labels=c("Não","Sim"))
data$provedor_Azure <- factor(data$provedor_Azure, levels=c(0,1), labels=c("Não","Sim"))
data$maior_desafio_Volume <- factor(data$maior_desafio_Volume, levels=c(0,1), labels=c("Não","Sim"))


# t test
p_bool_cert <- round(t.test(data[data$bool_cert == "Não",]$qt_falhas, data[data$bool_cert == "Sim",]$qt_falhas)$p.value, 6)
p_bool_mtd_scrum <- round(t.test(data[data$bool_mtd_scrum == "Não",]$qt_falhas, data[data$bool_mtd_scrum == "Sim",]$qt_falhas)$p.value, 6)
p_bool_tam_equipe <- round(t.test(data[data$`tam_equipe_]10,20]` == "Não",]$qt_falhas, data[data$`tam_equipe_]10,20]` == "Sim",]$qt_falhas)$p.value, 6)
p_bool_experiencia <- round(t.test(data[data$bool_experiencia == "Não",]$qt_falhas, data[data$bool_experiencia == "Sim",]$qt_falhas)$p.value, 6)
p_bool_provedor_Azure <- round(t.test(data[data$provedor_Azure == "Não",]$qt_falhas, data[data$provedor_Azure == "Sim",]$qt_falhas)$p.value, 6)
p_bool_maior_desafio_Volume <- round(t.test(data[data$maior_desafio_Volume == "Não",]$qt_falhas, data[data$maior_desafio_Volume == "Sim",]$qt_falhas)$p.value, 6)



library(ggpubr)
library(hrbrthemes)

##########################################################################
##### Plot
##########################################################################

plot_cert <- ggdotplot(data, x = "bool_cert", y = "qt_falhas",
                            color = "bool_cert", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = bool_cert), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Possui certificação\nP Value: ", round(p_bool_cert, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_cert * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
  ) + 
  xlab("")


plot_scrum <- ggdotplot(data, x = "bool_mtd_scrum", y = "qt_falhas",
                       color = "bool_mtd_scrum", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = bool_mtd_scrum), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Usa metodologia scrum\nP Value: ", round(p_bool_mtd_scrum, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_mtd_scrum * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
  ) + 
  xlab("")


plot_equipe <- ggdotplot(data, x = "tam_equipe_]10,20]", y = "qt_falhas",
                        color = "tam_equipe_]10,20]", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = `tam_equipe_]10,20]`), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Equipe entre 10 a 20 pessoas\nP Value: ", round(p_bool_tam_equipe, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_tam_equipe * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
  ) + 
  xlab("")


plot_exp <- ggdotplot(data, x = "bool_experiencia", y = "qt_falhas",
                         color = "bool_experiencia", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = bool_experiencia), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Equipe com experiência em projetos semelhantes\nP Value: ", round(p_bool_experiencia, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_experiencia * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
  ) + 
  xlab("")

plot_azure <- ggdotplot(data, x = "provedor_Azure", y = "qt_falhas",
                      color = "provedor_Azure", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = provedor_Azure), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Utiliza provedor Azure\nP Value: ", round(p_bool_provedor_Azure, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_provedor_Azure * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
  ) + 
  xlab("")

plot_desavio <- ggdotplot(data, x = "maior_desafio_Volume", y = "qt_falhas",
                        color = "maior_desafio_Volume", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = maior_desafio_Volume), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Volume foi o maior desafio\nP Value: ", round(p_bool_maior_desafio_Volume, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_maior_desafio_Volume * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
  ) + 
  xlab("")


plot_cert
plot_scrum
plot_equipe 
plot_exp
plot_azure
plot_desavio

ggarrange(plot_cert + font("title", size = 10),
          plot_scrum + font("title", size = 10),
          plot_equipe + font("title", size = 10),
          plot_exp + font("title", size = 10),
          plot_azure + font("title", size = 10), 
          plot_desavio + font("title", size = 10),
          # labels = c("A", "B", "C"),
          ncol = 2, nrow = 3)


