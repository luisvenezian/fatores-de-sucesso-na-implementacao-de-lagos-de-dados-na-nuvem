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

###
### MODELO 1 - LM
###
# modelo de regressão linear em função da avaliação do projeto
modelo_avaliacao <- lm(avaliacao ~ ., data)

# stepwise
step_modelo_avaliacao <- step(modelo_avaliacao, k = 3.841459)

###
### MODELO 2 -LM com BOX-COX 
###
lambda_bc <- powerTransform(data$avaliacao) 
data$avaliacao_bc <- (((data$avaliacao ^ lambda_bc$lambda) - 1) / 
                        lambda_bc$lambda)
modelo_avaliacao_bc <- lm(avaliacao_bc ~ . - avaliacao, data)

# stepwise 
step_modelo_avaliacao_bc <- step(modelo_avaliacao_bc, k = 3.841459)


# resumo dos dois modelos obtidos 
export_summs(
  step_modelo_avaliacao_bc, 
  step_modelo_avaliacao, 
  scale = F, 
  digits = 6)


# criando uma variavel com os valores de avaliação calculados pelos modelos
data$yhat_step_modelo_avaliacao <- step_modelo_avaliacao$fitted.values
data$yhat_step_modelo_avaliacao_bc <- (((step_modelo_avaliacao_bc$fitted.values*(lambda_bc$lambda))+
                                    1))^(1/(lambda_bc$lambda))


# visualizando graficamente tb
data %>%
  select(avaliacao, yhat_step_modelo_avaliacao, yhat_step_modelo_avaliacao_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

# plot
data %>%
  ggplot() +
  geom_smooth(aes(x = avaliacao, y = yhat_step_modelo_avaliacao, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = avaliacao, y = yhat_step_modelo_avaliacao),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = avaliacao, y = yhat_step_modelo_avaliacao_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = avaliacao, y = yhat_step_modelo_avaliacao_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = avaliacao, y = avaliacao), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Avaliação", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


# visualizando importância relativa de cada variavel no R²
library(caret)
varImp(step_modelo_avaliacao)
?varImp
# https://www.youtube.com/watch?v=CXG_1O-EWps

library(relaimpo)

relative_importance <- calc.relimp(step_modelo_avaliacao, type="lmg")$lmg

df = data.frame(
variable=names(relative_importance),
importance=round(c(relative_importance) * 100,2)
)

ggplot(df, aes(x = reorder(variable, -importance), y = importance)) +
  geom_col(fill = "deepskyblue4") + 
  geom_text(aes(label=importance), vjust=.3, hjust=1.2, size=3, color="white")+
  coord_flip() +
  labs(title = "Relative importance of variables",
       subtitle = "Key factors that influence the evaluation of this \ndata lake projects survey") +
  theme_classic(base_size = 16)

install.packages("hrbrthemes")
library(hrbrthemes)

# Análise da avaliação em função das variáveis que mais explicam o R² 
data$bool_falha_dev <-factor(data$bool_falha_dev, levels=c(0,1), labels=c("Não", "Sim"))
data$bool_cert <- factor(data$bool_cert, levels=c(0,1), labels=c("Não","Sim"))
data$bool_falha_usu <- factor(data$bool_falha_usu, levels=c(0,1), labels=c("Não","Sim"))
data$bool_falha_aus_info <- factor(data$bool_falha_aus_info, levels=c(0,1), labels=c("Não","Sim"))
data$bool_mtd_classica <- factor(data$bool_mtd_classica, levels=c(0,1), labels=c("Não","Sim"))
data$`tam_equipe_]10,20]` <- factor(data$`tam_equipe_]10,20]`, levels=c(0,1), labels=c("Não","Sim"))

p_bool_cert <- round(t.test(subset(data, bool_cert = "Não")$avaliacao, subset(data, bool_cert == "Sim")$avaliacao)$p.value, 6)




p_bool_falha_dev <- round(t.test(data[data$bool_falha_dev == "Não",]$avaliacao, data[data$bool_falha_dev == "Sim",]$avaliacao)$p.value, 6)
p_bool_cert <- round(t.test(data[data$bool_cert == "Não",]$avaliacao, data[data$bool_cert == "Sim",]$avaliacao)$p.value, 6)
p_bool_falha_usu <- round(t.test(data[data$bool_falha_usu == "Não",]$avaliacao, data[data$bool_falha_usu == "Sim",]$avaliacao)$p.value, 6)
p_bool_falha_aus_info <- round(t.test(data[data$bool_falha_aus_info == "Não",]$avaliacao, data[data$bool_falha_aus_info == "Sim",]$avaliacao)$p.value, 6)
p_bool_mtd_classica <- round(t.test(data[data$bool_mtd_classica == "Não",]$avaliacao, data[data$bool_mtd_classica == "Sim",]$avaliacao)$p.value, 6)
p_bool_mtd_tam_equipe <- round(t.test(data[data$`tam_equipe_]10,20]` == "Não",]$avaliacao, data[data$`tam_equipe_]10,20]` == "Sim",]$avaliacao)$p.value, 6)


library(ggpubr)

plot_falha_dev <- ggdotplot(data, x = "bool_falha_dev", y = "avaliacao",
                color = "bool_falha_dev", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = bool_falha_dev), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Falha no desenvolvimento\nP Value: ", round(p_bool_falha_dev, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_falha_dev * 100)
    )) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
    ) + 
    xlab("")

plot_certificado <- ggdotplot(data, x = "bool_cert", y = "avaliacao",
                            color = "bool_cert", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = bool_cert), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Possuí certificação\nP Value: ", round(p_bool_cert, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_cert * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain")
  ) + 
  xlab("")


plot_falha_usu <- ggdotplot(data, x = "bool_falha_usu", y = "avaliacao",
                              color = "bool_falha_usu", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = bool_falha_usu), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Falha com usuário \nP Value: ", round(p_bool_falha_usu, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_falha_usu * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain")
  ) + 
  xlab("")

plot_aus_info <- ggdotplot(data, x = "bool_falha_aus_info", y = "avaliacao",
                            color = "bool_falha_aus_info", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = bool_falha_aus_info), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Falha de ausência de informação\nP Value: ", round(p_bool_falha_aus_info, 6)),
      " - Percentual de Confiança: ", 100 - (p_bool_falha_aus_info * 100)
    )) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain")
  ) + 
  xlab("")

plot_gestao_classica <- ggdotplot(data, x = "bool_mtd_classica", y = "avaliacao",
                           color = "bool_mtd_classica", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = bool_mtd_classica), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Usou metodologia clássica\nP Value: ", round(p_bool_mtd_classica, 6)),
      "    Percentual de Confiança: ", 100 - (p_bool_mtd_classica * 100)
    )
  ) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain")
  ) + 
  xlab("")
plot_gestao_classica


plot_tam_equipe <- ggdotplot(data, x = "tam_equipe_]10,20]", y = "avaliacao",
                                  color = "tam_equipe_]10,20]", binwidth = .1) +
  theme_ipsum() +
  geom_violin(aes(colour = `tam_equipe_]10,20]`), size=.3, fill="black", alpha = .03)+
  ggtitle(
    paste(
      paste("Equipe de 10 a 20 pessoas.\nP Value: ", round(p_bool_mtd_tam_equipe, 6)),
      "    Percentual de Confiança: ", 100 - (p_bool_mtd_tam_equipe * 100)
    )
  ) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.y = element_text(size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain")
  ) + 
  xlab("")




ggarrange(plot_falha_dev + font("title", size = 10),
          plot_gestao_classica + font("title", size = 10),
          plot_aus_info + font("title", size = 10),
          plot_falha_usu + font("title", size = 10),
          plot_certificado + font("title", size = 10), 
          plot_tam_equipe + font("title", size = 10),
          # labels = c("A", "B", "C"),
          ncol = 2, nrow = 3)

