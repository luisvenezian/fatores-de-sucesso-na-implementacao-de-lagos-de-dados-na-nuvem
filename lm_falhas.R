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
