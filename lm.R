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
modelo_avaliacao <- lm(avaliacao ~ ., data)

step_modelo_avaliacao <- step(modelo_avaliacao, k = 3.841459)

export_summs(step_modelo_avaliacao, scale = F, digits = 5)

# box-cox
lambda_bc <- powerTransform(data$avaliacao) #função powerTransform do pacote car#

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
data$avaliacao_bc <- (((data$avaliacao ^ lambda_bc$lambda) - 1) / 
                        lambda_bc$lambda)

modelo_avaliacao_bc <- lm(avaliacao_bc ~ . - avaliacao, data)

step_modelo_avaliacao_bc <- step(modelo_avaliacao_bc, k = 3.841459)


# Resumo dos dois modelos obtidos pelo procedimento Stepwise (linear e com Box-Cox)
# Função export_summs do pacote jtools
export_summs(step_modelo_avaliacao_bc, step_modelo_avaliacao, scale = F, digits = 6)

summary(step_modelo_avaliacao)

data$yhat_step_modelo_avaliacao <- step_modelo_avaliacao$fitted.values
data$yhat_step_modelo_avaliacao_bc <- (((step_modelo_avaliacao_bc$fitted.values*(lambda_bc$lambda))+
                                    1))^(1/(lambda_bc$lambda))


#Visualizando os dois fitted values no dataset
#modelos step_empresas e step_modelo_bc
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

