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
       subtitle = "Main factors for consider when creating data lakes") +
  theme_classic(base_size = 16)
