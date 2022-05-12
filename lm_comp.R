export_summs(
  step_modelo_avaliacao,
  scale = F, 
  digits = 6,
  model.names = c("Step Avaliação")
)

export_summs(
  step_modelo_qt_falhas_bc,
  scale = F, 
  digits = 6,
  model.names = c("Step Qtd Falhas com Box-Cox")
)


g1 <- c(5,4,3,4,5,4,5,5,4,5,5,4,3,4,5,4,3,4,5,3,4,4,5,4,4,4,2,2,4,4,3,4,4,5,5,4,4,3,5,5,5,5,5,4,4,5,3,5,4,4,4,5,4,4,5,4,4,5,5,5,5,4,5,5,4,5,3)
g2 <- c(5,4,4,3,3,4,4,3,5,4,1,4,4,4,3,4,3,5,2,2,2,3,4,3,4,3,4,4,3,4,4,2,1,4,4)
t.test(g2, g1)
