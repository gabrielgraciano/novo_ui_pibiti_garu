#Enunciados adaptados do exercício de paralisia cerebral
library(shinyWidgets)

enunciados_pc <- list(
  "1" = "Calcule as medidas-resumos adequadas às variáveis e apresente os resultados
  em uma única tabela.",
  
  "2" = "Construa um gráfico para visualizar o grau de disfunção motora oral (DMO).",
  
  "3" = "Construa um gráfico para visualizar os distúrbios de comunicação.",
  
  "4" = "Construa um gráfico para visualizar o tempo de deglutição de alimentos líquidos.",
  
  "5" = "Calcule medidas-resumos adequadas aos tempos de deglutição de alimentos
  (líquidos, pastosos e sólidos) segundo o grupo de crianças. Apresente os resultados
  em uma única tabela.",
  
  "6" = "Observe a tabela de perda auditiva por grupo de crianças.
  Há indícios de que a perda auditiva esteja associada à PC? Qual é o teste estatístico
  mais apropriado para verificar essa associação?",

  "7" = "Cruze os distúrbios de comunicação com o grupo de crianças e dê os resultados
  em uma tabela. Há indícios de que os distúrbios de comunicação estejam associados à PC? 
Verifique sua observação com um teste estatístico apropriado e nível de significância de 5%.",
  
  "8" = "Faça um gráfico para o tempo de deglutição de alimentos líquidos pelo grupo de crianças. 
Há indícios de que o tempo de deglutição de alimentos líquidos esteja associado à PC?
Verifique sua observação com um teste estatístico apropriado e nível de significância de 5%.",
  
"9" = "Faça um gráfico para o tempo de deglutição de alimentos pastosos pelo grupo de crianças. 
Há indícios de que o tempo de deglutição de alimentos pastosos esteja associado à PC? 
Verifique sua observação com um teste estatístico apropriado e nível de significância de 5%.",

"10" = "Faça um gráfico para o tempo de deglutição de alimentos sólidos pelo grupo de crianças. 
Há indícios de que o tempo de deglutição de alimentos sólidos esteja associado à PC? 
Verifique sua observação com um teste estatístico apropriado e nível de significância de 5%.",

"11" = "Faça um gráfico para visualizar o tempo de deglutição de alimentos líquidos com
o tempo de deglutição de alimentos sólidos. Há indícios de que as variáveis estejam
correlacionadas? Verifique sua observação com um teste estatístico apropriado e nível
de significância de 5%.",

"12" = "Faça um gráfico para visualizar o tempo de deglutição de alimentos pastosos
com o tempo de deglutição de alimentos sólidos. Há indícios de que as variáveis estejam
correlacionadas? Verifique sua observação com um teste estatístico apropriado e nível
de significância de 5%."
)

variaveis_fator <- names(dados_paralisia)[sapply(dados_paralisia, is.factor)]
variaveis_numeric <- names(dados_paralisia)[sapply(dados_paralisia, is.numeric)]

variaveis_fator


respostas_esperadas_pc <- list(
  ex1 = list(variavel_quali = variaveis_fator, variavel_quanti = variaveis_numeric, medidas_resumo_quali = 'Porcentagem', medidas_resumo_quanti = 'Média'),
  ex2 = list(variavel_pc_x = 'dmo', variavel_pc_y = "", tipo_grafico_pc = 'Barras'),
  ex3 = list(variavel_pc_x = 'dist_comun', variavel_pc_y = "", tipo_grafico_pc = 'Barras'),
  ex4 = list(variavel_pc_x = "", variavel_pc_y = 'td_liquido', tipo_grafico_pc = 'Boxplot'),
  ex5 = list(medidas_resumo_ex5 = c('Média', 'Desvio-Padrão')),
  ex6 = list(hipotese_ex6 = 'Sim', teste_ex6 = 'Teste Qui-quadrado de Pearson'),
  ex7 = list(hipotese_ex7 = 'Sim', teste_ex7 = 'Teste Qui-quadrado de Pearson'),
  ex8 = list(variavel_pc_x = 'grupo', variavel_pc_y = 'td_liquido', tipo_grafico_pc = 'Boxplot'),
  ex9 = list(variavel_pc_x = 'grupo', variavel_pc_y = 'td_pastoso', tipo_grafico_pc = 'Boxplot'),
  ex10 = list(variavel_pc_x = 'grupo', variavel_pc_y = 'td_solido', tipo_grafico_pc = 'Boxplot'),
  ex11 = list(variavel_pc_x = 'td_solido', variavel_pc_y = 'td_liquido', tipo_grafico_pc = 'Dispersão'),
  ex12 = list(variavel_pc_x = 'td_pastoso', variavel_pc_y = 'td_liquido', tipo_grafico_pc = 'Dispersão')
)

plot_ex6 <- 
  dados_paralisia%>%
  select(perda_audit, grupo)%>%
  tbl_summary(
    by = 'grupo',
    missing = 'no',
    digits = 
      list(all_continuous() ~ 1,
           all_categorical() ~ c(0,2)),
    label = list(
      grupo ~ 'Grupo de crianças por condição de saúde',
      perda_audit ~ 
        'Perda auditiva'))%>%
  modify_header(label = '**Variável**')%>%
  modify_footnote(update = starts_with('stat_') ~ 'n (%):
  frequências absolutas e relativa')

plot_ex6 <-
  plot_ex6%>%
  as_gt()


plot_ex7 <-
  dados_paralisia%>%
  select(dist_comun, grupo)%>%
  tbl_summary(
    by = 'grupo',
    missing = 'no',
    digits = 
      list(all_continuous() ~ 1,
           all_categorical() ~ c(0,2)),
    label = list(
      grupo ~ 'Grupo de crianças por condição de saúde',
      dist_comun ~ 
        'Distúrbios de Comunicação'))%>%
  modify_header(label = '**Variável**')%>%
  modify_footnote(update = starts_with('stat_') ~ 'n (%):
  frequências absolutas e relativa')

plot_ex7 <-
  plot_ex7%>%
  as_gt()

