unifesp <- 
  sidebarLayout(
    sidebarPanel(
      selectInput('var_qualitativa', 'Selecione a variável qualitativa', choices = var_qualitativas, multiple = F )
    ), 
    mainPanel(
      fluidRow(
        column(6, plotOutput('pizza_unifesp')),
        column(6, plotOutput('barras_unifesp'))
      )
    )
  )


dados_discentes_f <- data.frame(lapply(dados_discentes, as.factor))


var_qualitativas <- names(dados_discentes_f)[sapply(dados_discentes_f, function(col) is.factor(col))]


library(readr)
library(tidyverse)
dados_discentes <- read_csv("data/dados-unifesp-discentes-2023/X2023_unifesp_dados_socioeconomicos.csv")

#dados_discentes <- dados_discentes[,-6]

#View(dados_discentes)


 #colnames(dados_discentes) <- c('Ano Ingresso', 'Campus', 'Curso', 'Ano de Nascimento',
                               #'Sexo', 'Período de Resposta', 'Seleção', 'Local Nascimento', 'Estado Nascimento',
                               #'Município Nascimento', 'Estado Civil', 'Gênero', 'Identidade de Gênero',
                               #'Filhos', 'Filhos até 5 anos', 'Local Filhos', 'Pele', 'Deficiência', 'Qual Deficiência?', 'Deficiência Visual', 'Deficiência Auditiva',
                               #'Deficiência física', 'TEA', 'Transtorno de Aprendizagem', 'Distúrbio Orgânico', 'Outro Distúrbio Orgânico', 'Adaptação', 'Acessibilidade leitura', 
                               #'Acessibilidade Comunicação', 'Outro recurso acessibilidade', 'Acessibilidade Arquitetônica')











#summary(dados_discentes)
#mode <- function(x) {
  #ux <- unique(x)
  #ux[which.max(tabulate(match(x, ux)))]
#}
#mode(dados_discentes$quest14)

#glimpse(dados_discentes$quest14)

#table(dados_discentes$quest14)
#table(dados_discentes$descr_curso)

#mode(dados_discentes$descr_curso)

