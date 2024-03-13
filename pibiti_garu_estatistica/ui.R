library(shiny)
library(shinyjs)
library(gt)
library(gtsummary)
library(cowplot)
library(shinydashboard)
library(DT)
library(ggokabeito)
library(ggthemes)
library(keras)

source("data_handling.R")
source("inicio.R")
source("tipos_variaveis.R")
source("tabela_frequencias.R")
source("medidas_resumo.R")
source('graf_qualitativa.R')
source('graf_quantitativa.R')
source('graf_bidimensional.R')
source("prob.R")
source("prob_cond.R")
source("distr_prob.R")
source("inferencia.R") 
source("geometry.R")
source('print_glossario.R')
source('questionario.R')
source('perguntaspibiti.R')
source('expraticos.R')
source('enunciados_pc.R')

useShinyalert(force=TRUE)

#Comcecei aqui
#substitui graficos.R por outras três sources
#substitui inferencia por outras quatro sources (ainda não troquei, talvez não precise)



dashboardPage(
  dashboardHeader(title = 'Garu Estatística'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Início', tabName = 'inicio', icon = icon("home", lib = "font-awesome")),
      
      menuItem('Descritiva', icon = icon("table", lib = "font-awesome"),
               menuSubItem('Tipos de Variáveis', tabName = 'tipos_variaveis'),
               menuSubItem('Tabela de Frequências', tabName = 'tabela_frequencias'),
               menuSubItem('Medidas Resumo', tabName = 'medidas_resumo')),
      
      menuItem('Gráficos', icon = icon("pie-chart", lib="font-awesome"),
               menuSubItem('Variáveis Qualitativas', tabName = 'graf_qualitativa'),
               menuSubItem('Variáveis Quantitativas', tabName = 'graf_quantitativa'),
               menuSubItem('Gráficos Bidimensionais', tabName = 'graf_bidimensional')),
      
      menuItem('Probabilidade', icon = icon("percent", lib="font-awesome"),
               menuSubItem('Definição de Probabilidade', tabName = 'def_prob'),
               menuSubItem('Probabilidade Condicional', tabName = 'prob_cond'),
               menuSubItem('Distribuições de Probabilidade', tabName = 'distr_prob')),
      
      menuItem('Inferência', icon = icon("area-chart", lib="font-awesome"),
               menuSubItem('Teste T para uma amostra', tabName = 'teste_t_1'),
               menuSubItem('Teste T para duas amostras (dep.)', tabName = 'teste_t_2'),
               menuSubItem('Teste qui quadrado', tabName = 'teste_qui'),
               menuSubItem('Teste de Correlação', tabName = 'teste_corr')),
      
      menuItem('Glossário', tabName = 'glossario', icon = icon("font", lib = "font-awesome")),
               
      
      menuItem('Exercícios', tabName = 'questionario', icon = icon("pencil", lib="font-awesome")),

      
      menuItem('Exercícios Práticos',
               menuSubItem('Dados - Paralisia Cerebral', tabName = 'paralisia'))
      
    )
  ),
  
  dashboardBody(
    tags$head(tags$script(HTML('
      var fakeClick = function(tabName) {
        var dropdownList = document.getElementsByTagName("a");
        for (var i = 0; i < dropdownList.length; i++) {
          var link = dropdownList[i];
          if(link.getAttribute("data-value") == tabName) {
            link.click();
          };
        }
      };
    '))),
    tabItems(
      inicio,
      tipos_variaveis,
      tabela_frequencias,
      medidas_resumo,
      graf_quantitativa,
      graf_qualitativa,
      graf_bidimensional,
      def_prob,
      prob_cond,
      distr_prob,
      teste_t_1,
      teste_t_2,
      teste_qui,
      teste_corr,
      glossario,
      questionario,
      paralisia
    )
  )
)





#navbarMenu("Glosssário",
#tabPanel('Glossário', glossario, value = 'tabGlossario'),
#icon = icon('font', lib='font-awesome')),


#tabPanel('Glossário', glossario, value = 'tabGlossario'),
