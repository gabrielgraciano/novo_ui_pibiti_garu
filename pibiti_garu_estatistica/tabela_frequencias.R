tabela_frequencias <-
  tabItem(tabName = 'tabela_frequencias',
          sidebarLayout(
            sidebarPanel(
              h3(strong("Tabela de Frequências")),
              p("Um objetivo comum ao analisar um conjunto de dados é conhecer o comportamento de uma determinada variável. 
      É importante analisar sua distribuição, ou seja, como seus valores estão dispostos. Uma forma de realizar 
      isso é através da tabela de frequências."),
              HTML("<p>A <strong>frequência</strong> é dada por quantas vezes aquele valor ocorre. Uma medida bastante útil em tabelas de 
         frequência é a <strong>proporção</strong> de cada realização em relação ao total. Esta se dá dividindo a frequência de 
         cada valor pelo número total de ocorrências.</p>"),
              HTML("<p>Representar variáveis quantitativas contínuas em uma tabela de frequências é diferente, no entanto. Como 
      pode-se, para este tipo de variável, ter inúmeros valores possíveis, tende-se a dividir esta variável em 
      uma quantidade limitada de <strong>intervalos</strong> (exemplo 3) para que seja possível representá-la em uma tabela de frequência.</p>"),
              br(),
              br(),
              verbatimTextOutput("calcFrequencia"),
              verbatimTextOutput("calcProporcao"),
              br(),
              helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva.")
            ), mainPanel(
              tabsetPanel(
                tabPanel("Exemplo 1", 
                         h4("Tabela com a variável 'Relacionamento'"),
                         helpText("Variável Qualitativa Nominal"),
                         tableOutput("tabelaFreqRelacionamento")), 
                tabPanel("Exemplo 2", 
                         h4("Tabela com a variável 'Ano letivo'"),
                         helpText("Variável Quantitativa Discreta"),
                         tableOutput("tabelaFreqAnoLetivo")),
                tabPanel("Exemplo 3",
                         h4("Tabela com a variável 'Peso'"),
                         helpText("Variável Quantitativa Contínua"),
                         tableOutput("tabelaFreqPeso"),
                         helpText("*Dividiu-se as observações em intervalos."))
              )
            ))
  )