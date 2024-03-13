teste_t_1 <-
  tabItem(tabName = 'teste_t_1',
          sidebarLayout(sidebarPanel(
            h3(strong("Teste T para média de uma amostra")),
            fluidRow(
              column(6, 
                     strong("População"),
                     numericInput("testeT1MediaPop", "Média μ a ser testada", value = 0, step = 0.1),
                     numericInput("testeT1DPPop", "Desvio σ conhecido", value = 1, min = 0.1, step = 0.1)
              ),
              column(6,
                     strong("Amostra"),
                     numericInput("testeT1MediaA", "Média X̄ obtida da amostra", value = 0.18, step = 0.1),
                     numericInput("testeT1TamanhoA", "Tamanho da amostra", value = 10, step = 1)
              )
            ),
            fluidRow(
              column(9, 
                     sliderInput("testeT1Alpha", "Alfa", min = 0.01, max = 0.1, value = 0.05, step = 0.005)
              ),
              column(3, 
                     actionButton("testeT1Refresh", "Atualizar", icon = icon("refresh"))
              )
            )
          ), 
          mainPanel(
            plotOutput("testeT1Plot"),
            verbatimTextOutput("testeT1Texto")
          ))
  )
  
teste_t_2 <-
  tabItem(tabName = 'teste_t_2',
          fluidPage(
            withMathJax(),
            fluidRow(
              h3(strong("Teste T para duas populações dependentes")),
              p("O teste T para duas populações é usado quando essas duas são dependentes, ou seja, pareadas. Isso significa que 
      os dados foram obtidos a partir das mesmas circustâncias, ou seja, os mesmos alunos realizando provas diferentes, 
      os mesmos carros testando gasolinas diferentes, etc. Dessa forma, se houver diferença entre os dois grupos, a diferença 
      é realmente devido ao método."),
              column(4,
                     fluidRow(sliderInput("testeT2TamanhoA", "Tamanho das amostras", min = 5, max = 30, value = 12, step = 1)),
                     fluidRow(actionButton("testeT2Refresh", "Atualizar", icon = icon("refresh")))
              ),
              column(8, 
                     fluidRow(column(6, 
                                     numericInput("testeT2MediaP1", "Média da População P1", value = 10, step = 0.1)
                     ),
                     column(6,
                            numericInput("testeT2DPP1", "Desvio Padrão da População P1", value = 2.2, step = 0.1)
                     )),
                     fluidRow(column(6, 
                                     numericInput("testeT2MediaP2", "Média da População P2", value = 9, step = 0.1)
                     ),
                     column(6,
                            numericInput("testeT2DPP2", "Desvio Padrão da População P2", value = 2.2, step = 0.1)
                     ))
              )
            ),
            fluidRow(
              tableOutput("testeT2Table")
            ),
            fluidRow(
              column(6,
                     plotOutput("testeT2Graph1")
              ),
              column(6, 
                     plotOutput("testeT2Graph2"))
            ),
            fluidRow(
              column(6, 
                     fluidRow(
                       withMathJax(helpText("Valor do teste: $$\\frac{\\bar{D}}{\\frac{S_{D}}{\\sqrt{n}}}$$"))
                     ),
                     fluidRow(
                       verbatimTextOutput("testeT2Calc")
                     )
              ),
              column(6, 
                     sliderInput("testeT2Alpha", "Alfa", min = 0.01, max = 0.1, value = 0.05, step = 0.005),
                     plotOutput("testeT2Graph3")
              )
            )
          )
  )
  

teste_qui <-
  tabItem(tabName = 'teste_qui',
          sidebarLayout(
            sidebarPanel(
              h3(strong("Teste qui-quadrado de independência")),
              p("O teste qui-quadrado de Pearson é aplicado a um conjunto de dados para mensurar o quanto a diferença 
      entre eles é puro acaso ou se há uma influência real."),
              helpText("Matriz de entrada"),
              fluidRow(column(6),
                       column(3, textInput("testeQuiCol1", value = "Melhora", label = "")),
                       column(3, textInput("testeQuiCol2", value = "Não", label = ""))
              ),
              fluidRow(column(6, textInput("testeQuiRow1", value = "Droga", label = "")),
                       column(3, numericInput("testeQuiValor1", value = 48, label = "", min = 0)),
                       column(3, numericInput("testeQuiValor2", value = 8, label = "", min = 0))),
              fluidRow(column(6, textInput("testeQuiRow2", value = "Placebo", label = "")),
                       column(3, numericInput("testeQuiValor3", value = 30, label = "", min = 0)),
                       column(3, numericInput("testeQuiValor4", value = 21, label = "", min = 0))),
              actionButton("testeQuiRefresh", "Atualizar", icon = icon("refresh")),
              sliderInput("testeQuiAlpha", "Alfa", min = 0.01, max = 0.1, value = 0.05, step = 0.01)
            ), mainPanel(
              withMathJax(),
              fluidRow(
                column(6,
                       strong("Tabela de frequências observadas"),
                       tableOutput("testeQuiTabela")
                ),
                column(6, 
                       strong("Tabela de frequências esperadas"),
                       tableOutput("testeQuiEsperado")
                )
              ),
              fluidRow(column(6,
                              withMathJax(helpText("$$\\chi^{2} = \\sum_{i=1}^{r} \\sum_{j=1}^{s}
                                          \\frac{(o_{ij} - e_{i}{j})^{2}}{e_{ij}}$$"))
              ),
              column(6, 
                     uiOutput("testeQuiCalc")
              )),
              plotOutput("testeQuiPlot"),
              verbatimTextOutput("testeQuiConta")
            )
          )
  )
          
  

teste_corr <-
  tabItem(tabName = 'teste_corr',
          sidebarLayout(sidebarPanel(withMathJax(),
                                     h3(strong("Testes de Correlação")),
                                     selectInput("tipoTesteCorr", "Teste", choices = 
                                                   c("Spearman" = "spearman", 
                                                     "Pearson" = "pearson")),
                                     
                                     uiOutput("selectTesteCorrVarUI"),
                                     uiOutput("testeCorrExplicacao")
                                     
                                     
                                     
                                     
          ),
          mainPanel(plotOutput("testeCorrPlot"),
                    verbatimTextOutput("testeCorrConta"))
          )
  )
