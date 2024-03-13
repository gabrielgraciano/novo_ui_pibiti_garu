def_prob <-
  tabItem(tabName = 'def_prob',
          fluidPage(
            h3(strong("Definição de Probabilidade")),
            fluidRow(column(6,
                            wellPanel(HTML("<p>Um evento é um <strong>subconjunto de realizações de interesse entre todas as possíveis</strong>. 
As frequências relativas nos dão uma noção das ocorrências de um determinado evento em um estudo. 
Ou seja, elas são <strong>estimativas de probabilidade</strong> de tais eventos. Essa probabilidade 
se dá dividindo o <strong>número de casos</strong> em que o evento esperado ocorre pelo <strong>número total</strong> de observações.
                    </p>"),
                                      br(),
                                      HTML("<p>Ao lado, encontra-se uma tabela de frequências dos dados. Podemos montá-la com variáveis 
                    qualitativas (ou com quantitativas se delimitarmos intervalos), que podem ser alteradas 
                    ao lado . A partir dessas frequências, calculamos um valor de probabilidade de uma variável 
                    ter um determinado valor dentre todas as ocorrências (um evento). Note que podemos calcular 
                    a probabilidade da <strong>união</strong> ou da <strong>intersecção</strong> de dois eventos.</p>")
                            )),
                     column(6, align = "center",
                            uiOutput("tabelaProbUI")
                     )),
            fluidRow(
              column(6, 
                     column(6, 
                            wellPanel(uiOutput("calcProbUI"))
                     ),
                     column(6, align = "center",
                            wellPanel(uiOutput("formulasProb")))
              ),
              column(6, 
                     h4(strong("Cálculo")),
                     verbatimTextOutput("caixaContaProb"))
            )
          )
  )