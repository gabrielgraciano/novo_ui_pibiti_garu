library(shinyjs)

medidas_resumo <-
  tabItem(tabName = 'medidas_resumo',
          fluidPage(
            title = "Medidas Resumo",
            
            useShinyjs(),
            withMathJax(),
            fluidRow(
              column(6,
                     h4(strong("Gerador de Elementos")),
                     p("Gere uma amostra de elementos (números) através dos parâmetros e do botão 'Gerar Elementos'. Depois 
             acompanhe como suas medidas resumo são calculadas."),
                     verbatimTextOutput("printElementos"),
                     fluidRow(
                       column(4, 
                              sliderInput("slider_qtd_elementos", label = "Quantidade de Elementos (Tamanho da Amostra)", min = 4, 
                                          max = 40, value = 10)),
                       column(4,
                              sliderInput("slider_min_max_valor", label = "Valores entre:", min = 1, max = 50, value = c(1, 10))),
                       column(2,
                              actionButton("geraElementos", "Gerar Elementos"))
                     ),
                     plotOutput("graficoElementos")
              ),
              column(6,
                     fluidRow(
                       column(6, 
                              wellPanel(htmlOutput("mediaTitle"),
                                        actionLink("mediaMostrarMais", "Mostrar mais"),
                                        hidden(actionLink("mediaMostrarMenos", "Mostrar menos")),
                                        hidden(p("A média aritmética, medida mais familiar, é a soma dos valores das 
                                      observações dividido pela quantidade de observações.", id = "mediaTexto")),
                                        hidden(uiOutput("mediaExplain")),
                                        hidden(helpText(id="htMedia", "Exemplo com os elementos gerados:"),
                                               verbatimTextOutput("exMedia"))
                              )),
                       column(6, wellPanel(htmlOutput("medianaTitle"),
                                           actionLink("medianaMostrarMais", "Mostrar mais"),
                                           hidden(actionLink("medianaMostrarMenos", "Mostrar menos")),
                                           hidden(p("A mediana representa a observação que ocupa a metade da lista 
                                          de observações, quando essa está ordenada. Quando o conjunto 
                                          possui um número ímpar de observações, então a mediana é 
                                          simplesmente o valor central. Caso contrário, é feita a média 
                                          dos dois valores centrais.", id = "medianaTexto")),
                                           hidden(uiOutput("medianaExplain")),
                                           hidden(helpText(id="htMediana", "Exemplo com os elementos gerados:"),
                                                  verbatimTextOutput("exMediana"))
                       ))
                     ),
                     fluidRow(
                       column(6, 
                              wellPanel(htmlOutput("modaTitle"),
                                        actionLink("modaMostrarMais", "Mostrar mais"),
                                        hidden(actionLink("modaMostrarMenos", "Mostrar menos")),
                                        hidden(p("A moda é representada pela observação mais frequente do conjunto. Ou 
                                       seja, o valor que aparece mais vezes. Em alguns casos, pode-se ter mais 
                                       de uma moda.", id = "modaTexto")),
                                        hidden(helpText(id="htModa", "Exemplo com os elementos gerados:"),
                                               verbatimTextOutput("exModa"))
                              )),
                       column(6, wellPanel(htmlOutput("minimoTitle"),
                                           actionLink("minimoMostrarMais", "Mostrar mais"),
                                           hidden(actionLink("minimoMostrarMenos", "Mostrar menos")),
                                           hidden(p("Observação de menor valor.", id = "minimoTexto")),
                                           hidden(helpText(id="htMinimo", "Exemplo com os elementos gerados:"),
                                                  verbatimTextOutput("exMinimo"))
                       ))
                     ),
                     fluidRow(
                       column(6, 
                              wellPanel(htmlOutput("maximoTitle"),
                                        actionLink("maximoMostrarMais", "Mostrar mais"),
                                        hidden(actionLink("maximoMostrarMenos", "Mostrar menos")),
                                        hidden(p("Observação de maior valor.", id = "maximoTexto")),
                                        hidden(helpText(id="htMaximo", "Exemplo com os elementos gerados:"),
                                               verbatimTextOutput("exMaximo"))
                              )),
                       column(6, wellPanel(htmlOutput("varianciaTitle"),
                                           actionLink("varianciaMostrarMais", "Mostrar mais"),
                                           hidden(actionLink("varianciaMostrarMenos", "Mostrar menos")),
                                           HTML("<p id='varianciaTexto' class='shinyjs-hide'>Em alguns casos, medidas como média, moda e mediana podem não nos trazer 
                                        informações suficientes sobre o conjunto de observações. Afinal,
                                        as medidas anteriores são <strong>medidas de posição</strong>. É interessente
                                        analisarmos <strong>medidas de dispersão</strong>, como a variância. Esta representa
                                        o quão desviados os dados estão de sua média. Para isso, precisamos
                                        calcular a distância de cada elemento da média, somar o quadrado dessas
                                        distâncias e dividir pelo número de observações. </p>"),
                                           hidden(uiOutput("varianciaExplain")),
                                           hidden(helpText(id="htVariancia", "Exemplo com os elementos gerados:"),
                                                  verbatimTextOutput("exVariancia"))
                       ))
                     ),
                     fluidRow(
                       column(6, 
                              wellPanel(htmlOutput("dpTitle"),
                                        actionLink("dpMostrarMais", "Mostrar mais"),
                                        hidden(actionLink("dpMostrarMenos", "Mostrar menos")),
                                        hidden(p("O desvio padrão também é uma medida clássica de dispersão. 
                                                 Em termos de cálculo, ele se dá pela raiz quadrada da variância do 
                                                 conjunto. A vantagem que ele apresenta sobre a variância é a possibilidade 
                                                 de uma interpretação direta, uma vez que ele está na mesma unidade que a variável 
                                                 (kg, m, cm, etc.).", id = "dpTexto")),
                                        hidden(uiOutput("dpExplain")),
                                        hidden(helpText(id="htDp", "Exemplo com os elementos gerados:"),
                                               verbatimTextOutput("exDp"))
                              )),
                       column(6, wellPanel(htmlOutput("epTitle"),
                                           actionLink("epMostrarMais", "Mostrar mais"),
                                           hidden(actionLink("epMostrarMenos", "Mostrar menos")),
                                           hidden(p("Quando tratamos de população e amostras, o erro padrão é utilizado 
                                          como uma medida de variação entre a média da amostra e a média da 
                                          população. Ou seja, é uma medida que ajuda a calcular a confiabilidade 
                                          da amostra e da média amostral calculada. Para obtê-lo, basta dividir o 
                                          desvio padrão pela raiz quadrada do tamanho amostral.", id = "epTexto")),
                                           hidden(uiOutput("epExplain")),
                                           hidden(helpText(id="htEp", "Exemplo com os elementos gerados:"),
                                                  verbatimTextOutput("exEp"))
                       ))
                     ),
                     fluidRow(
                       column(6, 
                              wellPanel(htmlOutput("quantilTitle"),
                                        actionLink("quantilMostrarMais", "Mostrar mais"),
                                        hidden(actionLink("quantilMostrarMenos", "Mostrar menos")),
                                        hidden(p("Foi dito acima que a mediana é a medida que divide os valores do conjunto, 
                                       deixando metade deles abaixo e metade acima. Um quantil é uma generalização dessa 
                                       ideia, onde define-se uma proporção p, 0 < p < 1, cujo p-quantil divide o conjunto 
                                       deixando 100p% das observações abaixo dele.", id = "quantilTexto")),
                                        hidden(uiOutput("quantilExplain")),
                                        hidden(helpText(id="htQuantil", "Exemplo com os elementos gerados:"),
                                               verbatimTextOutput("exQuantil"))
                              )),
                       column(6, 
                              helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva."))
                     )
              )
            )
            
            
            
          )
  )