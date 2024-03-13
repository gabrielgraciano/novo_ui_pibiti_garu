prob_cond <- 
  tabItem(tabName = 'prob_cond',
          fluidPage(
            h3(strong("Probabilidade Condicional")),
            fluidRow(column(6, 
                            wellPanel(HTML("<p>A probabilidade condicional refere-se à probabilidade de um evento A acontecer <strong>dado que 
                    um evento B já aconteceu</strong>. Ou seja, agora em vez de calcular a probabilidade de uma variável assumir 
                                 um valor dentre todas as ocorrências possíveis, analisamos apenas entre as ocorrências em que uma outra 
                                 variável já assumiu um outro valor. 
                                 Exemplo: Em vez de calcular a probabilidade de um indivíduo praticar esportes, podemos 
                                 calcular a probabilidade de um indivíduo praticar esportes <strong>dado que</strong>
                                 esse indivíduo pratica exercícios físicos.</p>"))
            ),
            column(6, align = "center",
                   uiOutput("tabelaCondUI")
            )
            
            ),
            fluidRow(column(6,
                            column(6, 
                                   wellPanel(uiOutput("selectProbCondVar"))
                            ),
                            column(6, align = "center", wellPanel(withMathJax("P(A | B) = $$\\frac{P(A ∩ B)}{P(B)} $$")))
                            
                            
            ),
            column(6,
                   h4(strong("Cálculo")),
                   verbatimTextOutput("caixaContaProbCond"),
                   p(HTML("A fórmula da <a style='color: blue; text-decoration: underline; cursor: pointer;' onclick='fakeClick(\"glossario\")'>média</a>"))
            )
            )
            
            
          )
  )
  