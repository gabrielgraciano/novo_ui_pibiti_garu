distr_prob <- 
  tabItem(tabName = 'prob_cond',
          sidebarLayout(
            sidebarPanel(
              useShinyjs(),
              h3(strong("Distribuições de Probabilidade")),
              p("Distribuições de Probabilidade são vistas como medida de comportamento de uma variável aleatória, discreta 
      ou contínua."),
              selectInput("distribuicao", "Distribuição", choices = c("Bernoulli" = "bernoulli",
                                                                      "Binomial" = "binomial", 
                                                                      "Poisson" = "poisson",
                                                                      "Normal" = "normal",
                                                                      "Exponencial" = "exp",
                                                                      "Qui-Quadrado" = "qui",
                                                                      "T de Student" = "t")),
              conditionalPanel("input.distribuicao == 'bernoulli'", 
                               p("Alguns eventos apresentam uma determinada característica: Tem possibilidades binárias de 
                       resultados. Por exemplo: sucesso ou fracasso, 0 ou 1, sim ou não, etc. Estes eventos, com 
                       duas possibilidades de resultados, são chamados de Eventos de Bernoulli. Dizemos que a probabilidade de 
                       que se obtenha 'sucesso' em um desses eventos é p. Logo, como o único outro possíve resultado é o fracasso, 
                       suas probabilidade é de 1 - p."),
                               helpText("Exemplo: Jogar uma moeda é um evento de Bernoulli, pois tem dois resultados possíveis: cara ou 
                              coroa. Se ela for não-viciada, ela tem p = 0.5."),
                               p("Logo, pode-se definir variáveis que assumem dois valores possíveis: 0 ou 1. Essa variável terá uma distribuição 
                       chamada Distribuição de Bernoulli, que pode ser observada ao lado."),
                               sliderInput("bernoulli_p", "p", 0, 1, value = 0.5, step = 0.1)
              ),
              conditionalPanel("input.distribuicao == 'binomial'", 
                               p("A distribuição binomial é uma distribuição de probabilidade discreta onde ocorrem 
                       n eventos sendo que cada um deles é um evento de sucesso ou fracasso, com probabilidade 
                       p de sucesso e 1-p de fracasso. A variável de interesse, k, é a quantidade de sucessos 
                       dentre essas n tentativas."),
                               helpText("Exemplo: Ao jogar uma moeda 20 vezes, deseja-se saber qual a probabilidade de 
                              obter 8 caras. Temos n=20, p=0.5 e k=8."),
                               p("Assim, temos, para cada possível k:"),
                               withMathJax("$$f(k; n, p) = {\\binom{n}{k}} p^{k} (1-p)^{n-k}$$"),
                               p("Observe a distribuição dessas probabilidades ao lado, com os seguintes parâmetros:"),
                               sliderInput("binom_p", "p", 0, 1, value = 0.5, step = 0.1),
                               sliderInput("binom_n", "n", 1, 100, value = 20, step = 1)
              ),
              conditionalPanel("input.distribuicao == 'poisson'", 
                               p("A distribuição de Poisson de uma variável aleatória discreta 
                        descreve a probabilidade de uma determinada quantidade de eventos 
                       ocorrer em um período de tempo."),
                               helpText("Por exemplo, se sabe-se o número de 
                       acidentes que ocorrem por hora em média em uma determinada avenida, qual 
                       a probabilidade de ocorrerem k acidentes em uma hora?"),
                               p("Assim, temos:"),
                               withMathJax("$$P(x=k) = \\frac{e^{-u} \\mu^{k}}{k!}$$"),
                               p("Observe a distribuição dessas probabilidades ao lado, com os seguintes parâmetros:"),
                               sliderInput("poisson_u", "Média", 0, 20, value = 4, step = 1),
                               sliderInput("poisson_minmax", "Intervalo", 0, 40, value = c(0, 10), step = 1)
              ), 
              conditionalPanel("input.distribuicao == 'normal'", 
                               p("A distribuição normal para variáveis contínuas é uma das mais importantes, visto que 
                       diversos fenômenos do mundo podem ser representados por uma distribuição normal. Nela, 
                       vê-se há uma distribuição em forma de sino, unimodal e simétrica em relação à sua 
                       média. Nessa distribuição, consta que 68,26% dos valores estão distribuídos em até 
                       1 desvio padrão da média. 95,44% em até 2 desvios e 99,73% em até 3."),
                               p("Sua função densidade de probabilidade é definida por:"),
                               withMathJax("$$f(x, \\mu, \\sigma) = \\frac{1}{\\sigma \\sqrt{2\\pi}}e^{-\\frac{1}{2}\\frac{(x - \\mu)^2}{\\sigma^{2}}}$$"),
                               p("Observe essa distribuição ao lado, com os seguintes parâmetros:"),
                               numericInput("normal_media", "Média", value = 0),
                               numericInput("normal_dp", "Desvio Padrão", value = 1, min = 1, step = 0.5),
                               p("Probabilidade de: "),
                               uiOutput("normal_params"),
                               actionButton("normal_refresh", "Atualizar", icon=icon("refresh"))
                               
                               
              ),
              conditionalPanel("input.distribuicao == 'exp'", 
                               p("A distribuição exponencial se relaciona bastante com a distribuição de Poisson. Enquanto em 
                       Poisson determinávamos o número de ocorrências (discreto) que poderíamos observar em um intervalo de 
                       tempo, a exponencial mostra qual a probabilidade do tempo observado entre dois acontecimentos seguidos esteja 
                       dentro de um intervalo."),
                               helpText("Ou seja, em vez de nos perguntar qual a chance de termos 13 acidentes em uma hora (como faríamos 
                              com um modelo de Poisson), agora perguntamos qual será o tempo entre dois acidentes consecutivos."),
                               p("Temos:"),
                               withMathJax("$$f(x; \\beta) = \\frac{1}{\\beta} e^{\\frac{-t}{\\beta}}$$"),
                               p("Observe a distribuição dessas probabilidades ao lado, com os seguintes parâmetros:"),
                               sliderInput("exp_b", "1/β", 0, 10, value = 2, step = 0.1),
                               sliderInput("exp_val", "Intervalo", 0, 5, value = c(0, 1), step = 0.2)
                               
              ),
              conditionalPanel("input.distribuicao == 'qui'", 
                               p("A distribuição qui-quadrado é a distribuição da soma dos quadrados dos desvios normais padrão. Essa distribuição tem um 
                     parâmetro: os graus de liberdade. Esses graus de liberdade nada mais são do que o número de desvios normais padrão sendo somados. 
                   Uma distribuição qui-quadrado com um grau de liberdade, denotada por X^2(1), é simplesmente uma distribuição de desvios normais padrão ao quadrado."),
                               helpText("Por exemplo, separamos os resultados de um teste, que estão distribuídos normalmente, elevamos cada resultado ao quadrado e somamos. Qual a probabilidade 
                            dessa soma ser maior que 6? Isso a distribuição qui-quadrado tenta responder. Neste caso, teríamos 2 graus de liberdade."),
                               p("Observe a distribuição ao lado, com os seguintes parâmetros:"),
                               sliderInput("qui_x", "Graus de Liberdade", 0, 10, value = 4, step = 1),
                               sliderInput("qui_values", "Intervalo", 0, 16, value = c(0, 6), step = 0.5)
                               
              ),
              conditionalPanel("input.distribuicao == 't'", 
                               p("A distribuição t de Student parece bastante com a normal, porém com caudas mais largas. Assim como a distribuição Qui-quadrado, 
                     o único parâmetro que define a distribuição t de Student são os graus de liberdade. Quanto maior for esse valor, mais parecida com a 
                     normal essa distribuição será. \nSuponha que temos uma variável Z de distribuição normal já na forma padrão, e outra variável com distribuição Qui-quadrado V 
                     com v graus de liberdade. Considerando Z e V independentes, a distribuição da variável t será:"),
                               withMathJax("t = $$\\frac{Z}{\\sqrt{\\frac{V}{v}}}$$"),
                               p("A distribuição t de Student é muito usada quando se trabalha com inferência, onde estima-se parâmetros da população a partir de amostras. O tamanho de 
                     amostra menos um será o número dos graus de liberdade da distribuição."),
                               p("Observe a distribuição ao lado, com os seguintes parâmetros:"),
                               sliderInput("t_gl", "Graus de Liberdade", 0, 15, value = 4, step = 1)
                               
              )
            ),
            mainPanel(
              plotOutput("distribuicao"),
              conditionalPanel("input.distribuicao != 'normal'", 
                               tableOutput("distrTable")
              ),
              conditionalPanel("input.distribuicao == 'normal'", 
                               verbatimTextOutput("normalExemplo")
              ),
              conditionalPanel("input.distribuicao == 'exp'",
                               uiOutput("expExemplo"))
              
            )
          )
  )