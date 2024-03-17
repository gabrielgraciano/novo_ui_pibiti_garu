inicio <-
  tabItem(tabName = 'inicio',
          fluidPage(
            fluidRow(
              HTML('<center><img src="images/garu_3.png"></center>')),
            fluidRow(column(6, wellPanel(
              h3(strong('Funcionalidades e métodos estatísticos')),
              p("O conteúdo do app está dividido pelos tópicos (clique para explorar):"),
              strong("1. Estatística Descritiva"), br(),
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"tipos_variaveis\")'>1.1 Tipos de Variáveis</a>")),
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"tabela_frequencias\")'>1.2 Distribuição de Frequências</a>")),
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"medidas_resumo\")'>1.3 Medidas Resumo </a>")),
              br(), 
              
              strong("2. Gráficos"), br(),
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"graf_qualitativa\")'>2.1. Gráficos para variáveis qualitativas</a>")),
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"graf_quantitativa\")'>2.2. Gráficos para variáveis quantitativas</a>")), 
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"graf_bidimensional\")'>2.3 Gráficos Bidimensionais</a>")),
              br(),  
              
              strong("3. Glossário"), br(),
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"glossario\")'>3. Glossário</a>")),br(),
              
              strong("4. Exercícios"), br(),
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"questionario\")'>4. Exercpicios</a>")),br(),
              
              strong("5. Exercícios Práticos"), br(),
              h5(HTML("<a style='color: light-blue; cursor: pointer;' onclick='fakeClick(\"paralisia\")'>5.1 Paralisia Cerebral</a>")),br(),
              
              HTML("<p><strong>Alunos participantes:</strong> João Henrique de Araujo Morais e Gabriel Graciano Dias</p>"),
              HTML("<p><strong>Orientadoras:</strong> Profa. Dra. Camila Bertini Martins e Téc. Dra. Alessandra A. S. Menezes</p>"),
              HTML("<p><strong>Contato: </strong><a> joao.morais@unifesp.br, gabriel.graciano@unifesp.br </a></p>"),
              helpText("GARU Estatística, 2024. Versão 1.0.5"),
              helpText("Última atualização: 17/03/2024")
            ),
            wellPanel(
              h3(strong("Fonte de Dados")),
              fluidRow(
                column(6, 
                       selectInput("dataSource", "Fonte de Dados", choices = c("Food choices (nativo)" = "nativo"))
                )
                
              ),
              downloadButton("botaoBaixarDados", "Download dos dados")
            )
            ), 
            column(6, wellPanel(
              h3(strong('Conjunto de dados')),
              h4(strong("Dados nativos")),
              HTML("<p>Nosso aplicativo tem um conjunto de dados nativo para que você possa acompanhar as definições junto aos 
dados, e aplicar os métodos estatísticos sobre ele. Esse conjunto se chama 'Food choices', é de domínio público, e foi extraído do <a href='https://www.kaggle.com'> Kaggle</a>. 
Ele consiste em nada mais do que um questionário aplicado na Universidade de Mercyhurst, Pennsilvânia, EUA. A base inclui informações de preferências gastronômicas, 
      nutrição, e relação com a saúde dos alunos. Ela pode ser visualizada <a href='https://www.kaggle.com/borapajo/food-choices'> aqui</a>. Logo abaixo temos uma breve 
explicação das variáveis que vocês podem encontrar e usar no aplicativo. </p>
</p>"),
      #        h4(strong("Use seus dados!")),
     #         HTML("<p>Nesta versão do GARU incluímos suporte para você carregar seu próprio conjunto de dados e utilizar os métodos 
     #    do aplicativo! Baste alterar a 'Fonte de dados' abaixo, e você poderá desfrutar de gráficos, cálculos de probabilidade e 
     #    testes sobre seus dados. É importante ressaltar que seu conjunto de dados pode não conter variáveis qualitativas ou quantitativas suficientes 
     #   para alguns dos nossos métodos. Você pode sempre alterar a <strong>fonte de dados</strong> de volta e utilizar o conjunto nativo."),
              h5("Significado das variáveis"),
              tableOutput("tabelaInformativa"),
              helpText("Vale informar que a variável 'Altura' e dados relacionados a exames de laboratório (HDL, LDL, etc.) não existiam na base de dados original e foram estimados a partir de 
             métodos de regressão treinados com a base de dados 'weight-height' e 'lab-data', também do Kaggle.")
            )
            )
            )
          )
  )
  



              
