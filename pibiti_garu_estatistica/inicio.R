inicio <-
  tabItem(tabName = 'inicio',
          fluidPage(
            fluidRow(
              HTML('<center><img src="images/garu_3.png"></center>')),
            fluidRow(column(6, wellPanel(
              h3(strong('Funcionalidades e métodos estatísticos')),
              p("O conteúdo do app está dividido pelos tópicos (clique para explorar):"),
              strong("1. Estatística Descritiva"), br(),
              actionLink("linkTiposVariaveis", "1.1. Tipos de Variáveis"), br(),
              actionLink("linkDistrFreq", "1.2. Distribuição de Frequências"), br(),
              actionLink("linkMedidasResumo", "1.3. Medidas Resumo"), br(),
              br(), 
              strong("2. Gráficos"), br(),
              actionLink("linkGrafQual", "2.1. Gráficos para variáveis qualitativas"), br(),
              actionLink("linkGrafQuant", "2.2. Gráficos para variáveis quantitativas"), br(), 
              actionLink("linkGrafBi", "2.3. Gráficos bidimensionais"), br(), 
              br(),  
              strong("3. Probabilidade"), br(),
              actionLink("linkDefProb", "3.1. Definição de Probabilidade"), br(),
              actionLink("linkProbCond", "3.2. Probabilidade Condicional"), br(), 
              actionLink("linkDistrProb", "3.3 Distribuições de Probabilidade"), br(),
              br(), 
              strong("4. Inferência"), br(),
              actionLink("linkTesteT1", "4.1. Teste T para uma amostra"), br(),
              actionLink("linkTesteT2", "4.2. Teste T para duas amostras dependentes"), br(),
              actionLink("linkTesteQui", "4.3. Teste qui-quadrado de independência"), br(),
              actionLink("linkTesteCorr", "4.4. Teste de Correlação de Pearson"), br(), 
              br(), br(),
              HTML("<p><strong>Aluno participante:</strong> João Henrique de Araujo Morais</p>"),
              HTML("<p><strong>Professora Orientadora:</strong> Profa. Dra. Camila Bertini Martins</p>"),
              HTML("<p><strong>Contato: </strong><a> joao.morais@unifesp.br </a></p>"),
              helpText("GARU Estatística, 2019. Versão 1.0.4"),
              helpText("Última atualização: 25/07/2019")
            ),
            wellPanel(
              h3(strong("Fonte de Dados")),
              fluidRow(
                column(6, 
                       selectInput("dataSource", "Fonte de Dados", choices = c("Food choices (nativo)" = "nativo", "Dados do usuário"))
                ),
                column(6, 
                       strong("Entrada de Dados"),
                       uiOutput("statusDadosUsuario"),
                       actionButton("selecionarDados", "Selecionar...", icon = icon("upload")),
                       checkboxInput("selecionarDadosHeader", "Possui cabeçalho", value = TRUE)
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
              h4(strong("Use seus dados!")),
              HTML("<p>Nesta versão do GARU incluímos suporte para você carregar seu próprio conjunto de dados e utilizar os métodos 
         do aplicativo! Baste alterar a 'Fonte de dados' abaixo, e você poderá desfrutar de gráficos, cálculos de probabilidade e 
         testes sobre seus dados. É importante ressaltar que seu conjunto de dados pode não conter variáveis qualitativas ou quantitativas suficientes 
         para alguns dos nossos métodos. Você pode sempre alterar a <strong>fonte de dados</strong> de volta e utilizar o conjunto nativo."),
              h5("Significado das variáveis"),
              tableOutput("tabelaInformativa"),
              helpText("Vale informar que a variável 'Altura' e dados relacionados a exames de laboratório (HDL, LDL, etc.) não existiam na base de dados original e foram estimados a partir de 
             métodos de regressão treinados com a base de dados 'weight-height' e 'lab-data', também do Kaggle.")
            )
            )
            )
          )
  )
  






              