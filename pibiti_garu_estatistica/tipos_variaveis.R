tipos_variaveis <- 
  tabItem(
    tabName = 'tipos_variaveis',
    fluidPage(
      title = "Tipos de Variáveis",
      p(strong(h3("Tipos de Variáveis"))),
      HTML("<p>Em um determinado estudo ou pesquisa, temos observações relacionadas a algum tipo de característica, o que
    chamamos de variáveis.
    Algumas variáveis representam um atributo ou caracteristica do indivíduo, como seu gênero, a região 
    onde mora, seu estado civil, etc. Estas são chamadas de <strong>variáveis qualitativas</strong>. Já outras variáveis são representadas por números provindos de uma 
    contagem ou mensuração. Estas são as <strong>variáveis quantitativas</strong>. </p>"),
      
      fluidRow(
        column(6,
               wellPanel(h4("Variáveis Qualitativas"),
                         HTML("<p>As variáveis qualitativas representam um atributo ou característica do indivíduo.
                       Ainda é possível realizar uma distinção dentro desse grupo: as variáveis qualitativas 
                       <strong>nominais</strong>, para quais não há nenhuma possível hierarquia ou ordenação entre
suas possíveis realizações, e as variáveis 
                       qualitativas <strong>ordinais</strong>, para quais existe uma ordenação entre suas categorias. 
                       Para exemplificar, a variável sexo é uma variável qualitativa nominal, pois não há
                       uma ordem entre os valores 'feminino' e 'masculino', e a variável grau de instrução é um
                       exemplo de variável qualitativa ordinal, pois há uma ordem entre seus possíveis valores: 
                       ensino primário, ensino fundamental, ensino médio, etc.</p>"))),
        column(6, 
               wellPanel(h4("Variáveis Quantitativas"),
                         HTML("<p>Como indicado pelo nome, as variáveis quantitativas representam uma quantidade. 
                       Essas também podem ser classificadas em dois tipos: as variáveis quantitativas 
                       <strong>discretas</strong>, geralmente provenientes de uma contagem e cujos possíveis valores podem ser listados em um conjunto finito de 
                       números; e as variáveis quantitativas <strong>contínuas</strong>, provenientes de uma mensuração, e que podem assumir qualquer valor 
                       real dentro de um intervalo. Um exemplo da primeira é o número de filhos que uma pessoa tem (0, 1, 2, 3, ...), 
                       e exemplos clássicos da segunda são altura e peso.</p>")))
      ),
      
      fluidRow(
        column(6,
               tags$head(tags$style(
                 type="text/css",
                 "#imagemTiposVariaveis img {max-width: 100%; width: 100%; height: auto}"
               )),
               
               imageOutput("imagemTiposVariaveis")
               #HTML('<center><img src="images/tipos_variaveis.png" filetype="image/png" width="400"></center>')
        ),
        column(6, 
               h4("Tabela com variáveis exemplo"),
               tableOutput("tabelaExemploVariaveis"))
      ),
      
      fluidRow(
        column(6),
        column(6, helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva.")))
      
      
    )
  )
    
    