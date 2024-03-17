library(shiny)
library(ggplot2)
library(shinyjs)
library(cowplot)
library(dplyr)
library(reshape2)
elementos <- sort(round(runif(20, min = 1, max = 10)))

floorN <- function(x, N){ 
  N*floor(x/N) 
}

ceilN <- function(x, N) {
  N*ceiling(x/N)
}

values <- reactiveValues()
values$showMedia <- FALSE
normal_params <- reactiveValues()
normal_params$u <- 0
normal_params$dp <- 1
normal_params$min <- 1
normal_params$max <- 4
normal_params$sit <- 1

testeT1_params <- reactiveValues()
testeT1_params$u <- 0
testeT1_params$dp <- 1
testeT1_params$X <- 0.18
testeT1_params$n <- 10
testeT1_params$alpha <- 0.05

testeT2_params <- reactiveValues()
testeT2_params$n <- 12
testeT2_params$u1 <- 10
testeT2_params$u2 <- 9
testeT2_params$dp1 <- 2.2
testeT2_params$dp2 <- 2.2
p1 <- rnorm(12, 10, 2.2)
p2 <- rnorm(12, 9, 2.2)
testeT2_params$p1 <- p1
testeT2_params$p2 <- p2
testeT2_params$D <- p1 - p2

testeQui_params <- reactiveValues()
testeQui_params$matrix <- rbind(c(48, 8, 56), c(30, 21, 51), c(78, 29, 107))
testeQui_params$expected <- rbind(c(41, 15, 56), c(37, 14, 51), c(78, 29, 107))
testeQui_params$colnames <- c("Melhora", "Não", "Total")
testeQui_params$rownames <- c("Droga", "Placebo", "Total")
testeQui_params$chi <- 9.3

userData <- reactiveValues(data=data, custom=FALSE, factors=native_factors, 
                           numbers = native_numbers, shortLevels = native_shortLevels,
                           continuous = native_continuous, uploaded = FALSE)

loaded <- reactiveValues()


blue <- c("#A1D2CE", "#8AD1CB", "#78CAD2", "#62A8AC", "#4CA6AA", "#5497A7", "#50858B", "#254A4F")
colorful <- c("#20BFB2", "#63BE76", "#A8B245", "#E49C45", "#FE8A77", "#F48BAE", "#BCA0D8", "#64B5DA", "#c9e5f2")
medium_cyan <- '#3db9bf'
  dark_cyan <- '#077e84'
    medium_ocre <- "#bf9f56"
      dark_orange <- "#8c6a1c"
        brown <- "#4c380c"
          red_nail <- '#ce4b37'
            red_broken_nail <- '#300802'
              
            
            
            
            getmode <- function(v) {
              uv <- unique(v)
              tab <- tabulate(match(v, uv))
              uv[tab == max(tab)]
            }
            
            vectorToString <- function(v) {
              string <- v[1]
              if (length(v) > 1) {
                string <- "{"
                for (i in (1:length(v))) {
                  if (i != length(v)) {
                    string <- paste0(string, v[i], ", ")
                  }
                  else {
                    string <- paste0(string, v[i], "}")
                  }
                }
              }
              return (string)
            }
            
            function(input, output, session) {
              
              # output$imagemLogo <- renderImage({
              #   return(list(
              #     src = "www/images/garu_3.png",
              #     filetype = "image/png",
              #     alt = "Garu"
              #   ))
              # }, deleteFile = FALSE)
              
              output$statusDadosUsuario <- renderUI({
                input$modalOk
                #print(head(userData$dados))
                texto <- "<p>Nenhuma base de dados selecionada.</p>"
                if (!is.null(userData$data)) {
                  texto <- paste0("<p>Base de dados selecionada: <strong>", userData$nome, "</strong></p>")
                }
                HTML(texto)
              })
              
              output$botaoBaixarDados <- downloadHandler(
                filename = function() {
                  paste("dados_saude_alimentacao", ".csv", sep="")
                },
                content = function(file) {
                  write.csv(mtcars, file)
                }
              )
              
              'userDataDialog <- function(failed = FALSE) {
                modalDialog(
                  fileInput("userDataUploader", "Escolha arquivo CSV",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  span(
                    if (failed) {
                      div(tags$b("Arquivo escolhido inválido. Por favor, escolha outro.", style = "color: red;"))
                    }
                  ), 
                  footer = tagList(
                    modalButton("Cancelar"),
                    actionButton("modalOk", "Ok")
                  )
                )
              }
              
              checaDialog <- reactive({
                # Check that data object exists and is data frame.
                tryCatch({
                  userData$data <- read.csv(input$userDataUploader$datapath, header = input$selecionarDadosHeader)
                  
                  userData$nome <- input$userDataUploader$name
                  if (!is.null(userData$data$X)) {
                    userData$data$X <- NULL
                  }
                  userData$factors <- sapply(userData$data, is.factor)
                  userData$numbers <- sapply(userData$data, is.numeric) | sapply(userData$data, is.integer)
                  userData$shortLevels <- sapply(userData$data, nlevels) < 8 & userData$factors
                  userData$continuous <- sapply(userData$data, is.numeric)
                  userData$custom <- TRUE
                  userData$uploaded <- TRUE
                  loaded$data <- userData$data
                  loaded$nome <- userData$nome
                  loaded$factors <- userData$factors
                  loaded$shortLevels <- userData$shortLevels
                  loaded$continuous <- userData$continuous
                  updateSelectInput(session, "varGrafQuant")
                }, error = function(e) {updateSelectInput(session, "dataSource", selected = "nativo")
                  showNotification("Não foi possível importar seu conjunto de dados. Verifique 
                       ou tente outro arquivo.",
                                   duration = 3, 
                                   closeButton = TRUE,
                                   type = "error")})
                if (!is.null(userData$data) && is.data.frame(userData$data)) {
                  removeModal()
                } else {
                  showModal(userDataDialog(failed = TRUE))
                }
              })
              
              observeEvent(input$modalOk, {
                print(input$userDataUploader)
                checaDialog()
                print(head(userData$data))
              }
              
              )'
              
              observeEvent(input$selecionarDados,
                           showModal(userDataDialog())
              )
              
              observeEvent(input$dataSource, {
                if (input$dataSource == "Dados do usuário") {
                  if (userData$uploaded) {
                    userData$data <- loaded$data
                    userData$factors <- loaded$factors
                    userData$numbers <- loaded$numbers
                    userData$shortLevels <- loaded$shortLevels
                    userData$continuous <- loaded$continuous
                    userData$custom <- TRUE
                  } else {
                    showModal(userDataDialog())
                  }
                  
                }
                
                if (input$dataSource == "nativo") {
                  userData$data <- data
                  userData$custom <- FALSE
                  userData$factors <- native_factors
                  userData$numbers <- native_numbers
                  userData$shortLevels <- native_shortLevels
                }
              })
              
              
              output$tabelaExemploVariaveis <- renderTable(example_dataframe, bordered = TRUE)
              
              output$imagemTiposVariaveis <- renderImage({
                return(list(
                  src = "www/images/tipos_variaveis.png",
                  filetype = "image/png",
                  alt = "Tipos de Variaveis"
                ))
              }, deleteFile = FALSE)
              
              output$calcFrequencia <- renderText({
                "Frequência: Número de ocorrências de determinado valor"
              })
              
              output$calcProporcao <- renderText({
                "Proporção: Frequência dividida pelo número total de ocorrências"
              })
              
              output$tabelaFreqRelacionamento <- renderTable(tab_frequencia_relacionamento,
                                                             bordered = TRUE, striped = TRUE)
              
              output$tabelaFreqAnoLetivo <- renderTable(tab_frequencia_ano_letivo,
                                                        bordered = TRUE, striped = TRUE)
              
              output$tabelaFreqPeso <- renderTable(tab_frequencia_peso,
                                                   bordered = TRUE, striped = TRUE)
              
              observeEvent(input$geraElementos, {
                elementos <<- sort(round(runif(input$slider_qtd_elementos, min = input$slider_min_max_valor[1], max = input$slider_min_max_valor[2])))
              })
              
              getElementos <- reactive({
                return(elementos)
              })
              
              output$printElementos <- renderText({
                input$geraElementos
                elementos
              })
              
              #Média
              
              observeEvent(input$mediaMostrarMais, {
                shinyjs::hide("mediaMostrarMais")
                shinyjs::show("mediaMostrarMenos")
                shinyjs::show("mediaTexto")
                shinyjs::show("mediaExplain")
                shinyjs::show("htMedia")
                shinyjs::show("exMedia")
              })
              
              observeEvent(input$mediaMostrarMenos, {
                shinyjs::hide("mediaMostrarMenos")
                shinyjs::show("mediaMostrarMais")
                shinyjs::hide("mediaTexto")
                shinyjs::hide("mediaExplain")
                shinyjs::hide("htMedia")
                shinyjs::hide("exMedia")
              })
              
              output$mediaTitle <- renderText({
                input$geraElementos
                paste0("<h4> Média: <strong> ", round(mean(elementos), 2), "</strong> </h4>")
              })
              
              output$showMedia <- reactive({
                return(values$showMedia)
              })
              
              output$mediaExplain <- renderUI({
                withMathJax(helpText("Média:  $$\\frac{1}{n} \\sum_{i=1}^{n} x_{i}$$"))
              })
              
              output$exMedia <- renderText({
                input$geraElementos
                text <- "Média = ("
                for (i in (1:length(elementos))) {
                  if (i != length(elementos)) {
                    text <- paste0(text, elementos[i], " + ")
                  } else {
                    text <- paste0(text, elementos[i])
                  }
                  
                }
                
                text <- paste0(text, ")/", length(elementos), " = ", round(mean(elementos), 4))
                text
              })
              
              #Mediana
              
              observeEvent(input$medianaMostrarMais, {
                shinyjs::hide("medianaMostrarMais")
                shinyjs::show("medianaMostrarMenos")
                shinyjs::show("medianaTexto")
                shinyjs::show("medianaExplain")
                shinyjs::show("htMediana")
                shinyjs::show("exMediana")
              })
              
              observeEvent(input$medianaMostrarMenos, {
                shinyjs::hide("medianaMostrarMenos")
                shinyjs::show("medianaMostrarMais")
                shinyjs::hide("medianaTexto")
                shinyjs::hide("medianaExplain")
                shinyjs::hide("htMediana")
                shinyjs::hide("exMediana")
              })
              
              output$medianaTitle <- renderText({
                input$geraElementos
                paste0("<h4> Mediana: <strong> ", round(median(elementos), 2), "</strong> </h4>")
              })
              
              output$medianaExplain <- renderUI({
                withMathJax(helpText("Mediana (n ímpar): $$x_{\\frac{n+1}{2}}$$"),
                            helpText("Mediana (n par): $$\\frac{x_{\\frac{n}{2}} + x_{\\frac{n}{2} + 1}}{2}$$")
                )
              })
              
              output$exMediana <- renderText({
                input$geraElementos
                text <- "Mediana = "
                if (length(elementos)%%2 == 1) {
                  text <- paste0(text, median(elementos))
                } else {
                  text <- paste0(text, "(", elementos[length(elementos)/2], " + ", elementos[(length(elementos)/2) + 1], ")/2 = ", round(median(elementos), 4))
                }
                text
              })
              
              #Moda
              
              observeEvent(input$modaMostrarMais, {
                shinyjs::hide("modaMostrarMais")
                shinyjs::show("modaMostrarMenos")
                shinyjs::show("modaTexto")
                shinyjs::show("modaExplain")
                shinyjs::show("htModa")
                shinyjs::show("exModa")
              })
              
              observeEvent(input$modaMostrarMenos, {
                shinyjs::hide("modaMostrarMenos")
                shinyjs::show("modaMostrarMais")
                shinyjs::hide("modaTexto")
                shinyjs::hide("modaExplain")
                shinyjs::hide("htModa")
                shinyjs::hide("exModa")
              })
              
              output$modaTitle <- renderText({
                input$geraElementos
                paste0("<h4> Moda: <strong> ", vectorToString(getmode(elementos)), "</strong> </h4>")
              })
              
              output$exModa <- renderText({
                input$geraElementos
                text <- paste0("Moda = ", vectorToString(getmode(elementos)))
                text
              })
              
              #Mínimo
              
              observeEvent(input$minimoMostrarMais, {
                shinyjs::hide("minimoMostrarMais")
                shinyjs::show("minimoMostrarMenos")
                shinyjs::show("minimoTexto")
                shinyjs::show("minimoExplain")
                shinyjs::show("htMinimo")
                shinyjs::show("exMinimo")
              })
              
              observeEvent(input$minimoMostrarMenos, {
                shinyjs::hide("minimoMostrarMenos")
                shinyjs::show("minimoMostrarMais")
                shinyjs::hide("minimoTexto")
                shinyjs::hide("minimoExplain")
                shinyjs::hide("htMinimo")
                shinyjs::hide("exMinimo")
              })
              
              output$minimoTitle <- renderText({
                input$geraElementos
                paste0("<h4> Mínimo: <strong> ", min(elementos), "</strong> </h4>")
              })
              
              output$exMinimo <- renderText({
                input$geraElementos
                text <- paste0("Mínimo = ", min(elementos))
                text
              })
              
              #Máximo
              
              observeEvent(input$maximoMostrarMais, {
                shinyjs::hide("maximoMostrarMais")
                shinyjs::show("maximoMostrarMenos")
                shinyjs::show("maximoTexto")
                shinyjs::show("maximoExplain")
                shinyjs::show("htMaximo")
                shinyjs::show("exMaximo")
              })
              
              observeEvent(input$maximoMostrarMenos, {
                shinyjs::hide("maximoMostrarMenos")
                shinyjs::show("maximoMostrarMais")
                shinyjs::hide("maximoTexto")
                shinyjs::hide("maximoExplain")
                shinyjs::hide("htMaximo")
                shinyjs::hide("exMaximo")
              })
              
              output$maximoTitle <- renderText({
                input$geraElementos
                paste0("<h4> Máximo: <strong> ", max(elementos), "</strong> </h4>")
              })
              
              output$exMaximo <- renderText({
                input$geraElementos
                text <- paste0("Máximo = ", max(elementos))
                text
              })
              
              #Variância
              
              observeEvent(input$varianciaMostrarMais, {
                shinyjs::hide("varianciaMostrarMais")
                shinyjs::show("varianciaMostrarMenos")
                shinyjs::show("varianciaTexto")
                shinyjs::show("varianciaExplain")
                shinyjs::show("htVariancia")
                shinyjs::show("exVariancia")
              })
              
              observeEvent(input$varianciaMostrarMenos, {
                shinyjs::hide("varianciaMostrarMenos")
                shinyjs::show("varianciaMostrarMais")
                shinyjs::hide("varianciaTexto")
                shinyjs::hide("varianciaExplain")
                shinyjs::hide("htVariancia")
                shinyjs::hide("exVariancia")
              })
              
              output$varianciaTitle <- renderText({
                input$geraElementos
                paste0("<h4> Variância: <strong> ", round(var(elementos), 2), "</strong> </h4>")
              })
              
              output$varianciaExplain <- renderUI({
                withMathJax(helpText("Variância: $$\\frac{\\sum_{i=1}^{n} (x_{i} - \\bar{x})^{2}}{n}$$")
                )
              })
              
              output$exVariancia <- renderText({
                input$geraElementos
                text <- "Variância = ("
                for (i in (1:length(elementos))) {
                  if (i != length(elementos)) {
                    text <- paste0(text, "(",elementos[i], " - ", round(mean(elementos), 4), ")^2 + ")
                  }
                  else {
                    text <- paste0(text, "(",elementos[i], " - ", round(mean(elementos), 4), ")^2)/", length(elementos), " = ", round(var(elementos), 4))
                  }
                }
                text
              })
              
              #Desvio Padrão
              
              observeEvent(input$dpMostrarMais, {
                shinyjs::hide("dpMostrarMais")
                shinyjs::show("dpMostrarMenos")
                shinyjs::show("dpTexto")
                shinyjs::show("dpExplain")
                shinyjs::show("htDp")
                shinyjs::show("exDp")
              })
              
              observeEvent(input$dpMostrarMenos, {
                shinyjs::hide("dpMostrarMenos")
                shinyjs::show("dpMostrarMais")
                shinyjs::hide("dpTexto")
                shinyjs::hide("dpExplain")
                shinyjs::hide("htDp")
                shinyjs::hide("exDp")
              })
              
              output$dpTitle <- renderText({
                input$geraElementos
                paste0("<h4> Desvio Padrão: <strong> ", round(sd(elementos), 2), "</strong> </h4>")
              })
              
              output$dpExplain <- renderUI({
                withMathJax(helpText("Desvio Padrão: $$\\sqrt{var(X)}$$")
                )
              })
              
              output$exDp <- renderText({
                input$geraElementos
                text <- paste0("Desvio Padrão = sqrt(", round(var(elementos), 2), ") = ", round(sd(elementos), 4))
                text
              })
              
              #Erro Padrão
              
              observeEvent(input$epMostrarMais, {
                shinyjs::hide("epMostrarMais")
                shinyjs::show("epMostrarMenos")
                shinyjs::show("epTexto")
                shinyjs::show("epExplain")
                shinyjs::show("htEp")
                shinyjs::show("exEp")
              })
              
              observeEvent(input$epMostrarMenos, {
                shinyjs::hide("epMostrarMenos")
                shinyjs::show("epMostrarMais")
                shinyjs::hide("epTexto")
                shinyjs::hide("epExplain")
                shinyjs::hide("htEp")
                shinyjs::hide("exEp")
              })
              
              output$epTitle <- renderText({
                paste0("<h4> Erro Padrão </h4>")
              })
              
              output$epExplain <- renderUI({
                withMathJax(helpText("Erro Padrão: $$\\frac{\\sigma}{\\sqrt{n}}$$")
                )
              })
              
              output$exEp <- renderText({
                input$geraElementos
                tam_amostra <- ceiling(sqrt(length(elementos)))
                amostra <- sample(elementos, tam_amostra, replace = FALSE)
                text <- paste0("Separando uma amostra exemplo dos elementos gerados: \n", vectorToString(amostra),
                               "\nCalcula-se o seu desvio padrão = sqrt(var(", vectorToString(amostra), ")) = ", 
                               round(sd(amostra), 4), "\nE divide-se pela raiz do tamanho da amostra: \n", 
                               round(sd(amostra), 4), "/sqrt(", tam_amostra, ") = ", round((sd(amostra)/sqrt(tam_amostra)), 4))
                text
              })
              
              #Quantis
              
              observeEvent(input$quantilMostrarMais, {
                shinyjs::hide("quantilMostrarMais")
                shinyjs::show("quantilMostrarMenos")
                shinyjs::show("quantilTexto")
                shinyjs::show("quantilExplain")
                shinyjs::show("htQuantil")
                shinyjs::show("exQuantil")
              })
              
              observeEvent(input$quantilMostrarMenos, {
                shinyjs::hide("quantilMostrarMenos")
                shinyjs::show("quantilMostrarMais")
                shinyjs::hide("quantilTexto")
                shinyjs::hide("quantilExplain")
                shinyjs::hide("htQuantil")
                shinyjs::hide("exQuantil")
              })
              
              output$quantilTitle <- renderText({
                paste0("<h4> Quantis </h4>")
              })
              
              output$exQuantil <- renderText({
                input$geraElementos
                isEven <- (length(elementos)%%2 == 0)
                text <- paste0("Temos: ", vectorToString(elementos), "\n")
                
                if (isEven) {
                  text <- paste0(text, 
                                 paste0("0.25 quantil = ", elementos[ceiling(0.25*length(elementos))], ", o "),
                                 ceiling(0.25*length(elementos)), "º elemento, pois divide 25% dos elementos abaixo dele.\n",
                                 "0.5 quantil (mediana) = (", elementos[length(elementos)/2] , " + ", elementos[(length(elementos)/2) + 1], 
                                 ")/2 = ", median(elementos), "\n",
                                 "0.75 quantil = ", elementos[ceiling(0.75*length(elementos))], ", o ",
                                 ceiling(0.75*length(elementos)), "º elemento, pois divide 75% dos elementos abaixo dele.\n"
                  )
                } else {
                  text <- paste0(text, 
                                 "0.25 quantil = (",  elementos[floor(0.25*length(elementos))] , " + ", elementos[ceiling(0.25*length(elementos))], 
                                 ")/2 = ", as.vector(quantile(elementos))[2], "\n",
                                 "0.5 quantil (mediana) = ", elementos[ceiling(0.5*length(elementos))], ", o ",
                                 ceiling(0.5*length(elementos)), "º elemento, pois divide metade dos elementos abaixo dele.\n",
                                 "0.75 quantil = (",  elementos[floor(0.75*length(elementos))] , " + ", elementos[ceiling(0.75*length(elementos))],
                                 ")/2 = ", as.vector(quantile(elementos))[4], "\n"
                  )
                }
                
                text
              })
              
              
              
              
              
              
              output$graficoElementos <- renderPlot({
                input$geraElementos
                tab <- as.data.frame(table(elementos))
                tab$elementos <- as.numeric(as.character(tab$elementos))
                mean <- round(mean(elementos), 2)
                mode <- getmode(elementos)
                tab$isMode <- ifelse(tab$elementos %in% mode, TRUE, FALSE)
                ggplot(data=tab, aes(x=elementos, y=Freq, fill = isMode)) + 
                  geom_bar(stat="identity", position=position_dodge(), color = "black", width = 0.9) +
                  geom_text(aes(label=Freq), vjust=-0.5, size=3.5)+
                  annotate("segment", x = mean, xend = mean, y = 0, yend = max(tab$Freq) + 0.5, colour = "black", size=1) +
                  #geom_segment(aes(x=mean, xend=mean, y=0, yend=max(tab$Freq) + 0.5)) +
                  geom_label(aes(x=mean, y=max(tab$Freq) + 0.5, label = paste0("Média = ", mean))) +
                  annotate("text", x = mode, y = 0.5, label = "Moda" , color="black", size=4 , angle=0) +
                  #geom_label(aes(x=mode, y=0, label = paste0("Moda"))) +
                  theme_classic() +
                  scale_fill_manual(values = c("steelblue", "red"),
                                    labels = c("", "Moda")) +
                  scale_x_continuous(breaks = round(seq(min(tab$elementos), max(tab$elementos), by = 1))) +
                  theme(legend.position="none") +
                  theme(legend.title = element_blank()) +
                  labs(x="Valores", y = "Quantidade de elementos com esse valor")
              })
              
              ##################################################################################################
              #Página Gráficos
              ##################################################################################################
              
              output$uiGrafQual <- renderUI({
                tags <- tagList(h3(strong("Gráficos para variáveis qualitativas")))
                if (sum(userData$shortLevels) >= 1) {
                  tags <- tagList(tags, selectInput("varGrafQual", "Variável", choices = colnames(userData$data)[userData$shortLevels]))
                }
                tags <- tagList(
                  tags,
                  h5(strong("Gráfico de Barras")),
                  p("O gráfico de barras se dá por retângulos (barras) em que em uma de suas direções (mais frequentemente 
      na vertical) representa-se a frequência absoluta (contagem) ou relativa (porcentagem) de uma 
      variável qualitativa. Cada barra representa um possível valor e todas são paralelas umas às outras."),
                  h5(strong("Gráfico de Pizza")),
                  p("O gráfico de setores, ou gráfico de pizza, consiste de um círculo dividido em fatias que são proporcionais 
      às frequências relativas da variável."),
                  br(),
                  helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva.")
                )
                tags
              })
              
              output$uiGrafQuant <- renderUI({
                tags <- tagList(h3(strong("Gráficos para variáveis quantitativas")))
                if (sum(userData$numbers) >= 1) {
                  tags <- tagList(tags, 
                                  selectInput("varGrafQuant", "Variável", choices = colnames(userData$data)[userData$numbers])
                  )
                }
                
                tags <- tagList(
                  tags,
                  h5(strong("Gráfico de Barras")),
                  p("Quando trabalhamos com variáveis quantitativas discretas, ou seja, que assumem uma quantidade limitada 
        de valores, podemos usar um gráfico similar ao de barras usado para variáveis qualitativas ordinais. 
        Dispomos os valores possíveis, em ordem, em barras e a altura é relativa a frequência."),
                  h5(strong("Histograma")),
                  p("Para variáveis quantitativas contínuas, fica inviável representar uma barra para cada observação. Imagine: 
      teria uma barra para o valor 4.82, uma para o 4.91, e assim em diante. Portanto, transformamos a variável 
      quantitativa em uma qualitativa ordinal por meio da criação de intervalos. A variável Peso, por exemplo, 
      é dividida em faixas de peso, para assim construir o histograma."),
                  p("O histograma é um gráfico de barras adjacentes com bases proporcionais aos intervalos definidos, e a área de cada 
      barra é proporcional à sua respectiva frequência."),
                  h5(strong("Boxplot")),
                  HTML("<p>O boxplot é um gráfico muito utilizado pois a partir dele pode-se obter muitas informações sobre a dispersão, 
         posição, assimetria, caudas e valores discrepantes entre os dados. Ele se dá por um retângulo, onde cada limite é um quantil (o limite inferior é o 
         primeiro quartil (q1), e o limite superior é o terceiro quartil (q3).) O traço no meio representa a <strong>mediana</strong> 
         dos dados (q2). A partir do retângulo, para cima, segue uma linha até o ponto mais remoto que não exceda 
         LS = q3 + 1,5dq, chamado de limite superior. Similarmente, segue uma linha abaixo até o ponto que não exceda 
         LI = q1 - 1,5dq, chamando limite inferior. Os valores acima do limite superior e abaixo do limite inferior 
         são plotados como pontos e chamados de outliers ou valores atípicos.</p>"),
                  br(),
                  helpText("Observação: dq é a distância interquartil, que se dá por dq = q3 - q1."),
                  br(),
                  helpText("Fonte: Morettin, P. and Bussab, W. (2000). Estatística Básica (7a. ed.). Editora Saraiva.")
                )
                tags
              })
              
              output$graficosQual <- renderUI({
                tags <- NULL
                
                
                if (sum(userData$shortLevels) >= 1) {
                  req(input$varGrafQual)
                  req(input$varGrafQual %in% colnames(userData$data))
                  if (!userData$custom) {
                    if (input$varGrafQual %in% c("Sexo", "Trabalha", "Relacionamento", "Pratica esportes", "Toma vitaminas", "Culinária favorita")) {
                      tags <- tagList(
                        column(6,h4(strong("Gráfico de Barras")),
                               plotOutput("grafBarras")),
                        column(6, h4(strong("Gráfico de Pizza")),
                               plotOutput("grafPizza")))
                    } else {
                      tags <- tagList(
                        h4(strong("Gráfico de Barras")),
                        plotOutput("grafBarras"),
                        helpText("O gráfico de pizza não é exibido pois a variável é qualitativa ordinal.")
                      )
                    }
                  } else {
                    tags <- tagList(
                      column(6,h4(strong("Gráfico de Barras")),
                             plotOutput("grafBarras")),
                      column(6, h4(strong("Gráfico de Pizza")),
                             plotOutput("grafPizza"))
                    )
                  }
                } else {
                  print("entrou")
                  tags <- tagList(p(h4("A fonte de dados que você está utilizando não possui variáveis qualitativas 
                para podemos mostrar os gráficos. Escolha outra fonte de dados ou use os dados nativos.")))
                  #imagemTriste?
                }
                
                
                tags
                
              })
              
              output$graficosQuant <- renderUI({
                tags <- NULL
                if (sum(userData$numbers) >= 1) {
                  tags <- tagList(
                    fluidRow(column(6,htmlOutput("tituloGraf1"),
                                    plotOutput("histograma"),
                                    conditionalPanel("input.varGrafQuant != 'Ano letivo' && input.varGrafQuant != 'Percepção de Saúde'",
                                                     sliderInput("histNumBins", "Número de Barras", min = 5, max = 20, value = 10, step = 1)),
                                    htmlOutput("htGraf1")
                    ),
                    column(6, h4(strong("Boxplot")),
                           plotOutput("boxplot"))),
                    fluidRow(column(6,
                                    uiOutput("histAlt")))
                  )
                } else {
                  tags <- tagList(
                    p("A fonte de dados que você está utilizando não possui variáveis quantitativas 
          para mostrarmos os gráficos. Escolha outra fonte de dados ou use os dados nativos.")
                  )
                }
                
                tags
              })
              
              getQualPlotData <- reactive({
                req(input$varGrafQual)
                req(input$varGrafQual %in% colnames(userData$data))
                return (userData$data[input$varGrafQual])
              })
              
              getQuantPlotData <- reactive({
                req(input$varGrafQuant)
                req(input$varGrafQuant %in% colnames(userData$data))
                return (userData$data[input$varGrafQuant])
              })
              
              output$grafBarras <- renderPlot({
                selData <- getQualPlotData()
                selData <- as.data.frame(table(selData))
                colnames(selData) <- c("cat", "freq")
                selData$freq <- selData$freq/sum(selData$freq)
                print(selData)
                
                g <- ggplot(data = selData, aes(x=cat, y=freq, fill = cat)) +
                  geom_bar(stat="identity", colour = "black", width = 0.8) +
                  geom_text(aes(x = cat, y = freq, label = round(freq, 2)), colour = "black", vjust = -2) +
                  scale_fill_manual(values=colorful) +
                  guides(fill=FALSE) +
                  theme_classic() +
                  scale_x_discrete(name = input$varGrafQual) +
                  scale_y_continuous(name = "Frequência relativa", limits = c(0, max(selData$freq) + 0.05)) +
                  theme(axis.title.y = element_text(size = 16),
                        axis.text.y = element_text(size = 12))
                
                g
              })
              
              output$grafPizza <- renderPlot({
                selData <- getQualPlotData()
                selData <- as.data.frame(table(selData))
                colnames(selData) <- c("cat", "freq")
                
                g <- ggplot(selData, aes(x="", y=freq, fill = cat)) +
                  geom_bar(width = 1, stat = "identity", colour = "black") +
                  coord_polar("y", start = 0) +
                  scale_fill_manual(values = colorful, name = input$varGrafQual) + 
                  xlab(" ") +
                  ylab(" ") + 
                  theme_classic() + 
                  theme(
                    axis.ticks = element_blank(),
                    axis.text.x=element_blank()
                  )
                
                if (length(levels(selData$cat)) <= 5) {
                  g <- g + geom_text(aes(label = scales::percent(freq/sum(freq))), position = position_stack(vjust = 0.5))
                }
                
                g
              })
              
              
              
              output$histograma <- renderPlot({
                g <- NULL
                req(input$varGrafQuant)
                req(input$varGrafQuant %in% colnames(userData$data))
                if (!userData$custom && input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")) { #gráfico de barras para var. quant. discr.
                  selData <- data.frame(as.factor(data[,colnames(data) == input$varGrafQuant]))
                  colnames(selData) <- c("Var")
                  g <- ggplot() + 
                    geom_bar(data=selData, aes(x=Var, fill = Var), stat="count", colour = "black", width = 0.8)
                  gd <- as.data.frame(ggplot_build(g)$data)
                  g <- g + geom_text(data=gd,
                                     aes(x=x, y=ymax, label=count),
                                     colour = "black", vjust = -2) +
                    theme_classic() +
                    guides(fill=FALSE) +
                    scale_x_discrete(name = input$varGrafQuant) +
                    scale_y_continuous(name = "Frequência absoluta", limits = c(0, max(gd$ymax) + 5))
                } else {
                  selData <- data.frame(userData$data[,colnames(userData$data) == input$varGrafQuant])
                  colnames(selData) <- c("Var")
                  #N <- ifelse(input$varGrafQuant == "Altura", 0.05, 5)
                  amplitude <- max(selData$Var, na.rm = TRUE) - min(selData$Var, na.rm = TRUE)
                  N <- 5 * 10^(floor(log10(amplitude)) - 1)
                  x_breaks <- seq(floorN(min(selData$Var, na.rm = TRUE), N), ceilN(max(selData$Var, na.rm = TRUE), N), length.out = input$histNumBins + 1)
                  gap <- x_breaks[2] - x_breaks[1]
                  freq <- as.data.frame(table(cut(selData$Var, breaks = x_breaks)))$Freq/(length(selData$Var))
                  seq <- (x_breaks + (gap/2))[-length(x_breaks)]
                  freqdata <- data.frame(seq, freq)
                  print(freqdata)
                  g <- ggplot(data=freqdata, aes(x=seq, y=freq)) + 
                    geom_bar(stat="identity", colour="black", fill = "#4cA6AA", width = gap) + 
                    theme_classic() +
                    scale_x_continuous(breaks = x_breaks, labels = round(x_breaks, ifelse(amplitude < 10, abs(floor(log10(amplitude))) + 1, 0))) + 
                    ylab("Frequência Relativa") +
                    xlab(input$varGrafQuant) + 
                    geom_text(data=freqdata,
                              aes(x=seq, y=freq + 0.008, label=scales::percent(freq)), size = 3.8)
                  
                }
                g
              })
              
              output$tituloGraf1 <- renderText({
                req(input$varGrafQuant)
                text <- ifelse((!userData$custom && input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")),
                               "<h4><strong>Gráfico de Barras</strong></h4>",
                               "<h4><strong>Histograma</strong></h4>")
                text 
              })
              
              output$htGraf1 <- renderText({
                req(input$varGrafQuant)
                ifelse((!userData$custom && input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")), paste0("<span class = 'help-block'> Pode-se utilizar o gráfico de barras pois a variável ", 
                                                                                                                   input$varGrafQuant, " é quantitativa discreta.</span>"),
                       paste0("<span class = 'help-block'> Utiliza-se somente o histograma pois a variável ", 
                              input$varGrafQuant, " é quantitativa contínua.</span>"))
              })
              
              output$boxplot <- renderPlot({
                req(input$varGrafQuant)
                req(input$varGrafQuant %in% colnames(userData$data))
                selData <- getQuantPlotData()
                selData <- as.data.frame(selData)
                colnames(selData) <- c("cat")
                p <- ggplot(selData, aes(x="",y=cat)) + 
                  geom_boxplot(fill = "#4CA6AA", colour = "black") + 
                  xlab("") +
                  ylab(input$varGrafQuant)
                p
              })
              
              output$histogramaAlt <- renderPlot({
                req(input$varGrafQuant)
                req(input$varGrafQuant %in% colnames(userData$data))
                selData <- getQuantPlotData()
                g <- NULL
                colnames(selData) <- c("Var")
                
                if (input$varGrafQuant == "Ano letivo") {
                  selData$Var <- cut(selData$Var, breaks=c(-Inf, 2.5, Inf), labels = c("Início", "Fim"))
                  selData <- as.data.frame(table(selData))
                  colnames(selData) <- c("Var", "Freq")
                  selData$Freq <- selData$Freq/sum(selData$Freq)
                  print(selData)
                  g <- ggplot(selData, aes(x=Var, y=Freq)) + 
                    geom_bar(stat="identity", colour = "black", fill = "#4cA6AA") +
                    geom_text(aes(x = Var, y = Freq, label = scales::percent(Freq)), colour = "black", vjust = -2) +
                    scale_y_continuous(limits = c(0, max(selData$Freq) + 0.05)) +
                    theme_classic() + 
                    ylab("Frequência Relativa") + 
                    xlab(input$varGrafQuant)
                } else {
                  selData$Var <- cut(selData$Var, breaks=c(-Inf, 2, 4, 6, 8, Inf), labels = c("Muito ruim", "Ruim", "Regular", "Bom", "Muito bom"))
                  selData <- as.data.frame(table(selData))
                  colnames(selData) <- c("Var", "Freq")
                  selData$Freq <- selData$Freq/nrow(selData)
                  selData$Freq <- selData$Freq/sum(selData$Freq)
                  print(selData)
                  g <- ggplot(selData, aes(x=Var, y=Freq)) + 
                    geom_bar(stat="identity", colour = "black", fill = "#4cA6AA") +
                    geom_text(aes(x = Var, y = Freq, label = scales::percent(Freq)), colour = "black", vjust = -2) +
                    scale_y_continuous(limits = c(0, max(selData$Freq) + 0.05)) +
                    theme_classic() + 
                    ylab("Frequência Relativa") + 
                    xlab(input$varGrafQuant)
                }
                g
              })
              
              output$histAlt <- renderUI({
                tags <- NULL
                req(input$varGrafQuant)
                req(input$varGrafQuant %in% colnames(userData$data))
                if (!userData$custom && input$varGrafQuant %in% c("Ano letivo", "Percepção de Saúde")) {
                  tags <- tagList(fluidRow(
                    column(6, 
                           h4(strong(("Histograma")),
                              plotOutput("histogramaAlt"),
                              helpText("Mesmo sendo uma variável quantitativa discreta, pode-se utilizar o 
                               histograma além do gráfico de barras."))
                    ), 
                    column(6,
                           p("Separação de valores em intervalos"),
                           tableOutput("tabHist")
                    )
                  ))
                }
                tags
              })
              
              output$tabHist <- renderTable({
                tab <- NULL
                req(input$varGrafQuant)
                req(input$varGrafQuant %in% colnames(userData$data))
                if (!userData$custom) {
                  if (input$varGrafQuant == "Ano letivo") {
                    tab <- as.data.frame(cbind(c("[1, 2]", "[3, 4]"), c("Início", "Fim")))
                  } else if (input$varGrafQuant == "Percepção de Saúde") {
                    tab <- as.data.frame(cbind(c("[1, 2]", "[3, 4]", "[5, 6]", "[7, 8]", "[9, 10]"), 
                                               c("Muito ruim", "Ruim", "Regular", "Boa", "Muito boa")))
                  } 
                }
                
                tab
              }, striped = TRUE, colnames = FALSE, bordered = TRUE)
              
              output$biUI <- renderUI({
                
                
                
                enable1 <- sum(userData$shortLevels) >= 2
                enable2 <- sum(userData$numbers) >= 2
                enable3 <- sum(userData$shortLevels) >= 1 & sum(userData$numbers) >= 1
                print(paste0("shortLevels: ", userData$shortLevels))
                print(paste0("enable2: ", enable2))
                print(paste0("enable3: ", enable3))
                
                tags <- NULL
                part1 <- NULL
                part2 <- NULL
                part3 <- NULL
                
                if (enable1) {
                  part1 <- tagList(
                    h4(strong("Duas variáveis qualitativas")),
                    selectInput("varGrafBiQual1", "Variável 1", choices = colnames(userData$data[userData$shortLevels]), 
                                selected = colnames(userData$data[userData$shortLevels])[1]),
                    selectInput("varGrafBiQual2", "Variável 2", choices = colnames(userData$data[userData$shortLevels]), 
                                selected = colnames(userData$data[userData$shortLevels])[2]),
                    p("Imagine que queiramos visualizar a distribuição das observações em 
                        relação a duas variáveis qualitativas. Podemos, primeiramente, visualizar 
                        a tabela de frequências relativas: "),
                    tableOutput("tabGrafVarQual"),
                    p("Podemos agora, construir um gráfico de barras muito similar ao visto 
                        anteriormente, mas com as barras divididas de acordo com a outra variável 
                        variável qualitativa escolhida"),
                    plotOutput("grafBarraBiQual")
                  )
                } else {
                  part1 <- tagList(h4(strong("Duas variáveis qualitativas")),
                                   p("Seu banco de dados utilizado não tem variáveis qualitativas o suficiente 
                         para isso. Por favor escolha outro ou tente com o banco de dados nativo."))
                }
                
                if (enable2) {
                  part2 <- tagList(
                    h4(strong("Duas variáveis quantitativas")),
                    selectInput("varGrafBiQuant1", "Variável 1", choices = colnames(userData$data[userData$numbers]), selected = colnames(userData$data[userData$numbers])[1]),
                    selectInput("varGrafBiQuant2", "Variável 2", choices = colnames(userData$data[userData$numbers]), selected = colnames(userData$data[userData$numbers])[2]),
                    p("O gráfico de dispersão bivariado é utilizado para observar a correlação entre duas variáveis quantitativas. Por exemplo, 
se a nuvem de pontos se assemelha a uma reta crescente ou descrescente, há indícios de correlação linear entre as duas variáveis. 
No entanto, vale ressaltar que há outros tipos de correlação, não lineares."),
                    plotOutput("scatterGraf")
                  )
                } else {
                  part2 <- tagList(h4(strong("Duas variáveis quantitativas")),
                                   p("Seu banco de dados utilizado não tem variáveis quantitativas o suficiente 
                         para isso. Por favor escolha outro ou tente com o banco de dados nativo."))
                }
                
                if (enable3) {
                  part3 <- tagList(
                    h4(strong("Variáveis qualitativas e quantitativas")),
                    selectInput("varQualBoxplot1", "Variável Qualitativa", choices = colnames(userData$data[userData$shortLevels]), 
                                selected = colnames(userData$data[userData$shortLevels])[1]),
                    selectInput("varQualBoxplot2", "Variável Quantitativa", choices = colnames(userData$data[userData$numbers]), 
                                selected = colnames(userData$data[userData$shortLevels])[2]),
                    p("Também é possível visualizar a relação existente entre uma variável qualitativa e uma variável quantitativa. Isto pode ser feito por meio do cálculo de 
medidas resumo, e construção de histogramas e boxplots, para a variável quantitativa em cada categoria da qualitativa. Compare cada variável qualitativa com uma outra quantitativa contínua nos boxplots abaixo:"),
                    checkboxInput("multiBoxplotUseNA", "Usar NA", value=FALSE),
                    #colocar tabela com medidas resumo
                    plotOutput("multiBoxplot")
                  )
                } else {
                  part3 <- tagList(h4(strong("Variáveis qualitativas e quantitativas")),
                                   p("Seu banco de dados utilizado não tem variáveis qualitativas/quantitativas o suficiente 
                         para isso. Por favor escolha outro ou tente com o banco de dados nativo."))
                }
                
                tags <- tagList(
                  h3(strong("Gráficos Bivariados")),
                  fluidRow(column(4, 
                                  part1),
                           column(4, 
                                  part2),
                           column(4,
                                  part3))
                )
                
                tags
                
                
                
                #     tagList(
                #       h3(strong("Gráficos Bivariados")),
                #       fluidRow(column(4,
                #                       h4(strong("Duas variáveis qualitativas")),
                #                       selectInput("varGrafBiQual1", "Variável 1", choices = colnames(userData$data[userData$factors]), selected = 1),
                #                       selectInput("varGrafBiQual2", "Variável 2", choices = colnames(userData$data[userData$factors]), selected = 2),
                #                       p("Imagine que queiramos visualizar a distribuição das observações em 
                #                         relação a duas variáveis qualitativas. Podemos, primeiramente, visualizar 
                #                         a tabela de frequências relativas: "),
                #                       tableOutput("tabGrafVarQual"),
                #                       p("Podemos agora, construir um gráfico de barras muito similar ao visto 
                #                         anteriormente, mas com as barras divididas de acordo com a outra variável 
                #                         variável qualitativa escolhida"),
                #                       plotOutput("grafBarraBiQual")
                #       ),
                #       column(4,
                #              h4(strong("Duas variáveis quantitativas")),
                #              selectInput("varGrafBiQuant1", "Variável 1", choices = colnames(userData$data[userData$numbers]), selected = 1),
                #              selectInput("varGrafBiQuant2", "Variável 2", choices = colnames(userData$data[userData$numbers]), selected = 2),
                #              p("O gráfico de dispersão bivariado é utilizado para observar a correlação entre duas variáveis quantitativas. Por exemplo, 
                # se a nuvem de pontos se assemelha a uma reta crescente ou descrescente, há indícios de correlação linear entre as duas variáveis. 
                # No entanto, vale ressaltar que há outros tipos de correlação, não lineares."),
                #              plotOutput("scatterGraf")
                #       ),
                #       column(4,
                #              h4(strong("Variáveis qualitativas e quantitativas")),
                #              selectInput("varQualBoxplot1", "Variável Qualitativa", choices = colnames(userData$data[userData$factors])),
                #              selectInput("varQualBoxplot2", "Variável Quantitativa", choices = colnames(userData$data[userData$numbers])),
                #              p("Também é possível visualizar a relação existente entre uma variável qualitativa e uma variável quantitativa. Isto pode ser feito por meio do cálculo de 
                # medidas resumo, e construção de histogramas e boxplots, para a variável quantitativa em cada categoria da qualitativa. Compare cada variável qualitativa com uma outra quantitativa contínua nos boxplots abaixo:"),
                #              #colocar tabela com medidas resumo
                #              plotOutput("multiBoxplot")
                #       )
                #       )
                #     )
              })
              
              getBiQualTable <- reactive({
                tab <- NULL
                req(input$varGrafBiQual1, input$varGrafBiQual2)
                req(input$varGrafBiQual1 %in% colnames(userData$data))
                req(input$varGrafBiQual2 %in% colnames(userData$data))
                df <- cbind(userData$data[input$varGrafBiQual1], userData$data[input$varGrafBiQual2])
                colnames(df) <- c("Var1", "Var2")
                if (!is.factor(df$Var1)) {
                  df$Var1 <- as.factor(df$Var1)
                }
                
                if (!is.factor(df$Var2)) {
                  df$Var2 <- as.factor(df$Var2)
                }
                for (i in levels(df$Var1)) {
                  rows <- as.factor(df$Var1) == i
                  col <- (as.data.frame(table(df$Var2[rows]))$Freq)/length(df$Var1[df$Var1==i & !is.na(df$Var1) & !is.na(df$Var2)])
                  col[is.nan(col)] <- 0
                  tab <- cbind(tab, col)
                }
                tab <- rbind(tab, round(colSums(tab)))
                colnames(tab) <- levels(df$Var1)
                rownames(tab) <- c(levels(df$Var2), "Total")
                tab
              })
              
              output$tabGrafVarQual <- renderTable({
                getBiQualTable()
              }, rownames = TRUE, bordered = TRUE, striped = TRUE)
              
              output$grafBarraBiQual <- renderPlot({
                # selData <- as.data.frame(table(data[,colnames(data) == input$varGrafBiQual1], data[, colnames(data) == input$varGrafBiQual2]))
                # selData <- selData %>% 
                #   group_by(Var1) %>%
                #   arrange(Var1, desc(Var2)) %>%
                #   mutate(y_pos = cumsum(Freq) - 0.5*Freq)
                # g <- ggplot(selData, aes(x=Var1, y=Freq, fill=Var2)) + 
                #   geom_bar(stat="identity", color = "black", width = 0.9) +
                #   geom_text(aes(y = y_pos, label = ifelse(Freq > 0, Freq, ""), group = Var2)) +
                #   labs(x=input$varGrafBiQual1, y="Frequência absoluta") + 
                #   scale_fill_discrete(name=input$varGrafBiQual2) +
                #   theme_classic()
                # g
                
                df_src <- getBiQualTable()
                df <- melt(df_src[-dim(df_src)[1],])
                print(df)
                pos <- df %>%
                  group_by(Var2) %>%
                  arrange(Var2, desc(Var1)) %>%
                  mutate(y_pos = cumsum(value) - 0.5*value)
                print(pos)
                g <- ggplot(df, aes(x=Var2, y=value, fill = as.factor(Var1))) + 
                  geom_bar(stat="identity", color = "black", width = 0.9) + 
                  geom_text(data=pos, aes(y=y_pos, label = ifelse(value>=0.01, scales::percent(value, digits = 2), ""), group = as.factor(Var1))) +
                  labs(x=input$varGrafBiQual1, y="Frequência absoluta") + 
                  scale_fill_discrete(name=input$varGrafBiQual2) + 
                  theme_classic()
                g
                
                
              })
              
              output$scatterGraf <- renderPlot({
                req(input$varGrafBiQuant1, input$varGrafBiQuant2)
                req(input$varGrafBiQuant1 %in% colnames(userData$data))
                req(input$varGrafBiQuant2 %in% colnames(userData$data))
                selData <- data.frame(userData$data[,colnames(userData$data) == input$varGrafBiQuant1], userData$data[,colnames(userData$data) == input$varGrafBiQuant2])
                colnames(selData) <- c("Var1", "Var2")
                g <- ggplot(selData, aes(x=Var1, y=Var2)) + 
                  geom_point(color = "#5497A7", size = 3L) +
                  theme_classic() + 
                  labs(x=input$varGrafBiQuant1, y=input$varGrafBiQuant2)
                g
              })
              
              output$multiBoxplot <- renderPlot({
                req(input$varQualBoxplot1, input$varQualBoxplot2)
                req(input$varQualBoxplot1 %in% colnames(userData$data))
                req(input$varQualBoxplot2 %in% colnames(userData$data))
                selData <- data.frame(userData$data[,colnames(userData$data) == input$varQualBoxplot1], userData$data[,colnames(userData$data) == input$varQualBoxplot2])
                colnames(selData) <- c("Var1", "Var2")
                if (!is.factor(selData$Var1)) {
                  selData$Var1 <- as.factor(as.character(selData$Var1))
                }
                if (!input$multiBoxplotUseNA) {
                  selData <- selData[!is.na(selData$Var1),]
                }
                #print(selData)
                g <- ggplot(selData, aes(x=Var1, y=Var2, fill=Var1)) + 
                  geom_boxplot() + 
                  theme_classic() + 
                  labs(x="", y = input$varQualBoxplot2) + 
                  scale_fill_discrete(name = input$varQualBoxplot1)
                g
                
              })
              
              
              
              bernoulliPlot <- reactive({
                p <- input$bernoulli_p
                bern <- data.frame(c(0, 1), c(1-p, p))
                colnames(bern) <- c("x", "p(x)")
                bern$x <- as.factor(as.character(bern$x))
                g <- ggplot(data = bern, aes(x=x, y=`p(x)`)) + 
                  geom_bar(stat="identity", fill = medium_cyan, color = "white", width = 0.3) + 
                  labs(title = paste0("Distribuição Bernoulli de Probabilidades com p = ", p),
                       y = "Probabilidade", 
                       x = "x") + 
                  scale_y_continuous(breaks = seq(0, 1, by=0.1), limits = c(0, 1)) + 
                  theme_classic() + 
                  theme(axis.ticks.x = element_blank(),
                        axis.title.x = element_text(size = 20),
                        axis.text.x = element_text(size = 14), 
                        axis.title.y = element_text(size = 14),
                        axis.text.y = element_text(size = 12),
                        title = element_text(size = 16))
                
                g
              })
              
              binomPlot <- reactive({
                p <- input$binom_p
                n <- input$binom_n
                x_breaks <- c(0:n)
                if (n > 25)
                  x_breaks <- seq(5, n, by = 5)
                
                values <- data.frame(x = 0:n, y = dbinom(0:n, n, p))
                print(x_breaks)
                g <- ggplot(data = values, aes(x=x, y=y)) + 
                  geom_bar(stat = "identity", width = 0.5, color = "white", fill = medium_cyan) + 
                  labs(title = paste0("Distribuição Binomial de Probabilidades com p = ", p, " e n = ", n)) + 
                  ylab("Probabilidade") + 
                  scale_x_continuous(name = "k", breaks = x_breaks) +
                  theme_classic() + 
                  theme(axis.ticks.x = element_blank(),
                        axis.title.x = element_text(size = 20),
                        axis.text.x = element_text(size = 14), 
                        axis.title.y = element_text(size = 14),
                        axis.text.y = element_text(size = 12),
                        title = element_text(size = 16))
                
                return(g)
              })
              
              buildBernoulliTable <- reactive({
                p <- input$bernoulli_p
                bern <- data.frame(c(0, 1), c(1-p, p), c("Probabilidade de que seja 0 (fracasso)", "Probabilidade de que seja 1 (sucesso)"))
                colnames(bern) <- c("x", "p(x)", "Explicação")
                bern$`p(x)` <- scales::percent(bern$`p(x)`)
                bern
              })
              
              buildBinomTable <- reactive({
                p <- input$binom_p
                n <- input$binom_n
                binomTab <- data.frame(0:n, round(dbinom(0:n, n, p), 4))
                colnames(binomTab) <- c("k", "p(k)")
                if (n > 25) {
                  binomTab <- binomTab[binomTab$`p(k)` > 0,]
                }
                binomTab$exp <- ""
                binomTab$`p(k)` <- scales::percent(binomTab$`p(k)`)
                for (i in 1:nrow(binomTab)) {
                  binomTab$exp[i] <- paste0("Probabilidade de ter ", binomTab$k[i], " sucesso(s) em ", n, " tentativas.")
                }
                colnames(binomTab)[3] <- "Explicação"
                return (binomTab)
              })
              
              buildPoissonTable <- reactive({
                u <- input$poisson_u
                min <- input$poisson_minmax[1]
                max <- input$poisson_minmax[2]
                
                poisTab <- data.frame(min:max, dpois(min:max, u))
                colnames(poisTab) <- c("k", "p(k)")
                poisTab$exp <- ""
                poisTab$`p(k)` <- scales::percent(poisTab$`p(k)`)
                for (i in 1:nrow(poisTab)) {
                  poisTab$exp[i] <- paste0("Probabilidade de ter ", poisTab$k[i], " acontecimento(s) no intervalo de tempo.")
                }
                colnames(poisTab)[3] <- "Explicação"
                return (poisTab)
              })
              
              output$distrTable <- renderTable({
                switch (input$distribuicao,
                        "bernoulli" = buildBernoulliTable(),
                        "binomial" = buildBinomTable(),
                        "poisson" = buildPoissonTable(),
                        "normal" = buildBinomTable()
                )
              }, bordered = TRUE, striped = TRUE, colnames = TRUE, width = "100%", align = "c")
              
              poissonPlot <- reactive({
                u <- input$poisson_u
                min <- input$poisson_minmax[1]
                max <- input$poisson_minmax[2]
                x_breaks <- c(min:max)
                if (max - min > 20)
                  x_breaks <- seq(min, max, by = 5)
                values <- data.frame(x = min:max, y = dpois(min:max, u))
                g <- ggplot(data = values, aes(x=x, y=y)) + 
                  geom_bar(stat = "identity", width = 0.5, color = "white", fill = medium_cyan) +
                  ylab("Probabilidade") + 
                  scale_x_continuous(name = "k", breaks = x_breaks) +
                  theme_classic() + 
                  theme(axis.ticks.x = element_blank(),
                        axis.title.x = element_text(size = 20),
                        axis.text.x = element_text(size = 14), 
                        axis.title.y = element_text(size = 14),
                        axis.text.y = element_text(size = 12),
                        title = element_text(size = 16))
                
                
                return(g)
              })
              
              observeEvent(input$distribuicao, {
                if (input$distribuicao == "normal") {
                  req(input$normal_p)
                  click("normal_refresh")
                  print("triggered")
                }
              })
              
              observeEvent(input$normal_refresh, {
                normal_params$u <- input$normal_media
                normal_params$dp <- input$normal_dp
                normal_params$min <- input$normal_range[1]
                normal_params$max <- input$normal_range[2]
                min <- normal_params$u - 4*normal_params$dp
                max <- normal_params$u + 4*normal_params$dp
                normal_params$sit <- 1
                if (normal_params$min > min) {
                  if (normal_params$max < max) {
                    normal_params$sit <- 3
                  } else {
                    normal_params$sit <- 4
                  }
                } else {
                  if (normal_params$max < max) {
                    normal_params$sit <- 2
                  } else {
                    normal_params$sit <- 1
                  }
                }
                
              })
              
              output$normal_params <- renderUI({
                u <- input$normal_media
                dp <- input$normal_dp
                min <- u - 4*dp
                max <- u + 4*dp
                tags <- tagList(
                  sliderInput("normal_range", label = "Intervalo de interesse", 
                              min = min, max = max, value = c(u + dp, max), round = -2, step = 0.05)
                  
                  
                  
                )
                tags
              })
              
              normalPlot <- reactive({
                req(input$normal_range)
                u <- normal_params$u
                dp <- normal_params$dp
                min_p <- normal_params$min
                max_p <- normal_params$max
                print(min_p)
                print(max_p)
                print(normal_params$sit)
                yend <- switch (normal_params$sit,
                                dnorm(u + 2*dp, mean=u, sd=dp),
                                dnorm(max_p, mean=u, sd=dp),
                                dnorm(max_p, mean=u, sd=dp),
                                dnorm(min_p, mean=u, sd=dp)
                ) 
                xpos <- switch(normal_params$sit,
                               u+2*dp + 1.5,
                               ifelse(max_p > u, max_p + 1.5, max_p - 1.5),
                               ifelse(max_p > u, max_p + 1.5, max_p - 1.5),
                               ifelse(min_p > u, min_p + 1.5, min_p - 1.5)
                )
                
                min <- u - 6*dp
                max <- u + 6*dp
                
                prob <- round(pnorm(max_p, u, dp) - pnorm(min_p, u, dp), 3)
                enum <- switch (normal_params$sit,
                                paste0("P(", min_p, " <= x <= ", max_p, ") = "),
                                paste0("P(x <= ", max_p, ") = "),
                                paste0("P(", min_p, " <= x <= ", max_p, ") = "),
                                paste0("P(x >= ", min_p, ") = ")
                )
                enum <- paste0(enum, scales::percent(prob))
                x <- seq(min_p, max_p, length = 300)
                y <- dnorm(x, mean=u, sd=dp)
                area <- data.frame(x, y)
                
                g <- ggplot(data = area, aes(x=x, y=y)) + 
                  geom_area(stat="identity", fill = medium_cyan, alpha = 0.7) + 
                  stat_function(fun=dnorm, n=101, args = list(mean=u, sd=dp), color=dark_cyan) + 
                  scale_x_continuous(limits = c(min, max)) + 
                  geom_segment(aes(x=u, xend=u, y=0, yend=dnorm(u, u, dp)), color = "gray", linetype=2, alpha=0.3) + 
                  annotate(geom="text", x=u, y=dnorm(u,u,dp) + 0.05, label=paste0("μ = ", u), size=5, color=dark_cyan) + 
                  annotate(geom="text", x=xpos, y=yend + 0.01, label=enum) + 
                  labs(x = "x", y = "f(x)")
                
                g <- switch (normal_params$sit,
                             g,
                             g + geom_segment(aes(x=max_p, xend=max_p, y=0, yend=dnorm(max_p, u, dp)), color=dark_cyan, linetype=2),
                             g + geom_segment(aes(x=max_p, xend=max_p, y=0, yend=dnorm(max_p, u, dp)), color=dark_cyan, linetype=2) + 
                               geom_segment(aes(x=min_p, xend=min_p, y=0, yend=dnorm(min_p, u, dp)), color=dark_cyan, linetype=2),
                             g + geom_segment(aes(x=min_p, xend=min_p, y=0, yend=dnorm(min_p, u, dp)), color=dark_cyan, linetype=2)
                )
                g
                
                
              })
              
              output$normalExemplo <- renderText({
                req(input$normal_range)
                u <- normal_params$u
                dp <- normal_params$dp
                min_p <- normal_params$min
                max_p <- normal_params$max
                z1 <- (min_p - u)/dp
                z2 <- (max_p - u)/dp
                text <- c("P(", min_p, " <= x <= ", max_p, ") = \nP((", min_p, " - μ)/σ <= (x- μ)/σ <= (", max_p, " - μ)/σ) = \n
              P((", min_p, " - ", u, ")/", dp, " <= Z <= (", max_p, " - ", u, ")/", dp, ") = \n
              P(", z1, " <= Z <= ", z2, ")\n\n
              P(Z >= ", z1, ") = 1 - P(Z <= ", z1, ") = 1 - ", round(pnorm(min_p, u, dp), 3), " = ", round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), "\n
              P(Z <= ", z2, ") = ", round(pnorm(max_p, u, dp), 3), "\n\n
              P(", z1, " <= Z <= ", z2, ") = ", round(pnorm(max_p, u, dp), 3), " - ", round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), " = ", round(pnorm(max_p, u, dp), 3) - round(pnorm(min_p, u, dp, lower.tail=TRUE), 3), " = \n
              ", scales::percent(round(pnorm(max_p, u, dp), 3) - round(pnorm(min_p, u, dp, lower.tail=TRUE), 3)))
                text
              })
              
              output$expExemplo <- renderUI({
                b <- input$exp_b
                min <- input$exp_val[1]
                max <- input$exp_val[2]
                prob <- pexp(max, b) - pexp(min, b)
                withMathJax(paste0("P(", min, " <= x <= ", max, ") = $$\\int_{", min, "}^{", max, "}
                       2e^{-2x}dx = ", prob, "$$"))
              })
              
              expPlot <- reactive({
                b <- input$exp_b
                min <- input$exp_val[1]
                max <- input$exp_val[2]
                x <- seq(min, max, length = 300)
                y <- dexp(x, b)
                area <- data.frame(x, y)
                prob <- pexp(max, b) - pexp(min, b)
                text <- paste0("P(", min, " <= x <= ", max, ") = ", scales::percent(round(prob, 4)))
                g <- ggplot(data = area, aes(x=x, y=y)) + 
                  geom_area(stat="identity", fill = medium_ocre) + 
                  stat_function(fun=dexp, n=101, args = list(rate = b), color="black") + 
                  scale_x_continuous(name="x", limits = c(0, 3)) + 
                  scale_y_continuous(name="f(x)", limits=c(0, b + 1)) + 
                  geom_segment(aes(x=max, xend=max, y=0, yend=dexp(max, b)), color = brown, alpha= 0.7, linetype=2) +
                  annotate(geom="text", x = 2, y=b+0.1, label=text, color=brown, size = 6) 
                
                if (min > 0) {
                  g <- g + geom_segment(aes(x=min, xend=min, y=0, yend=dexp(min, b)), color = "brown", alpha=0.7, linetype=2)
                }
                g
              })
              
              quiPlot <- reactive({
                qx <- input$qui_x
                min <- input$qui_values[1]
                max <- input$qui_values[2]
                x <- seq(min, max, length = 300)
                y <- dchisq(x, qx)
                area <- data.frame(x, y)
                prob <- pchisq(max, qx) - pchisq(min, qx)
                text <- paste0("P(", min, " <= x <= ", max, ") = ", scales::percent(round(prob, 4)))
                g <- ggplot(data = area, aes(x=x, y=y)) + 
                  geom_area(stat="identity", fill = red_nail) + 
                  stat_function(fun=dchisq, n=101, args = list(df = qx), color=red_broken_nail) + 
                  scale_x_continuous(name="x", limits = c(0, 16)) + 
                  ylab("f(x)") + 
                  geom_segment(aes(x=max, xend=max, y=0, yend=dchisq(max, qx)), color = red_broken_nail, alpha= 0.7, linetype=2) +
                  annotate(geom="text", x = 12, y=dchisq(max, qx) + 0.1, label=text, color=red_broken_nail, size = 6) 
                
                if (min > 0) {
                  g <- g + geom_segment(aes(x=min, xend=min, y=0, yend=dchisq(min, qx)), color = red_broken_nail, alpha=0.7, linetype=2)
                }
                g
              })
              
              tPlot <- reactive({
                gl <- input$t_gl
                min <- -4
                max <- 4
                x <- seq(min, max, length=300)
                y <- dt(x, gl)
                area <- data.frame(x, y)
                
                g <- ggplot(data=area, aes(x=x, y=y)) + 
                  stat_function(fun=dnorm, n=101, args=list(mean=0, sd=1), color = medium_cyan, alpha = 0.7) +
                  stat_function(fun=dt, n=101, args=list(df=gl), color = "black") + 
                  annotate(geom="text", x=2, y=dnorm(1, 0, 1), color=medium_cyan, label = "Distribuição Normal") + 
                  annotate(geom="text", x=-0.1, y=dt(-1, gl) - 0.15, color="black", label = paste0("Distribuição T com ", gl, " graus de liberdade")) + 
                  labs(x="x", y="f(x)")
                g
              })
              
              
              
              distributionInput <- reactive({
                switch (input$distribuicao,
                        "bernoulli" = bernoulliPlot(),
                        "binomial" = binomPlot(),
                        "poisson" = poissonPlot(),
                        "normal" = normalPlot(),
                        "exp" = expPlot(),
                        "qui" = quiPlot(),
                        "t" = tPlot()
                )
              })
              
              output$distribuicao <- renderPlot({
                g <- distributionInput()
                g
              })
              
              #####################################################################3####33
              ##################### P R O B A B I L I D A D E ############################
              ############################################################################
              
              output$tabelaProbUI <- renderUI({
                tags <- NULL
                if (sum(userData$shortLevels) >= 2) {
                  tags <- tagList(
                    fluidRow(
                      column(6, 
                             selectInput("probTabVar1", "Variável 1", choices = colnames(userData$data)[userData$shortLevels], selected = colnames(userData$data)[userData$shortLevels][1])
                      ),
                      column(6, selectInput("probTabVar2", "Variável 2", choices = colnames(userData$data)[userData$shortLevels], selected = colnames(userData$data)[userData$shortLevels][2]))
                    ),
                    tableOutput("tabelaProbDef")
                  )
                } else {
                  tags <- tagList(
                    column(12, align = "center",
                           p("O conjunto de dados que você escolheu não tem variáveis qualitativas o 
                 suficiente para esta função. Escolha outro conjunto de dados ou utilize nosso 
                 nativo."))
                  )
                }
                
                tags
                
                
                
              })
              
              output$tabelaProbDef <- renderTable({
                req(input$probTabVar1)
                req(input$probTabVar2)
                req(input$probTabVar1 %in% colnames(userData$data)[userData$shortLevels])
                req(input$probTabVar2 %in% colnames(userData$data)[userData$shortLevels])
                var1 <- input$probTabVar1
                var2 <- input$probTabVar2
                
                tab <- table(userData$data[,colnames(userData$data) == var1], userData$data[,colnames(userData$data) == var2])
                tab <- as.data.frame.matrix(tab)
                tab <- cbind(rownames(tab), tab)
                rownames(tab) <- NULL
                colnames(tab)[1] <- paste0(var1, "/", var2)
                tab$Total <- rowSums(tab[-1])
                tab[,1] <- as.character(tab[,1])
                tab <- rbind(tab, c("Total", colSums(tab[-1])))
                tab[,1] <- as.factor(tab[,1])
                tab
              }, striped = TRUE, bordered = TRUE, digits = 0, width = "80%")
              
              output$calcProbUI <- renderUI({
                tags <- NULL
                if (sum(userData$shortLevels) >= 2) {
                  req(input$probTabVar1)
                  req(input$probTabVar2)
                  req(input$probTabVar1 %in% colnames(userData$data)[userData$shortLevels])
                  req(input$probTabVar2 %in% colnames(userData$data)[userData$shortLevels])
                  tags <- tagList(
                    selectInput("prob_in1", "Probabilidade de: ", 
                                choices = levels((userData$data[,colnames(userData$data) == input$probTabVar1]))),
                    radioButtons("probOp", " ", choices = c("União" = "uniao", "Intersecção" = "intersec", 
                                                            "Apenas dela" = "only"), selected = "only"),
                    conditionalPanel("input.probOp != 'only'",
                                     selectInput("prob_in2", "com: ", 
                                                 choices = levels((userData$data[,colnames(userData$data) == input$probTabVar2]))))
                  )
                }
                
                tags
                
              })
              
              output$caixaContaProb <- renderText({
                text <- "Nada a mostrar."
                if (sum(userData$shortLevels) >= 2) {
                  req(input$probTabVar1)
                  req(input$probTabVar2)
                  req(input$probTabVar1 %in% colnames(userData$data)[userData$shortLevels])
                  req(input$probTabVar2 %in% colnames(userData$data)[userData$shortLevels])
                  req(input$probOp)
                  var1 <- input$probTabVar1
                  var2 <- input$probTabVar2
                  att1 <- input$prob_in1
                  att2 <- NULL
                  letter1 <- substr(input$prob_in1, 1, 2)
                  letter2 <- "NO_INPUT"
                  
                  #numero de ocorrencias primeiro caso
                  freq1 <- nrow(userData$data[userData$data[!is.na(userData$data[var1]),colnames(userData$data) == var1] == att1,])
                  tot <- nrow(userData$data[!is.na(userData$data[var1]) & !is.na(userData$data[var2]),])
                  #pode ser que nao tenha segunda variavel
                  freq2 <- 0
                  freq12 <- 0
                  #se houver segunda variavel:
                  if (input$probOp != "only") {
                    att2 <- input$prob_in2
                    letter2 <- substr(att2, 1, 2)
                    if (letter2 == letter1) {
                      letter2 <- substr(att2, 1, 4)
                    }
                    
                    freq2 <- nrow(userData$data[userData$data[!is.na(userData$data[var2]),colnames(userData$data) == var2] == att2,])
                    freq12 <- nrow(userData$data[userData$data[!is.na(userData$data[var1]) & !is.na(userData$data[var2]),colnames(userData$data) == var1] == att1 & 
                                                   userData$data[!is.na(userData$data[var1]) & !is.na(userData$data[var2]),colnames(userData$data) == var2] == att2,])
                    
                  }
                  print(letter1)
                  print(letter2)
                  text <- paste0(letter1, " = ", att1, "\n")
                  text <- paste0(text, "P(", letter1, ") = Probabilidade de ", var1, " ser ", att1, "\n")
                  text <- paste0(text, "P(", letter1, ") = ", 
                                 freq1,
                                 "/", tot, " = ", round((freq1/tot), 4), " = ", 
                                 scales::percent(round((freq1/tot), 4)),
                                 "\n"
                  )
                  
                  if (letter2 != "NO_INPUT") {
                    text <- paste0(text, "\n", letter2, " = ", att2, "\n",
                                   "P(", letter2, ") = Probabilidade de ", var2, " ser ", att2, "\n",
                                   "P(", letter2, ") = ", freq2,
                                   "/", tot, " = ", round((freq2/tot), 4), " = ", 
                                   scales::percent(round((freq2/tot), 4)),"\n"
                                   
                    )
                    
                    text <- paste0(text, 
                                   "\nP(", letter1, " ∩ ", letter2, ") =  Probabilidade de ", 
                                   var1, " ser ", att1, " e ", var2, " ser ", att2, 
                                   "\nP(", letter1, " ∩ ", letter2, ") = ", freq12, "/", tot, 
                                   " = ", round((freq12/tot), 4), " = ", 
                                   scales::percent(round((freq12/tot), 4)), "\n")
                    
                    if (input$probOp == "uniao") {
                      text <- paste0(text, "\nP(", letter1, " ∪ ", 
                                     letter2, ") = ", "P(", letter1, ") + P(", letter2, ") - P(", letter1, " ∩ ", letter2,
                                     ") = ", round((freq1/tot), 4), " + ", round((freq2/tot), 4), 
                                     " - ", round((freq12/tot), 4), " = ", 
                                     round(((freq1 + freq2 - freq12)/tot), 4), " = ",
                                     scales::percent(round(((freq1 + freq2 - freq12)/tot), 4)), "\n"
                      )
                    }
                  }
                }
                
                
                text
                
              })
              
              
              output$formulasProb <- renderUI({
                formula <- ifelse(input$probOp == "uniao", 
                                  "P(A ∪ B) = $$ P(A) + P(B) - P(A ∩ B) $$",
                                  ifelse(input$probOp == "intersec", 
                                         "P(A ∩ B) = $$\\frac{Freq(A, B)}{Total}$$", 
                                         "P(A) = $$\\frac{Freq(A)}{Total}$$")
                )
                withMathJax(helpText(formula))
              })
              
              output$tabelaCondUI <- renderUI({
                tags <- NULL
                if (sum(userData$shortLevels) >= 2) {
                  tags <- tagList(
                    column(6, selectInput("condTabVar1", "Variável 1", choices = colnames(userData$data)[userData$shortLevels], selected = colnames(userData$data)[userData$shortLevels][1])),
                    column(6, selectInput("condTabVar2", "Variável 2", choices = colnames(userData$data)[userData$shortLevels], selected = colnames(userData$data)[userData$shortLevels][2])),
                    tableOutput("tabelaProbCond")
                  )
                } else {
                  tags <- tagList(
                    column(12, align = "center",
                           p("O conjunto de dados que você escolheu não tem variáveis qualitativas o 
                 suficiente para esta função. Escolha outro conjunto de dados ou utilize nosso 
                 nativo."))
                  )
                }
                
                tags
              })
              
              
              #create frequency table
              output$tabelaProbCond <- renderTable({
                req(input$condTabVar1)
                req(input$condTabVar2)
                req(input$condTabVar1 %in% colnames(userData$data)[userData$shortLevels])
                req(input$condTabVar2 %in% colnames(userData$data)[userData$shortLevels])
                var1 <- input$condTabVar1
                var2 <- input$condTabVar2
                
                tab <- table(userData$data[,colnames(userData$data) == var1], userData$data[,colnames(userData$data) == var2])
                tab <- as.data.frame.matrix(tab)
                tab <- cbind(rownames(tab), tab)
                rownames(tab) <- NULL
                colnames(tab)[1] <- paste0(var1, "/", var2)
                tab$Total <- rowSums(tab[-1])
                tab[,1] <- as.character(tab[,1])
                tab <- rbind(tab, c("Total", colSums(tab[-1])))
                tab[,1] <- as.factor(tab[,1])
                tab
              }, striped = TRUE, bordered = TRUE, width = "80%")
              
              output$caixaContaProbCond <- renderText({
                text <- NULL
                if (sum(userData$shortLevels) >= 2) {
                  req(input$prob_cond1)
                  req(input$prob_cond2)
                  req(input$condTabVar1 %in% colnames(userData$data)[userData$shortLevels])
                  req(input$condTabVar2 %in% colnames(userData$data)[userData$shortLevels])
                  
                  var1 <- input$condTabVar1
                  var2 <- input$condTabVar2
                  att1 <- input$prob_cond1
                  att2 <- input$prob_cond2
                  tot <- nrow(userData$data[!is.na(userData$data[var1]) & !is.na(userData$data[var2]),])
                  
                  freq1 <- nrow(userData$data[userData$data[!is.na(userData$data[var1]) & !is.na(userData$data[var2])
                                                            ,colnames(userData$data) == var1] == att1,])
                  freq2 <- nrow(userData$data[userData$data[!is.na(userData$data[var1]) & !is.na(userData$data[var2]),
                                                            colnames(userData$data) == var2] == att2,])
                  freq12 <- nrow(userData$data[userData$data[!is.na(userData$data[var1]) & !is.na(userData$data[var2]),colnames(userData$data) == var1] == att1 & 
                                                 userData$data[!is.na(userData$data[var1]) & !is.na(userData$data[var2]),colnames(userData$data) == var2] == att2,])
                  
                  text <- paste0("A: ", var1, " é ", att1, "\n")
                  text <- paste0(text, "B: ", var2, " é ", att2, "\n\n")
                  
                  text <- paste0(text, "P(A) = ", freq1, "/", tot, " = ", round((freq1/tot), 4), "\n")
                  text <- paste0(text, "P(B) = ", freq2, "/", tot, "= ", round((freq2/tot), 4), "\n")
                  text <- paste0(text, "P(A ∩ B) = ", freq12, "/", tot, " = ", round((freq12/tot), 4), "\n\n")
                  
                  text <- paste0(text, "P(A | B) = P(A ∩ B)/P(B)\n")
                  text <- paste0(text, "P(A | B) = ", round((freq12/tot), 4), "/", round((freq2/tot), 4), "\n")
                  f <- ((freq12/tot)/(freq2/tot))
                  text <- paste0(text, "P(A | B) = ", round(f, 4), "\n")
                  text <- paste0(text, "P(A | B) = ", round(f*100, 2), ifelse(!is.nan(f), "%\n", ""))
                } else {
                  text <- "Nada a mostrar."
                }
                
                
                text
              })
              
              output$selectProbCondVar <- renderUI({
                tags <- NULL
                if (sum(userData$shortLevels) >= 2) {
                  req(input$condTabVar1, input$condTabVar2)
                  req(input$condTabVar1 %in% colnames(userData$data)[userData$shortLevels])
                  req(input$condTabVar2 %in% colnames(userData$data)[userData$shortLevels])
                  tags <- tagList(
                    #column(6, 
                    selectInput("prob_cond1", "Probabilidade de: ", levels((userData$data[,colnames(userData$data) == input$condTabVar1]))),
                    selectInput("prob_cond2", "dado que: ", levels((userData$data[,colnames(userData$data) == input$condTabVar2])))
                    #)#,
                    #column(6, 
                    #align = "center", withMathJax("P(A | B) = $$\\frac{P(A ∩ B)}{P(B)} $$")
                    #)
                  )
                }
                tags
                
                
              })
              
              
              ######################################################################
              ####################---I N F E R Ê N C I A---#########################
              ######################################################################
              
              observeEvent(input$testeT1Refresh, {
                testeT1_params$u <- input$testeT1MediaPop
                testeT1_params$dp <- input$testeT1DPPop
                testeT1_params$X <- input$testeT1MediaA
                testeT1_params$n <- input$testeT1TamanhoA
                testeT1_params$alpha <- input$testeT1Alpha
              })
              
              output$testeT1Plot <- renderPlot({
                u <- testeT1_params$u
                dp <- testeT1_params$dp
                X <- testeT1_params$X
                n <- testeT1_params$n
                S <- dp/sqrt(n)
                alpha <- testeT1_params$alpha
                x_min <- u - 4*dp
                x_max <- u + 4*dp
                rc1 <- qnorm(alpha/2, mean=u, sd=S)
                rc2 <- qnorm(1-alpha/2, mean=u, sd=S)
                accept <- FALSE
                if (X > rc1 && X < rc2) {
                  accept <- TRUE
                }
                x1 <- c(seq(x_min, rc1, length = 200))
                y1 <- dnorm(x1, mean=u, sd=S)
                data1 <- as.data.frame(cbind(x1, y1))
                colnames(data1) <- c("x", "y")
                
                x2 <- c(seq(rc2, x_max, length=200))
                y2 <- dnorm(x2, mean=u, sd=S)
                data2 <- as.data.frame(cbind(x2, y2))
                colnames(data2) <- c("x", "y")
                
                point <- data.frame(X, dnorm(X, u, S)/2)
                colnames(point) <- c("x", "y")
                
                g <- ggplot(data=data1, aes(x=x, y=y))  + 
                  geom_area(stat="identity", fill="red", alpha = 0.4) + 
                  geom_area(data=data2, aes(x=x, y=y), stat="identity", fill="red", alpha=0.4) +
                  stat_function(fun=dnorm, n=300, args = list(mean=u, sd=S), color = "brown") + 
                  scale_x_continuous(limits = c(x_min, x_max)) + 
                  geom_segment(aes(x=u, xend=u, y=0, yend=dnorm(u, u, S)), color = "gray", linetype=2, alpha=0.6) + 
                  annotate(geom="text", x=u, y=1.1*dnorm(u,u,S), label=paste0("μ = ", u), size=5, color=dark_cyan) + 
                  geom_point(data=point, aes(x=x, y=y), 
                             color = ifelse(accept, "green", "red"), size = 8, shape = 8)
                g
              })
              
              output$testeT1Texto <- renderText({
                u <- testeT1_params$u
                dp <- testeT1_params$dp
                X <- testeT1_params$X
                n <- testeT1_params$n
                S <- dp/sqrt(n)
                alpha <- testeT1_params$alpha
                rc1 <- qnorm(alpha/2, mean=u, sd=S)
                rc2 <- qnorm(1-alpha/2, mean=u, sd=S)
                accept <- FALSE
                if (X > rc1 && X < rc2) {
                  accept <- TRUE
                }
                p <- 2*pnorm(X, u, S, lower.tail = X < u)
                
                text <- paste0("H0: μ = ", u, "\nH1: μ ≠ ", u, 
                               "\n\nX̄ ~ N(μ, σ/sqrt(n))\n",
                               "X̄ ~ N(", u, ", ", round(S, digits = 4), ")\n",
                               "α = P(Cometer Erro Tipo I) = ", alpha, " = ", scales::percent(alpha), "\n",
                               "Região Crítica = RC = ]-∞, ", round(rc1, digits = 4), "] U [", round(rc2, digits = 4), ", ∞[\n"
                )
                
                if (accept) {
                  text <- paste0(text, "X̄̄ ∉ RC, logo H0 é aceita. μ = ", u, "\n")
                } else {
                  text <- paste0(text, "X̄ ∈ RC, logo H0 é rejeitada e a hipótese alternativa H1 é aceita. 
                     μ ≠ ", u, "\n")
                }
                
                text <- paste0(text, "\nValor-p: ", p)
                text
                
              })
              
              
              observeEvent(input$testeT2Refresh, {
                testeT2_params$n <- input$testeT2TamanhoA
                testeT2_params$u1 <- input$testeT2MediaP1
                testeT2_params$u2 <- input$testeT2MediaP2
                testeT2_params$dp1 <- input$testeT2DPP1
                testeT2_params$dp2 <- input$testeT2DPP2
                p1 <- rnorm(testeT2_params$n, testeT2_params$u1, testeT2_params$dp1)
                p2 <- rnorm(testeT2_params$n, testeT2_params$u2, testeT2_params$dp2)
                testeT2_params$p1 <- p1
                testeT2_params$p2 <- p2
                testeT2_params$D <- p1 - p2
              })
              
              output$testeT2Table <- renderTable({
                tab <- as.data.frame(rbind(testeT2_params$p1, testeT2_params$p2, testeT2_params$D))
                tab <- round(tab, 2)
                rownames(tab) <- c("Amostra A1 de P1", "Amostra A2 de P2", "Diferença (A1 - A2)")
                tab
              }, colnames = FALSE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%")
              
              output$testeT2Graph1 <- renderPlot({
                n <- testeT2_params$n
                u1 <- testeT2_params$u1
                u2 <- testeT2_params$u2
                dp1 <- testeT2_params$dp1
                dp2 <- testeT2_params$dp2
                
                p1 <- testeT2_params$p1
                p2 <- testeT2_params$p2
                p <- as.data.frame(cbind(c(p1, p2), c(rep("População 1", n), rep("População 2", n))))
                colnames(p) <- c("x", "pop")
                p$x <- as.numeric(as.character(p$x))
                
                g <- ggplot(data=p, aes(x=x)) + 
                  geom_density(aes(group = pop, color = pop)) + 
                  scale_x_continuous(limits = c(min(u1 - 4*dp1, u2 - 4*dp2), max(u1 + 4*dp1, u2 + 4*dp2))) + 
                  scale_color_discrete(name="População") + 
                  xlab(" ") +
                  ylab("Densidade")
                g
                
              })
              
              output$testeT2Graph2 <- renderPlot({
                u1 <- testeT2_params$u1
                u2 <- testeT2_params$u2
                dp1 <- testeT2_params$dp1
                dp2 <- testeT2_params$dp2
                D <- as.data.frame(testeT2_params$D)
                colnames(D) <- c("x")
                
                g <- ggplot(data=D, aes(x=x)) + 
                  geom_density() + 
                  scale_x_continuous(limits = c(min(u1 - u2, u2 - u1) - 4*(max(dp1, dp2)), max(u1 - u2, u2 - u1) + 4*max(dp1, dp2))) + 
                  xlab("Diferença") + 
                  ylab("Densidade")
                g
                
                
              })
              
              output$testeT2Calc <- renderText({
                D <- testeT2_params$D
                n <- testeT2_params$n
                t <- mean(D)/(sd(D)/sqrt(n))
                p <- dt(t, df=n-1)
                alpha <- input$testeT2Alpha
                print(t.test(D, alternative = "two.sided", mu = 0, conf.level = .95))
                text <- paste0("H0: µ1 = µ2\nH1:µ1 ≠ µ2\n\n")
                
                text <- paste0(text, "Valor do teste = T = ", round(mean(D), 4), "/(", round(sd(D), 4), "/sqrt(", n, ")) = \n
                   ", round(t, 4), "\n\n
                   p-valor = P(T > |", round(t, 4), "| + P(T < - |", round(t, 4), "|) = \n
                   ", round(p, 4))
                
                if (p < alpha) {
                  text <- paste0(text, "\nComo p-valor = ", round(p, 4), " < ", "α = ", alpha, ", rejeita-se a hipótese nula H0 e aceita-se H1:\n
                     µ1 ≠ µ2")
                } else {
                  text <- paste0(text, "\nComo p-valor = ", round(p, 4), " >= ", "α = ", alpha, ", aceita-se a hipótese nula H0:\n
                     µ1 = µ2")
                }
                text 
              })
              
              output$testeT2Graph3 <- renderPlot({
                D <- testeT2_params$D
                n <- testeT2_params$n
                t <- mean(D)/(sd(D)/sqrt(n))
                p <- dt(t, df=n-1)
                alpha <- input$testeT2Alpha
                accept <- p >= alpha
                
                lower_p <- qt(alpha/2, df=n-1)
                upper_p <- qt(1 - alpha/2, df=n-1)
                
                x1 <- seq(-6, lower_p, length = 200)
                x2 <- seq(upper_p, 6, length = 200)
                y1 <- dt(x1, df=n-1)
                y2 <- dt(x2, df=n-1)
                
                data1 <- data.frame(x1, y1)
                data2 <- data.frame(x2, y2)
                
                colnames(data1) <- c("x", "y")
                colnames(data2) <- c("x", "y")
                
                point <- data.frame(t, dt(t, df=n-1)/2)
                colnames(point) <- c("x", "y")
                g <- ggplot(data=data1, aes(x=x, y=y)) + 
                  stat_function(fun=dt, n=101, args=list(df=n-1), color = "black") + 
                  geom_area(data=data1, aes(x=x, y=y), fill="red", alpha=0.4) + 
                  geom_area(data=data2, aes(x=x, y=y), fill="red", alpha=0.4) + 
                  geom_point(data=point, aes(x=x, y=y), 
                             color = ifelse(accept, "green", "red"), size = 8, shape = 8) + 
                  scale_x_continuous(limits = c(-6, 6)) + 
                  ylab("f(x)")
                ggtitle(paste0("Distribuição T de Student com ", n-1, " graus de liberdade"))
                g
                
                
                
              })
              
              
              observeEvent(input$testeQuiRefresh, {
                testeQui_params$matrix <- rbind(c(input$testeQuiValor1, input$testeQuiValor2, input$testeQuiValor1 + input$testeQuiValor2), 
                                                c(input$testeQuiValor3, input$testeQuiValor4, input$testeQuiValor3 + input$testeQuiValor4), 
                                                c(input$testeQuiValor1 + input$testeQuiValor3, input$testeQuiValor2 + input$testeQuiValor4, 
                                                  input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4))
                testeQui_params$colnames <- c(input$testeQuiCol1, input$testeQuiCol2, "Total")
                testeQui_params$rownames <- c(input$testeQuiRow1, input$testeQuiRow2, "Total")
                p <- (input$testeQuiValor1 + input$testeQuiValor3)/
                  (input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4)
                testeQui_params$expected <- rbind(c(round(p*(input$testeQuiValor1 + input$testeQuiValor2)), 
                                                    round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2)),
                                                    input$testeQuiValor1 + input$testeQuiValor2
                ),
                c(round(p*(input$testeQuiValor3 + input$testeQuiValor4)),
                  round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)),
                  input$testeQuiValor3 + input$testeQuiValor4
                ),
                c(input$testeQuiValor1 + input$testeQuiValor3, input$testeQuiValor2 + input$testeQuiValor4, 
                  input$testeQuiValor1 + input$testeQuiValor2 + input$testeQuiValor3 + input$testeQuiValor4))
                testeQui_params$chi <- 
                  ((input$testeQuiValor1 - round(p*(input$testeQuiValor1 + input$testeQuiValor2)))^2)/(round(p*(input$testeQuiValor1 + input$testeQuiValor2))) +
                  ((input$testeQuiValor2 - round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2)))^2)/(round((1-p)*(input$testeQuiValor1 + input$testeQuiValor2))) +
                  ((input$testeQuiValor3 - round(p*(input$testeQuiValor3 + input$testeQuiValor4)))^2)/(round(p*(input$testeQuiValor3 + input$testeQuiValor4))) +
                  ((input$testeQuiValor4 - round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)))^2)/(round((1-p)*(input$testeQuiValor3 + input$testeQuiValor4)))
              })
              
              output$testeQuiTabela <- renderTable({
                tab <- as.data.frame(testeQui_params$matrix)
                colnames(tab) <- testeQui_params$colnames
                rownames(tab) <- testeQui_params$rownames
                
                tab
              }, colnames = TRUE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%", digits = 0, align = "c")
              
              output$testeQuiEsperado <- renderTable({
                tab <- as.data.frame(testeQui_params$expected)
                colnames(tab) <- testeQui_params$colnames
                rownames(tab) <- testeQui_params$rownames
                
                tab
              }, colnames = TRUE, rownames = TRUE, bordered = TRUE, striped = TRUE, width = "100%", digits = 0, align = "c")
              
              output$testeQuiCalc <- renderUI({
                matrix <- testeQui_params$matrix
                exp <- testeQui_params$expected
                chi <- 0
                for (i in c(1, 2, 4, 5)) {
                  chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
                }
                withMathJax(helpText(paste0("$$\\chi^{2} = ", round(chi, 2), "$$")))
              })
              
              output$testeQuiPlot <- renderPlot({
                matrix <- testeQui_params$matrix
                exp <- testeQui_params$expected
                chi <- 0
                rc <- qchisq(1 - input$testeQuiAlpha, 1)
                for (i in c(1, 2, 4, 5)) {
                  chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
                }
                
                accept <- chi < rc
                
                x <- seq(rc, 8, length = 300)
                y <- dchisq(x, df=1)
                
                data <- data.frame(x, y)
                colnames(data) <- c("x", "y")
                
                point <- data.frame(chi, dchisq(chi, df=1)/2)
                colnames(point) <- c("x", "y")
                
                g <- ggplot(data, aes(x=x, y=y)) + 
                  geom_area(stat="identity", fill = "red", alpha = 0.4) + 
                  stat_function(fun=dchisq, n=101, args = list(df = 1), color="black") + 
                  geom_point(data=point, aes(x=x, y=y), color = ifelse(accept, "green", "red"), size = 8, shape = 8) + 
                  scale_x_continuous(limits = c(0, max(7, 1.2*chi))) + 
                  xlab("x") +
                  ylab("f(x)") + 
                  ggtitle("Distribuição Qui-quadrado com 1 grau de liberdade")
                
                g
              })
              
              output$testeQuiConta <- renderText({
                matrix <- testeQui_params$matrix
                exp <- testeQui_params$expected
                chi <- 0
                rc <- qchisq(1 - input$testeQuiAlpha, 1)
                for (i in c(1, 2, 4, 5)) {
                  chi <- chi + (matrix[i] - exp[i])^2/(exp[i])
                }
                
                accept <- chi < rc
                text <- paste0("H0: Não há influência (independente)\nH1: Há influência (não é independente)\n
    Valor-qui = ", round(chi, 4), "\nRegião Crítica RC = [", round(rc, 4), ", +∞[\n")
                if (accept) {
                  text <- paste0(text, "Como o valor-qui está fora da região crítica, aceita-se H0 e portanto 
                     não há influência se ", input$testeQuiRow1, " ou ", input$testeQuiRow2, ".")
                } else {
                  text <- paste0(text, "Como o valor-qui está dentro da região crítica, H0 é rejeitada e portanto H1 é aceita, ou seja,
                     há influência quando ", input$testeQuiRow1, " ou ", input$testeQuiRow2, ".")
                }
                
                text
              })
              
              output$testeCorrExplicacao <- renderUI({
                tags <- NULL
                req(input$tipoTesteCorr)
                if (input$tipoTesteCorr == "spearman") {
                  
                } else {
                  tags <- tagList(
                    withMathJax(helpText("Coeficiente ρ = 
                                                              $$\\frac{1}{n}\\sum_{i=1}^{n}
                                                              (\\frac{x_{i} - \\bar{x}}{dp(X)})
                                                              (\\frac{y_{i} - \\bar{y}}{dp(Y)})$$")),
                    p("ρ = -1: Correlação linear perfeita negativa"),
                    p("ρ = 0: Não há nenhuma relação linear"),
                    p("ρ = 1: Correlação linear perfeita positiva")
                  )
                }
                tags
              })
              
              getCorrPlotData <- reactive({
                req(input$testeCorrVar1)
                req(input$testeCorrVar2)
                req(input$testeCorrVar1 %in% colnames(userData$data))
                req(input$testeCorrVar2 %in% colnames(userData$data))
                return (as.data.frame(cbind(userData$data[,colnames(userData$data) == input$testeCorrVar1], 
                                            userData$data[,colnames(userData$data) == input$testeCorrVar2])))
              })
              
              output$selectTesteCorrVarUI <- renderUI({
                tags <- NULL
                if (sum(userData$numbers) >= 2) {
                  tags <- tagList(
                    selectInput("testeCorrVar1", "Variável 1", 
                                choices = colnames(userData$data)[userData$numbers], 
                                selected = colnames(userData$data)[userData$numbers][1]),
                    selectInput("testeCorrVar2", "Variável 2", 
                                choices = colnames(userData$data)[userData$numbers],
                                selected = colnames(userData$data)[userData$numbers][2])
                  )
                } else {
                  tags <- tagList(p("Os dados que você está utilizando não possuem uma quantidade suficiente de variáveis 
        quantitativas (duas). Por favor selecione outro conjunto de dados ou utilize nosso padrão."))
                }
                
                tags
              })
              
              output$testeCorrPlot <- renderPlot({
                selData <- getCorrPlotData()
                colnames(selData) <- c("Var1", "Var2")
                g <- ggplot(selData, aes(x=Var1, y=Var2)) + 
                  geom_point(color=dark_cyan, size = 2) + 
                  geom_smooth(method=lm) + 
                  xlab(input$testeCorrVar1) + 
                  ylab(input$testeCorrVar2) + 
                  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size = 12))
                g
              })
              
              output$testeCorrConta <- renderText({
                selData <- getCorrPlotData()
                colnames(selData) <- c("Var1", "Var2")
                correlacao <- cor(selData$Var1, selData$Var2, method = input$tipoTesteCorr, use = "all.obs")
                text <- paste0("Correlação(", input$testeCorrVar1, ", ", input$testeCorrVar2, ") = ", correlacao)
                text
              })
              
              
              output$tabelaInformativa <- renderTable({
                tab <- rbind(c("Sexo", "Sexo do aluno: Masculino ou Feminino."),
                             c("Ano letivo", "Ano do curso em que o aluno se encontra. 1 é o primeiro ano e 4, o último."),
                             c("Peso", "Peso do aluno, em kg."),
                             c("Trabalha", "Variável que indica se o aluno trabalha ou não, e se trabalha, se é meio período ou integral."),
                             c("Relacionamento", "Estado de relacionamento do aluno."),
                             c("Cozinha", "Frequência em que o aluno costuma cozinhar, de nunca até sempre."),
                             c("Come fora", "Frequência em que o aluno come fora de casa, em algum restaurante."),
                             c("Percepção de saúde", "Como o aluno classifica, de 1 a 10, sua saúde, sendo 1 péssima e 10 excelente."), 
                             c("Culinária favorita", "Tipo de culinária preferida do aluno"),
                             c("Vegetais nas refeições", "Frequência em que o aluno tem vegetais como parte de suas refeições"),
                             c("Pratica exercícios", "Frequência em que o aluno se exercita."),
                             c("Pratica esportes", "Se o aluno participa de algum esporte ou não."),
                             c("Toma vitaminas", "Se o aluno toma vitaminas."),
                             c("Altura", "Altura do aluno, em metros."))
                tab <- as.data.frame(tab)
                tab
              }, colnames = FALSE, striped = TRUE, bordered = TRUE, width = "100%", align = "c")
              
              ######Glossário
              
              output$glossario_table <- renderDT({
                datatable(glossario_tab1, options = list(dom = 't', autoWidth = TRUE), callback = JS("table.on('init.dt', function(){ 
    $('table').addClass('custom-dt');
  });"))
              }, width = "90%")
              
              
              observe({
                # Se a URL possui um fragmento "glossario", então redirecione para a tab "glossario"
                if (!is.null(input$url_fragment) && input$url_fragment == "glossario") {
                  js$fakeClick("glossario")
                }
              })
              
              pergunta_atual <- reactiveVal(1)
              feedback <- reactiveVal(NULL)
              counter <- reactiveVal(0)
              botao <- reactiveVal(T)
              acertos <- reactiveVal(0)
              erros <- reactiveVal(0)
              
              
              #pergunta
              
              output$pergunta <- renderUI({
                if(pergunta_atual() <= length(perguntas_respostas)){
                  pergunta <- perguntas_respostas[[pergunta_atual()]]
                  HTML(paste('<h4>Questão', pergunta_atual(), ':', pergunta$pergunta, '</p>'))
                } else {
                  HTML('<h3>Erro: Imagem não encontrada.</h3>')
                }
              })
              
              #opções de resposta
              
              output$opcoes_resposta <- renderUI({
                if(pergunta_atual() <= length(perguntas_respostas)){
                  pergunta <- perguntas_respostas[[pergunta_atual()]]
                  radioButtons('resposta', 'Escolha uma opção:', choices = pergunta$respostas)
                }
              })
              
              #verificação da resposta
              
              observeEvent(input$confirmar_resposta, {
                if(pergunta_atual() <= length(perguntas_respostas)){
                  pergunta <- perguntas_respostas[[pergunta_atual()]]
                  resposta_correta <- pergunta$resposta_correta
                  if (input$resposta == pergunta$respostas[resposta_correta]) {
                    feedback('Resposta Correta.')
                    acertos(acertos() + 1)
                    counter(counter() + 1)
                  } else {
                    feedback('Resposta Incorreta.')
                    erros(erros() + 1)
                  }
                  
                  pergunta_atual(pergunta_atual() + 1)
                }
                
                #sumuir com o botao de verificar
                
                if(pergunta_atual() > length(perguntas_respostas)){
                  botao(F)
                }
              })
              
              #counter de questõoes erradas e certas
              output$score <- render_gt({
                data <- data.frame(
                  Categoria = c("Acertos", "Erros"),
                  Quantidade = c(acertos(), erros())
                )
                
                data %>%
                  gt() %>%
                  tab_header(title = "Score")
              })
              
            
              
              #printar resultado
              
              output$resultado <- renderText({
                if(pergunta_atual() <= length(perguntas_respostas)){
                  feedback()
                } else{
                  ''
                }
              })
              
              #printar mensagem final
              
              observe({
                if (pergunta_atual() > length(perguntas_respostas)) {
                  if (counter() == length(perguntas_respostas)) {
                    shinyalert(
                      title = "Parabéns!",
                      text = "Você acertou todas as perguntas.",
                      type = "success"
                    )
                  } else {
                    shinyalert(
                      title = "Resultado",
                      text = paste("Você acertou", counter(), "perguntas."),
                      type = "info"
                    )
                  }
                  botao(FALSE) # Desativar o botão de verificar
                }
              })
              
              observe({
                shinyjs::toggle('confirmar_resposta', condition = botao())
              })
              
              
              
              #linkTexts
              
              
              'observeEvent(input$linkTiposVariaveis, { 
                updateNavbarPage(session, "mainNav", selected = "tipos_variaveis")
              })
              observeEvent(input$linkDistrFreq, { 
                updateNavbarPage(session, "mainNav", selected = "tabTabFreqs")
              })
              observeEvent(input$linkMedidasResumo, { 
                updateNavbarPage(session, "mainNav", selected = "tabMedidasResumo")
              })
              observeEvent(input$linkGrafQual, { 
                updateNavbarPage(session, "mainNav", selected = "tabGrafQual")
              })
              observeEvent(input$linkGrafQuant, { 
                updateNavbarPage(session, "mainNav", selected = "tabGrafQuant")
              })
              observeEvent(input$linkGrafBi, { 
                updateNavbarPage(session, "mainNav", selected = "tabGrafBi")
              })
              observeEvent(input$linkDefProb, { 
                updateNavbarPage(session, "mainNav", selected = "tabDefProb")
              })
              observeEvent(input$linkProbCond, { 
                updateNavbarPage(session, "mainNav", selected = "tabProbCond")
              })
              observeEvent(input$linkDistrProb, { 
                updateNavbarPage(session, "mainNav", selected = "tabDistrProb")
              })
              observeEvent(input$linkTesteT1, { 
                updateNavbarPage(session, "mainNav", selected = "tabTesteT1")
              })
              observeEvent(input$linkTesteT2, { 
                updateNavbarPage(session, "mainNav", selected = "tabTesteT2")
              })
              observeEvent(input$linkTesteQui, { 
                updateNavbarPage(session, "mainNav", selected = "tabTesteQui")
              })
              observeEvent(input$linkTesteCorr, { 
                updateNavbarPage(session, "mainNav", selected = "tabTesteCorr")
              })
              '
             
              ###Exercícios - Paralisia Cerebral
              criar_ui_exercicio <- function(numero_exercicio) {
                enunciado <- enunciados_pc[[numero_exercicio]]
                
                if (!is.null(enunciado)) {
                  if (numero_exercicio %in% c(2, 3, 4, 8, 9, 10, 11, 12)) {
                    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", numero_exercicio)]]
                    fluidRow(
                      column(6,
                             p(HTML(enunciado))
                      ),
                      column(6,
                             h3('Seleção de variáveis e Tipo de Gráfico'),
                             
                             pickerInput('variavel_pc_x', 'Escolha a variável para o eixo x:',
                                         choices = c("", colnames(dados_paralisia))),
                             pickerInput('variavel_pc_y', 'Escolha a variável para o eixo y:',
                                         choices = c("", colnames(dados_paralisia))),
                             pickerInput('tipo_grafico_pc', 'Escolha o tipo de gráfico:',
                                         choices = c("", 'Boxplot', 'Dispersão', 'Barras'))
                      )
                    )
                  }
                  
                  else if (numero_exercicio == 1) {
                    fluidRow(
                      column(6,
                             p(HTML(enunciado))
                      ),
                      column(6,
                             h3('Seleção de variáveis'),
                             
                             pickerInput('variavel_quali', 'Quais variáveis são qualitativas?',
                                         choices = c(colnames(dados_paralisia)), multiple = TRUE),
                             pickerInput('variavel_quanti', 'Quais variáveis são quantitativas?',
                                         choices = c(colnames(dados_paralisia)), multiple = TRUE),
                             pickerInput('medidas_resumo_quali', 'Qual é uma medida resumo adequada para variáveis qualitativas?',
                                         choices = c('Média', 'Porcentagem', 'Mediana')),
                             pickerInput('medidas_resumo_quanti', 'Qual é uma medida resumo adequada para variáveis quantitativas?',
                                         choices = c('Porcentagem', 'Frequência absoluta', 'Média'))
                             
                             
                      )
                    )
                  }
                  
                  else if (numero_exercicio == 5) {
                    fluidRow(
                      column(6,
                             p(HTML(enunciado))
                      ),
                      column(6,
                             h3('Seleção de variáveis'),
                             
                             pickerInput('medidas_resumo_ex5', 'Quais são as medidas-resumo adequadas?',
                                         choices = c('Média', 'Desvio-Padrão','Moda', 'Frequência relativa'), multiple = TRUE)
                             
                             
                      )
                    )
                  }
                  
                  else if (numero_exercicio == 6) {
                    fluidRow(
                      column(6,
                             p(HTML(enunciado)),
                             render_gt(plot_ex6)
                      ),
                      column(6,
                             h3('Seleção de variáveis'),
                             
                             pickerInput('teste_ex6', 'Qual seria o  teste estatístico mais adequado para verificação da hipótese?',
                                         choices = c('Teste t de Student', 'Teste Qui-quadrado de Pearson', 'ANOVA')),
                             uiOutput("hipotese_ui_6"),
                             verbatimTextOutput('resultado_teste_ex_6'),
                             actionButton('calcular_teste', 'Calcular')

                             
                      )
                    )
                  }
                  
                  else if (numero_exercicio == 7) {
                    fluidRow(
                      column(6,
                             p(HTML(enunciado)),
                             render_gt(plot_ex7)
                      ),
                      column(6,
                             h3('Seleção de variáveis'),
                             
                             pickerInput('teste_ex7', 'Qual seria o  teste estatístico mais adequado para verificação da hipótese?',
                                         choices = c('Teste t de Student', 'Teste Qui-quadrado de Pearson', 'ANOVA')),
                             uiOutput("hipotese_ui_7"),
                             verbatimTextOutput('resultado_teste_ex_7'),
                             actionButton('calcular_teste', 'Calcular')
                             
                             
                      )
                    )
                  }
                  
                  else {
                    fluidRow(
                      column(12,
                             p(enunciado)
                      )
                    )
                  }
                } else {
                  NULL
                }
              }
              
              output$hipotese_ui_6 <- renderUI({
                if(input$teste_ex6 == 'Teste Qui-quadrado de Pearson' && input$calcular_teste > 0 && length(calculo_teste()) == 0) {
                  pickerInput('hipotese_ex6', 'A perda auditiva está associada à Paralisia Cerebral?',
                              choices = c('Sim', 'Não'), selected = 'Sim')
                }
                
                else {
                  NULL
                }
              })
              
              output$hipotese_ui_7 <- renderUI({
                if(input$teste_ex7 == 'Teste Qui-quadrado de Pearson' && input$calcular_teste > 0 && length(calculo_teste()) == 0) {
                  pickerInput('hipotese_ex7', 'A perda auditiva está associada à Paralisia Cerebral?',
                              choices = c('Sim', 'Não'), selected = 'Sim')
                }
                else {
                  NULL
                }
              })
                
              
              output$resultado_teste_ex_6 <- renderPrint({
                if(input$teste_ex6 == 'Teste Qui-quadrado de Pearson' && input$calcular_teste > 0 && length(calculo_teste()) == 0) {
                  chisq.test(dados_paralisia$perda_audit, dados_paralisia$grupo)
                }
              })
              
              output$resultado_teste_ex_7 <- renderPrint({
                if(input$teste_ex7 == 'Teste Qui-quadrado de Pearson' && input$calcular_teste > 0 && length(calculo_teste()) == 0) {
                  chisq.test(dados_paralisia$dist_comun, dados_paralisia$grupo)
                }
              })

              
              
              # Plotar UI
              output$opcoes_exercicio_pc <- renderUI({
                exercicio_selecionado <- input$exercicio_pc
                criar_ui_exercicio(as.character(exercicio_selecionado))
              })
              
              validar_opcoes_ex <- reactive({
                msgs_erro <- character(0)
                
                print(input$exercicio_pc)
                req(!is.null(input$exercicio_pc))  
                
                # Verificar se as variáveis estão sendo definidas corretamente
                print(input$variavel_pc_x)
                print(input$variavel_pc_y)
                print(input$tipo_grafico_pc)
                
                if (!is.null(input$variavel_pc_x) && !is.null(input$variavel_pc_y) && !is.null(input$tipo_grafico_pc) && input$exercicio_pc %in% c(2, 3, 4, 8, 9, 10, 11, 12)) {
                  ex <- input$exercicio_pc
                  resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
                  
                  # Verificar se resposta_esperada está sendo definido corretamente
                  print(resposta_esperada)
                  
                  if (input$variavel_pc_x != resposta_esperada$variavel_pc_x) {
                    print("Variável x incorreta")
                    msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo x")
                  }
                  
                  else if (input$variavel_pc_y != resposta_esperada$variavel_pc_y) {
                    print("Variável y incorreta")
                    msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo y")
                  }
                  
                  else if (input$tipo_grafico_pc != resposta_esperada$tipo_grafico_pc) {
                    print("Tipo de gráfico incorreto")
                    msgs_erro <- c(msgs_erro, "Esse não é o tipo de gráfico correto")
                  }
                  
                  else if (input$variavel_pc_x == input$variavel_pc_y) {
                    print("Variáveis iguais")
                    msgs_erro <- c(msgs_erro, 'As variáveis não podem ser iguais')
                  }
                }
                
                else if (input$exercicio_pc == 1){
                  ex <- input$exercicio_pc
                  resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
                  print(resposta_esperada)
                  print(input$variavel_quali)
                  print(input$variavel_quanti)
                  print(input$medidas_resumo_quali)
                  print(input$medidas_resumo_quanti)

                  
                    if (!identical(input$variavel_quali, resposta_esperada$variavel_quali)) {
                      msgs_erro <- c(msgs_erro, "Não foram selecionadas todas as variáveis categóricas")
                  }
                  
                  
                    if (!identical(input$variavel_quanti, resposta_esperada$variavel_quanti)) {
                      msgs_erro <- c(msgs_erro, "Não foram selecionadas todas as variáveis contínuas")
                  }
                  
                  # Verificar se as medidas resumo selecionadas correspondem às esperadas
                  
                    if (input$medidas_resumo_quali != resposta_esperada$medidas_resumo_quali) {
                      msgs_erro <- c(msgs_erro, "Essa não é a medida resumo adequada para variáveis categóricas")
                  }
                  
                  
                    if (input$medidas_resumo_quanti != resposta_esperada$medidas_resumo_quanti) {
                      msgs_erro <- c(msgs_erro, "Essa não é a medida resumo adequada para variáveis contínuas")
                  }
                }
                
                else if (input$exercicio_pc == 5){
                  ex <- input$exercicio_pc
                  resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
                  
                  
                  if (!identical(input$medidas_resumo_ex5, resposta_esperada$medidas_resumo_ex5)) {
                    msgs_erro <- c(msgs_erro, "Não foram selecionadas somente as medidas-resumo adequadas")
                  }
                  
                  
                  
                }
                
                else if (input$exercicio_pc == 6){
                  ex <- input$exercicio_pc
                  resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
                  
                  if(!identical(input$hipotese_ex6, resposta_esperada$hipotese_ex6)){
                    msgs_erro <- c(msgs_erro, 'Essa não é a conclusão correta')
                  }
                }
                
                else if (input$exercicio_pc == 7){
                  ex <- input$exercicio_pc
                  resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
                  
                  if(!identical(input$hipotese_ex7, resposta_esperada$hipotese_ex7)){
                    msgs_erro <- c(msgs_erro, 'Essa não é a conclusão correta')
                  }
                }

                
                else if (length(msgs_erro) > 0) {
                  return(msgs_erro)
                } else {
                  return(NULL)
                }
              })
              
    
              # Renderizar shinyalert
              observeEvent(input$gerar_pc, {
                if (!is.null(validar_opcoes_ex())) {
                  shinyalert(title = "Erro!", text = validar_opcoes_ex(), type = "error")
                }
              })
              
              
              # Renderizar testes estatísticos
              calculo_teste <- reactive({
                msgs_erro_teste <- character(0)
                req(!is.null(input$exercicio_pc))
                
                if(input$exercicio_pc == 6){
                  ex <- input$exercicio_pc
                  resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
                  if (!identical(input$teste_ex6, resposta_esperada$teste_ex6)) {
                    msgs_erro <- c(msgs_erro_teste, "Este não é o teste adequado")
                  }
                }
                
                if(input$exercicio_pc == 7){
                  ex <- input$exercicio_pc
                  resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
                  if (!identical(input$teste_ex7, resposta_esperada$teste_ex7)) {
                    msgs_erro <- c(msgs_erro_teste, "Este não é o teste adequado")
                  }
                }
                
              })
                
              observeEvent(input$calcular_teste, {
                if(!is.null(calculo_teste())){
                  shinyalert(title = 'Erro!', text = calculo_teste(), type = 'error')
                }
              })
              
              
              
              output$grafico_pc <- renderPlot({
                
                if(input$exercicio_pc %in% c(2, 3, 4, 8, 9, 10, 11, 12)){
                  
                  if ( is.null(validar_opcoes_ex())) {
                    exercicio_selecionado <- input$exercicio_pc
                    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", exercicio_selecionado)]]
                    
                    
                    
                    
                    if (exercicio_selecionado == 2){
                      plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, fill = input$variavel_pc_x ))+
                        geom_bar(stat = 'count')
                      req(input$gerar_pc)
                      return(plot)
                    }
                    
                    if (exercicio_selecionado == 3){
                      plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, fill = input$variavel_pc_x ))+
                        geom_bar(stat = 'count')
                      req(input$gerar_pc)
                      return(plot)
                    }
                    
                    if (exercicio_selecionado == 4){
                      plot <- ggplot(dados_paralisia, aes_string(x = 1, y = input$variavel_pc_y))+
                        geom_boxplot()
                      req(input$gerar_pc)
                      return(plot)
                    }
                    
                    
                    if (resposta_esperada$tipo_grafico_pc == 'Boxplot') {
                      plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, y = input$variavel_pc_y)) +
                        geom_boxplot()
                      req(input$gerar_pc)
                      return(plot)
                      
                    }
                    
                    else if (resposta_esperada$tipo_grafico_pc == 'Dispersão') {
                      plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, y = input$variavel_pc_y)) +
                        geom_point()
                      req(input$gerar_pc)
                      return(plot)
                      
                    } else if (resposta_esperada$tipo_grafico_pc == 'Barras') {
                      plot <- ggplot(dados_paralisia, aes_string(x = input$variavel_pc_x, y = input$variavel_pc_y)) +
                        geom_bar()
                      req(input$gerar_pc)
                      return(plot)
                      
                    } else {
                      plot <- NULL
                      
                      
                    }
                    
                    return(NULL)
                    
                  } else {
                    return(NULL)
                  }
                }
              })
                
              
            output$tabela_ex1 <- DT::renderDataTable({
              
              if ( is.null(validar_opcoes_ex())) {
                exercicio_selecionado <- input$exercicio_pc
                resposta_esperada <- respostas_esperadas_pc[[paste0("ex", exercicio_selecionado)]]
                
                if (exercicio_selecionado == 1) {
                  
                  plot <- dados_paralisia %>%
                    tbl_summary(
                      missing = 'no',
                      digits = 
                        list(all_continuous() ~ 1,
                             all_categorical() ~ c(0, 2)),
                      statistic = list(all_continuous() ~ "{mean} ({sd})",
                                       all_categorical() ~ "{n} ({p}%)")
                    ) %>%
                    modify_header(label = '**Variável') %>%
                    modify_footnote(update = starts_with('stat_') ~ 'Média (desvio-padrão) para variáveis numéricas; n (%) para variáveis categóricas')
                  
                  # Converter para formato DT
                  plot_dt <- datatable(
                    plot$table_body,
                    options = list(
                      dom = 't',
                      paging = FALSE,
                      searching = FALSE,
                      ordering = FALSE
                    )
                  )
                  
                  return(plot_dt)
                }
                
                if (exercicio_selecionado == 5) {
                  
                  plot <- dados_paralisia %>%
                    select(td_liquido, td_pastoso, td_solido, grupo) %>%
                    tbl_summary(
                      by = 'grupo',
                      missing = 'no',
                      digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 2)),
                      statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p})%")
                    ) %>%
                    modify_header(label = '**Variável**') %>%
                    modify_footnote(update = starts_with('stat_') ~ 'Média (desvio-padrão) para')
                  
                  # Mantendo apenas as colunas desejadas
                  plot_dt <- datatable(
                    plot$table_body[, c("variable", "label", "stat_0")],
                    options = list(
                      dom = 't',
                      paging = FALSE,
                      searching = FALSE,
                      ordering = FALSE
                    )
                  )
                  
                  return(plot_dt)
                  
                }
                
                }
              
            })
            
              
              
              
              

              
              


            
              
              

          
            }
            
            
              
                          
                         
                         
                  
                            
           
           
            

          
            
          
            
