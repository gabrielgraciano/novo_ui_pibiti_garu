#output$opcoes_exercicio_pc <- renderUI({
 # if(input$exercicio_pc == 9) {
  #  fluidRow(
   #   column(6,
    #         p(HTML('Faça um gráfico para o tempo de deglutição de alimentos
     #                pastosos pelo grupo de crianças.
      #               <br>
       #              Há indícios de que o tempo de deglutição de alimentos
        #             pastosos esteja associado à PC?
         #            <br>
          #           Verifique sua observação com um teste estatístico apropriado
           #          e nível de significância de 5%.'
            # )
             #),
             #h3('Seleção de variáveis e Tipo de Gráfico'),
             #
             #selectInput('variavel_pc_9x', 'Escolha a variável para o eixo x:',
              #           choices = colnames(dados_paralisia)),
             #
             #selectInput('variavel_pc_9y', 'Escolha a variável para o eixo y:',
              #           choices = colnames(dados_paralisia)),
             #
             #selectInput('tipo_grafico_pc9', 'Escolha o tipo de gráfico:',
              #           choices = c('Boxplot', 'Gráfico de dispersão',
               #                      'Gráfico de pizza'))
      #)
    #)
  #} else{
   # NULL
  #}
#})


#validar_opcoes_ex9 <- reactive({
 # msgs_erro <- character(0)
  
  
  #if (!is.null(input$variavel_pc_x) && !is.null(input$variavel_pc_y) && !is.null(input$tipo_grafico_pc9)) {
   # if (input$variavel_pc_x != 'grupo' && input$variavel_pc_x != 'td_pastoso') {
    #  msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo x")
    #}
    
    #if (input$variavel_pc_y != 'td_pastoso' && input$variavel_pc_y != 'grupo') {
     # msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo y")
    #}
    
    #if (input$tipo_grafico_pc != 'Boxplot') {
     # msgs_erro <- c(msgs_erro, "Esse não é o tipo de gráfico correto")
    #}
    
    #if(input$variavel_pc_x == input$variavel_pc_y){
     # msgs_erro <- c(msgs_erro, 'As variáveis não podem ser iguais')
    #}
    
  #} else {
   # msgs_erro <- c(msgs_erro, "Selecione alguma opção")
    
    
  #}
  
  #if (length(msgs_erro) > 0) {
   # return(msgs_erro)
  #} else {
   # return(NULL)
  #}
#})


# Renderizar shinyalert
#observeEvent(input$gerar_pc, {
 # if (!is.null(validar_opcoes_ex9())) {
  #  shinyalert(title = "Erro!", text = validar_opcoes_ex9(), type = "error")
  #}
#})

#plotar graficos

#output$grafico_pc <- renderPlot({
 # if (is.null(validar_opcoes_ex9())) {
  #  dados_paralisia %>% 
   #   ggplot(aes_string(x = input$variavel_pc_x, y = input$variavel_pc_y)) +
    #  geom_boxplot()
#  } else {
 #   NULL
  #}
#})



#Validação de opções - gráficos
validar_opcoes_ex <- reactive({
  msgs_erro <- character(0)
  
  req(!is.null(input$exercicio_pc))  
  
  if (!is.null(input$variavel_pc_x) && !is.null(input$variavel_pc_y) && !is.null(input$tipo_grafico_pc)) {
    ex <- input$exercicio_pc
    
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    
    if (input$variavel_pc_x != resposta_esperada$variavel_pc_x) {
      msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo x")
    }
    
    if (input$variavel_pc_y != resposta_esperada$variavel_pc_y) {
      msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo y")
    }
    
    if (input$tipo_grafico_pc != resposta_esperada$tipo_grafico_pc) {
      msgs_erro <- c(msgs_erro, "Esse não é o tipo de gráfico correto")
    }
    
    if (input$variavel_pc_x == input$variavel_pc_y) {
      msgs_erro <- c(msgs_erro, 'As variáveis não podem ser iguais')
    }
  } else {
    msgs_erro <- c(msgs_erro, "Selecione alguma opção")
  }
  
  if (length(msgs_erro) > 0) {
    return(msgs_erro)
  } else {
    return(NULL)
  }
})










# Função para criar o UI correspondente ao exercício
# Função para criar o UI correspondente ao exercício
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



# Plotar UI
output$opcoes_exercicio_pc <- renderUI({
  exercicio_selecionado <- input$exercicio_pc
  criar_ui_exercicio(as.character(exercicio_selecionado))
})


# Validação de opções - gráficos
validar_opcoes_ex <- reactive({
  msgs_erro <- character(0)
  
  print(input$exercicio_pc)
  
  req(!is.null(input$exercicio_pc))  
  
  # Verificar se as variáveis estão sendo definidas corretamente
  print(input$variavel_pc_x)
  print(input$variavel_pc_y)
  print(input$tipo_grafico_pc)
  
  if (!is.null(input$variavel_pc_x) && !is.null(input$variavel_pc_y) && !is.null(input$tipo_grafico_pc)) {
    ex <- input$exercicio_pc
    resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
    
    # Verificar se resposta_esperada está sendo definido corretamente
    print(resposta_esperada)
    
    if (input$variavel_pc_x != resposta_esperada$variavel_pc_x) {
      print("Variável x incorreta")
      msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo x")
    }
    
    if (input$variavel_pc_y != resposta_esperada$variavel_pc_y) {
      print("Variável y incorreta")
      msgs_erro <- c(msgs_erro, "Essa não é a variável correta para o eixo y")
    }
    
    if (input$tipo_grafico_pc != resposta_esperada$tipo_grafico_pc) {
      print("Tipo de gráfico incorreto")
      msgs_erro <- c(msgs_erro, "Esse não é o tipo de gráfico correto")
    }
    
    if (input$variavel_pc_x == input$variavel_pc_y) {
      print("Variáveis iguais")
      msgs_erro <- c(msgs_erro, 'As variáveis não podem ser iguais')
    }
  } else {
    msgs_erro <- c(msgs_erro, "Selecione alguma opção")
  }
  
  if (length(msgs_erro) > 0) {
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


# validar exercício 1
validar_opcoes_ex1 <- reactive({
  msgs_erro_ex1 <- character(0)
  
  req(!is.null(input$exercicio_pc))  # Certificar-se de que o exercício está selecionado
  
  ex <- input$exercicio_pc
  
  resposta_esperada <- respostas_esperadas_pc[[paste0("ex", ex)]]
  
  # Verificar se as variáveis selecionadas correspondem às esperadas
  if (!is.null(input$variavel_quali)) {
    if (!identical(input$variavel_quali, resposta_esperada$variavel_quali)) {
      msgs_erro_ex1 <- c(msgs_erro_ex1, "Não foram selecionadas todas as variáveis categóricas")
    }
  }
  
  if (!is.null(input$variavel_quanti)) {
    if (!identical(input$variavel_quanti, resposta_esperada$variavel_quanti)) {
      msgs_erro_ex1 <- c(msgs_erro_ex1, "Não foram selecionadas todas as variáveis contínuas")
    }
  }
  
  # Verificar se as medidas resumo selecionadas correspondem às esperadas
  if (!is.null(input$medidas_resumo_quali)) {
    if (input$medidas_resumo_quali != resposta_esperada$medidas_resumo_quali) {
      msgs_erro_ex1 <- c(msgs_erro_ex1, "Essa não é a medida resumo adequada para variáveis categóricas")
    }
  }
  
  if (!is.null(input$medidas_resumo_quanti)) {
    if (input$medidas_resumo_quanti != resposta_esperada$medidas_resumo_quanti) {
      msgs_erro_ex1 <- c(msgs_erro_ex1, "Essa não é a medida resumo adequada para variáveis contínuas")
    }
  }
  
  if (length(msgs_erro_ex1) > 0) {
    return(msgs_erro_ex1)
  } else {
    return(NULL)
  }
})



#Renderizar shinyalert ex1
observeEvent(input$gerar_pc, {
  if (!is.null(validar_opcoes_ex1())) {
    shinyalert(title = "Erro!", text = validar_opcoes_ex1(), type = "error")
  }
})




#plotar graficos
#BACkup
output$grafico_pc <- renderPlot({
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
})



library(DT)
library(dplyr)


all_co
