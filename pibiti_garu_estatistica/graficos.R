graf_qualitativa <- sidebarLayout(
  sidebarPanel(
    uiOutput("uiGrafQual")
  ),
  mainPanel(
    uiOutput("graficosQual"))
)
graf_quantitativa <- sidebarLayout(
  sidebarPanel(
    uiOutput("uiGrafQuant")
  ),
  mainPanel(
    uiOutput("graficosQuant")
    
            )
)

graf_bidimensional <- fluidPage(
  uiOutput("biUI")
)