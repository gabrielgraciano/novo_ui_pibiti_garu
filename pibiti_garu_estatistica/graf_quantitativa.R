graf_quantitativa <-
  tabItem(tabName = 'graf_quantitativa',
          sidebarLayout(
            sidebarPanel(
              uiOutput("uiGrafQuant")
            ),
            mainPanel(
              uiOutput("graficosQuant")
              
            )
          )
  )
