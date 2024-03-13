graf_qualitativa <-
  tabItem(tabName = 'graf_qualitativa',
          sidebarLayout(
            sidebarPanel(
              uiOutput("uiGrafQual")
            ),
            mainPanel(
              uiOutput("graficosQual"))
          )
  )
