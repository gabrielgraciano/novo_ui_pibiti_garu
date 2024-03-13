#testandi trstandi
#testando novamente
tags$head(tags$script(HTML('
      var fakeClick = function(tabName) {
        var dropdownList = document.getElementsByTagName("a");
        for (var i = 0; i < dropdownList.length; i++) {
          var link = dropdownList[i];
          if(link.getAttribute("data-value") == tabName) {
            link.click();
          };
        }
      };
    ')))
glossario_tab1 <- read.csv("glossario.csv", stringsAsFactors = FALSE)


glossario <- 
  tabItem(tabName = 'glossario',
          fluidRow(
            h2("Glossário"),
            DTOutput("glossario_table")
          )
  )
           
  
