library(shiny)

shinyServer(
  function(input, output, session) {
      observeEvent(
          input$updateButton,
          {
              system("git clone https://github.com/raytonghk/genepiper.git /srv/shiny-server/genepiper")
              system("mv /srv/shiny-server/genepiper/* /srv/shiny-server")
              system("rm -rf /srv/shiny-server/genepiper")
          }
      )
  }
)
