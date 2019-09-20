library(shiny)

shinyUI(
    fluidPage(
        theme = "../16S/style.css",
        tags$script(type = "text/javascript", src = "../16S/www/busy.js"),
        tags$script(type = "text/javascript", src = "../16S/www/controls.js"),
        tags$script(type = "text/javascript", charset = "utf-8",
                    "function button_content_onclick(name) {parent.postMessage(name, '*');}"),
        tags$div(class = "busy", p("Busy..."), img(src = "../16S/www/hour-glass.gif")),
        tags$style("#button-bar {display: flex; justify-content: center;}
                   .button-container {position: relative; display: inline; margin: 10px; 
                   height: 400px; width: 400px;}
                   img {height: 100%; width: 100%;}
                   .img-top {position: absolute; bottom: 0; left: 0;}
                   .button-container:hover .img-top {display: none;}
                   #button-update-container {position: fixed; bottom: 70px; right: 10px;}"),
        
        fluidRow(
            tags$div(
                tags$div(
                    id = "button-bar",
                    tags$div(
                        class = "button-container",
                        tags$img(src = '../www/button_16S_hover.png', alt="", onclick="button_content_onclick('content_16S')"),
                        tags$img(src = '../www/button_16S.png', alt="", class = "img-top")
                    )
                ),
                tags$div(
                    id = "button-update-container",
                    actionButton("updateButton", "Update GenePiper")
                )
            )
        )
    )  
)
