#' fidlr is an RStudio addin designed to simplify the financial data downloading process.
#' This initial version is a wrapper around the getSymbols function in the quantmod package and only Yahoo, Google, FRED and Oanda are supported.
#' More data providers and functionalities might be added over time.
#' @author Arnaud Amsellem
#' @return Return either a csv file per instrument or a data frame. The csv file is stored in the working directory and the data frame in the globalenv.
#' @title fidlr: FInancial Data LoadeR
#' @export
#' @import shiny miniUI xts zoo quantmod
#' @seealso \code{quantmod}
#' @seealso \code{shiny}

fidlr <- function() {
  requireNamespace("shiny")
  requireNamespace("miniUI")
  requireNamespace("quantmod")

  #### 1 - UI
  ui <- miniPage(
    gadgetTitleBar(" ",
                   right = miniTitleBarButton("run", "Run", primary = TRUE),
                   left = miniTitleBarButton("close", "Close", primary = TRUE)),
    miniTabstripPanel(
      miniTabPanel("Main",icon = icon("download"),
                   miniContentPanel(
                     selectInput("dataSource",
                                 "Source",
                                 choices = c("google",
                                             "yahoo",
                                             "FRED",
                                             "oanda"),
                                 selected = "yahoo"),

                     dateRangeInput("dateRange",
                                    "Date range",
                                    start = Sys.Date()-10,
                                    end   = Sys.Date()),

                     textInput("instrument", "Instrument", " "),

                     radioButtons("csv",
                                  " ",
                                  choices=c("Save as CSV","Save in globalenv"),
                                  selected = "Save in globalenv")
                   )
      ),
      miniTabPanel("Information",icon = icon("info"),
                   div(HTML("This RStudio addin was created by Arnaud Amsellem (<a href='http://thertrader.com' target='_blank'>the R Trader</a>).
                            Please report any bug, comment or suggestion to: thertrader@gmail.com
                            <br>Enjoy!")
                   )
                   )
                   )
                   )

  #### 2 - SERVER
  server <- function(input, output, session) {

    result <- reactive({
      insrumentList <- toupper(gsub(" ", "", input$instrument, fixed = TRUE))
      insrumentList <- c(unlist(strsplit(insrumentList,",")))

      if (input$csv == "Save in globalenv"){
        for (ii in insrumentList){
          getSymbols(Symbols = ii,
                     src = input$dataSource,
                     from = format(input$dateRange[1]),
                     to = format(input$dateRange[2]),
                     env = globalenv())
        }
      }

      if (input$csv == "Save as CSV"){
        for (ii in insrumentList){
          price <- getSymbols(Symbols = ii,
                              src = input$dataSource,
                              from = format(input$dateRange[1]),
                              to = format(input$dateRange[2]),
                              env = NULL,
                              auto.assign = TRUE)
          if(length(grep("/",ii)) == 0)
            insrumentName <- ii
          if(length(grep("/",ii)) > 0)
            insrumentName <- gsub("/","",ii)
          write.zoo(price,paste0(insrumentName,".csv"),sep=",",row.names=FALSE)
        }
      }
    })
    observeEvent(input$run, {result()})
    observeEvent(input$close, {stopApp()})
  }

  #### 3 - RUN GADGET
  runGadget(ui,
            server,
            viewer = dialogViewer("fidlr: FInancial Data LoadeR", width = 330, height = 420))
}



