library(shiny); library(rhandsontable); library(shinydashboard)

myHeader <-  dashboardHeader(title = "Coastal OA study")
mySidebar <-  dashboardSidebar(width = 150, disable = FALSE,
    sidebarMenu(
      #Icons from: http://fontawesome.io/icons/ and http://glyphicons.com/
      menuItem("Coastal OA", tabName = "coastalTab", icon = icon("stats", lib="glyphicon")),
      menuItem("Cai data", tabName = "caiTab", icon = icon("ship", lib="font-awesome"))
    ) #close sidebarMenu
  ) #close dashboardSidebar



#Construct the body rows: frowA1 where A = First tab, 1 = first row
frowA1 <- fluidRow(
  column(width = 8,
    box(title = "Ocean surface conditions", status = "danger", solidHeader = TRUE, width = NULL, collapsible = TRUE,
      helpText(HTML("Units: <b>T</b>emperature, &deg;C; &nbsp;&nbsp;  <b>S</b>alinity; &nbsp;&nbsp;  <b>D</b>issolved <b>O</b>xygen, &mu;mol kg<sup>-1</sup>; &nbsp;&nbsp;  <b>T</b>otal <b>A</b>lkalinity, &mu;mol kg<sup>-1</sup>")), 
      helpText("Ocean names must be unique."), 
      rHandsontableOutput("hot")
  )),
  column(width = 4,
   fluidRow(
     box(title = "RCP colors", status = "warning", solidHeader = TRUE, width = NULL, collapsible = TRUE, collapsed = TRUE,
         uiOutput("colorpick.RCP"),
         footer = HTML("RCP 0 = present day 400 	&mu;atm")
     ),
     box(title = "Ocean colors", status = "warning", solidHeader = TRUE, width = NULL, collapsible = TRUE, collapsed = TRUE,
         uiOutput("colorpick.oceans")
     )
   ))) #close fluidRow> column > fluidRow

frowA2 <- fluidRow(
    box(title = "Plot Options", status = "primary", solidHeader = TRUE, width = 4, collapsible = TRUE,
        uiOutput("plotA_Options")
    ),
    box(title = "OA with depth", status = "primary", solidHeader = TRUE, width = 8, collapsible = TRUE,
        plotOutput("plotA"),#, click = "plotA_click", dblclick = "plotA_dblclick", brush = brushOpts(id = "plotA_brush", resetOnNew = TRUE)),
        fluidRow(column( 6, HTML("")), #blank row
                 column( 3, downloadButton(outputId = "Download_plotA", label="Download Plot") ),
                 column( 3, selectInput(inputId = "plotA_filetype", label = NULL, choices =c("eps", "jpeg", "pdf", "png"), selected = "pdf", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)))
    ))
frowA3 <- fluidRow(
  box(title = "Plot Options", status = "info", solidHeader = TRUE, width = 4, collapsible = TRUE,
      uiOutput("plotB_Options")
  ),
  box(title = "OA at surface", status = "info", solidHeader = TRUE, width = 8, collapsible = TRUE,
      plotOutput("plotB"),#, click = "plotB_click", dblclick = "plotB_dblclick", brush = brushOpts(id = "plotB_brush", resetOnNew = TRUE)),
      fluidRow(column( 6, HTML("")), #blank row
               column( 3, downloadButton(outputId = "Download_plotB", label="Download Plot") ),
               column( 3, selectInput(inputId = "plotB_filetype", label = NULL, choices =c("eps", "jpeg", "pdf", "png"), selected = "pdf", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)))
  ))
frowA4 <- fluidRow(
  box(title = "Troubleshooting", status = "success", solidHeader = TRUE, width = 12,
      verbatimTextOutput("T1"),
      helpText("hv[['DF']]"), br(),
      #tableOutput("T2"), 
      br()
  ))

frowB1 <- fluidRow(
    box(title = "Filter data", status = "info", solidHeader = TRUE, width = 5,
        #          #Allow users to upload data
        #          helpText("Upload data"),br(),
        #          fluidRow(fileInput("inputFile", "1) Browse for file"),  #Upload button
        #                  tags$script('$( "#inputFile" ).on( "click", function() { this.value = null; });')),
        helpText("Select data and model parameters"), br(),
        uiOutput("pr_plotOptions"),
        br(),
        sliderInput("salinityRange", "Salinity Range:",  min = 0, max = 40, value = c(0,40)),
        sliderInput("depthRange", "Depth Range (dbar):",  min = 0, max = 5000, value = c(0,5000)),
        #dateRangeInput(inputId = "dateRange", label = "Date Range (NOT USED HERE)", start = "2006-01-01", end = "2016-12-31", min = "2006-01-01", max = "2016-12-31", format = "yyyy-mm-dd", startview = "year", weekstart = 0, language = "en", separator = " to ", width = NULL),
        selectizeInput(inputId = "months", label = "Months", choices = 1:12, selected = 1:12, multiple = TRUE), br(),
        downloadButton("downloadCaiData", "Download CSV")
    ),
    box(title = "Plot", status = "success", solidHeader = TRUE, width = 7, height = 760,
        plotOutput("plotC")
    ))
frowB2 <- fluidRow(
  box(title = "pH actual and modeled data", color = "purple", solidHeader = TRUE, width = 6,
      plotOutput("plotD", click = "plotD_click"),#, dblclick = "plotD_dblclick", brush = brushOpts(id = "plotD_brush", resetOnNew = TRUE)),
      fluidRow(column( 4, HTML("")), #blank row
               column( 4, downloadButton(outputId = "Download_plotD", label="Download Plot") ),
               column( 4, selectInput(inputId = "plotD_filetype", label = NULL, choices =c("eps", "jpeg", "pdf", "png"), selected = "pdf", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)))
      
  ),
  box(title = HTML("Omega[arag] actual and modeled data"), color = "purple", solidHeader = TRUE, width = 6,
      plotOutput("plotE", click = "plotE_click"),#, dblclick = "plotE_dblclick", brush = brushOpts(id = "plotE_brush", resetOnNew = TRUE)),
      fluidRow(column( 4, HTML("")), #blank row
               column( 4, downloadButton(outputId = "Download_plotE", label="Download Plot") ),
               column( 4, selectInput(inputId = "plotE_filetype", label = NULL, choices =c("eps", "jpeg", "pdf", "png"), selected = "pdf", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)))
      
  ))
frowB3 <- fluidRow(
  box(title = "Filtered Cai data", color = "orange", width = 12,
      actionButton("pr_resetSingleClick", label = "Clear selection"),
      DT::dataTableOutput("prDT", width = "90%")
  ))

myBody <-  dashboardBody(
  #Custom CSS
  tags$head(tags$style(HTML(
    ".shiny-html-output th,td {
    border: 1px solid black;
    border-spacing: 2px;
    padding: 4px;
    text-align: center;
    }
    #rcp-colorcol .palette-square {
    width: 100px;
    padding: 0px;
    border-spacing: 0px;
    }"
    ))),
  
  tabItems(
    tabItem(tabName = "coastalTab",
      frowA1, frowA2, frowA3, frowA4
      ),
    tabItem(tabName = "caiTab",
      frowB1, frowB2, frowB3
    ) 
  )) #close tabItems and dashboardBody

dashboardPage(myHeader, mySidebar, myBody)