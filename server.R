###############################
###  Initial inforamation   ###
###############################
# server.R
library(shiny); library(shinydashboard); library(rhandsontable); library(DT); library(shinyjs)
library(data.table)
library(seacarb); library(marelac)
library(ggplot2); library(gridExtra); library(viridis)
#library(dplyr); library(grid); library(cowplot)

#Declare/upload variables
Redfield.C.to.O <- 106/138
RCPz <- data.frame(RCP = c(0, 2.6, 4.5, 6, 8.5), pCO2atm = c(400L, 430L, 550L, 750L, 910L), color = c("black", "#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")) #Make sure df is sorted in ascending order
  RCPz$color <- as.character(RCPz$color)
#  "FOLDER/FILE" the filepath on the server is "//srv/shiny-server/coastalOA/helper"
  #Note: filenames are case-sensitive!
allvar.labels <-  readRDS("helper/allvar.labels.Rds") #Load variable graphing labels. Opposite of saveRDS()
modOA <- readRDS(file="helper/cai_dd1_S33.5_TA2300.Rda")
wc2 <- readRDS(file="helper/wc2.Rda")

  
#Create plotting functions
makePlot <- function(data, x, y, color, colorIsFactor = TRUE, colorValues = NULL, colorLabels= NULL, facet = NULL, myxlim = NULL, myylim =NULL, extra_ggplot = NULL) {
  if (colorIsFactor) { myColor <- paste0("factor(", color, ")")} else { myColor <- color }
  if (!is.null(x) & !is.null(y)){
    myPlot <-  ggplot(data, aes_string(x, y, colour = myColor, group = myColor)) +   theme_classic(base_size = 16) +
      theme(axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(color="black", size = 1), strip.background = element_rect(colour = alpha("white", alpha = 0), fill = "white", size=0), legend.key = element_blank()) +
      geom_line(size = 1) + labs(y = bquote( .( eval(parse(text = allvar.labels[allvar.labels$var == y, "selectable"])))), x = bquote( .( eval(parse(text = allvar.labels[allvar.labels$var == x, "selectable"])))), colour = color)+ 
      theme(legend.position = "bottom", legend.background = element_rect(fill=alpha('white', 0)), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + extra_ggplot + 
      if(!is.null(facet)) { facet_grid(reformulate(facet,'.' )) } else {NULL}
    if (!is.null(colorValues)) { 
      myPlot <- myPlot + scale_colour_manual(values = colorValues)
      if(!is.null(colorLabels)) {
        myPlot <- myPlot + scale_colour_manual(values = colorValues, labels = colorLabels)
    }}
    myPlot_zoom <- myPlot + coord_cartesian(xlim = myxlim, ylim = myylim)
  }
  YvX <- paste0(y,"-",x) #Used for plot name
  return(list(myPlot, myPlot_zoom, YvX))    #Do not print plots if you want them to be interactive  
}

shinyServer(function(input, output) {

##############################
###  Initial shiny setup   ###
##############################
#Coastal OA tab
#Construct special colorpicker tables
output$colorpick.RCP <- renderUI({
  rowz <- list()    #create table cells
  #Fill out table cells [i,j] with static elements
  for( i in 1:nrow( RCPz )) {
    rowz[[i]] <- tags$tr(lapply( RCPz[i, 1:ncol(RCPz)], function( x ) {  
      tags$td( HTML(as.character(x)) )  
    }) )
  }
  #Add colourInput() to cells in the "Select" column in myHTMLtable
  for( i in 1:nrow( RCPz ) ) {
    #Note: in the list rowz:
    #  i = row; [3] = row information; children[1] = table cells (list of 1); $Select = Column 'Select' 
    rowz[[i]][3]$children[[1]]$color <- tags$td(
      colourpicker::colourInput(inputId = as.character(paste0("Colour.RCP", i)), label = NULL, value = as.character(RCPz$color[i]))
    ) 
  } 
  mybody <- tags$tbody( rowz )
  tags$div(id = "rcp-colorcol", list(
    tags$table( 
      tags$thead( 
        tags$tr(lapply( c("RCP", "pCO<sub>2</sub>, &mu;atm", "color"), function( x ) tags$th( HTML(x) ) ) )
      ),
      mybody
    ) #close tags$table
  )) #close tags$div
}) #close renderUI
output$colorpick.oceans <- renderUI({
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  } #http://stackoverflow.com/a/8197703/4718512
  
  oceanz <- data.frame(ocean = sort(unique(hv[["DF"]]$ocean)), color = gg_color_hue(length(unique(hv[["DF"]]$ocean)))) #Sorting is critical here else colors won't match in plots!
  rowz <- list()    #create table cells
  #Fill out table cells [i,j] with static elements
  for( i in 1:nrow( oceanz )) {
    rowz[[i]] <- tags$tr(lapply( oceanz[i, 1:ncol(oceanz)], function( x ) {  
      tags$td( HTML(as.character(x)) )  
    }) )
  }
  #Add colourInput() to cells in the "Select" column in myHTMLtable
  for( i in 1:nrow( oceanz ) ) {
    #Note: in the list rowz:
    #  i = row; [3] = row information; children[1] = table cells (list of 1); $Select = Column 'Select' 
    rowz[[i]][3]$children[[1]]$color <- tags$td(
      colourpicker::colourInput(inputId = as.character(paste0("Colour.ocean", i)), label = NULL, value = as.character(oceanz$color[i]))
    ) 
  } 
  mybody <- tags$tbody( rowz )
  tags$div(id = "rcp-colorcol", list(
    tags$table( 
      tags$thead( 
        tags$tr(lapply( c("ocean", "color"), function( x ) tags$th( HTML(x) ) ) )
      ),
      mybody
    ) #close tags$table
  )) #close tags$div
}) #close renderUI                     
  
#Initial water chemistry conditions  
dd.initial <- data.table(ocean = c("Gulf of Mexico", "West Coast"),
                   T = c(26.4, 11.8), S = c(30.6, 32.7), DO = c(190, 280), TA=c(2381.6, 2218.4))

####################################
###  Calculations and Elements   ###
####################################
#Handsontable
  hv <- reactiveValues() #hv = hot (hands-on-table) values
  observe({ 
    if(is.null(hv[["DF"]])) { 
      hv[["DF"]] <- dd.initial
    }
  })
  observe({
    if(!is.null(input$hot)) {
      hv[["DF"]] <- hot_to_r(input$hot)
    }
  })
      
  #Create rhandsontable
  output$hot <- renderRHandsontable({
    tempdf <- hv[["DF"]]
    if(!is.null(tempdf)) {
      rhandsontable(tempdf)
    }
  })
  output$T1 <- renderPrint(hv_allRowsNotNA$check)
  output$T2 <- renderTable(rdt() ) #renderTable(hv[["DF"]])  #Troubleshooting

hv_allRowsNotNA <- reactiveValues(check = 0L)
observeEvent(hv[["DF"]], { hv_allRowsNotNA$check <- hv_allRowsNotNA$check + all(!is.na(hv[["DF"]])) })  
  
#Calculate carbonate chemistry changes over respiration (~ depth)
rdt <- eventReactive(hv_allRowsNotNA$check, {
  dd <- hv[["DF"]]
  dd <- dd[, .(ocean, T, S, TA, pCO2atm = rep(c(400, 430, 550, 750, 910), each= length(unique(dd$ocean)) ) )] #create data
  zz <- data.table(pCO2atm = c(400, 430, 550, 750, 910), RCP = c(0, 2.6, 4.5, 6, 8.5)) #RCP 0 = present day
  setkey(dd, pCO2atm)
  dd <- dd[zz]; rm(zz) #Join inner to outer
  dd[, pCO2i := x2pCO2(S= S, T= T, Patm=1.0, xCO2= pCO2atm ) ] #pCO2 in uatm
  #1) calculate O2i. Atmospheric O2 assumed 20.9%. Results in umol kg-1 = (mmol m-3 / (kg m-3) * 1000 umol mmol-1)
  dd[, c("O2i") := as.data.table(marelac::gas_satconc(S = S, t = T, P = 1, species =c("O2"), 
                                                      atm = c(atmComp(c("O2"))))/sw_dens(S = S, t = T)*1000), 
     by = pCO2atm] #atm argument can only take a vector of values matching species. Therefore, do each calculation separate for each pCO2 level
  dd[, O2sat := 100] #Model O2 as a percentage to put colder temperatures on the same scale as warmer temperatures
  #3) calculate DICi. Set TA here.
  dd[, c("DICi", "pHi", "CO2i", "HCO3i", "CO3i", "Aragi", "Calcitei") := carb(flag = 24, var1 = pCO2i, var2 = TA* 1e-6, S= S, T = T, P=0, Patm = 1, Pt = 0e-6, Sit = 0e-6)[, c("DIC", "pH", "CO2", "HCO3", "CO3", "OmegaAragonite", "OmegaCalcite")]]
  dd[, RFi := buffesm(flag = 24, var1 = pCO2i, var2 = TA* 1e-6, S= S, T = T, P=0, Patm = 1, Pt = 0e-6, Sit = 0e-6)$R]
  dd[, c("betaDICi", "omegaDICi") := buffesm(flag = 24, var1 = pCO2i, var2 = TA* 1e-6, S= S, T = T, P=0, Patm = 1, Pt = 0e-6, Sit = 0e-6)[, c("betaDIC", "omegaDIC")]]
  system.time(dd[, c("DICi", "CO2i", "HCO3i", "CO3i", "betaDICi", "omegaDICi") := lapply(.SD, function(zz) zz*1e6), .SDcols = c("DICi", "CO2i", "HCO3i", "CO3i")]) #convert to umol kg-1
  #4) calculate dO2. First make a list of O2f from 0 to O2i
  #Add rows from 0 to O2i, recycling T, S, and O2i
  dt <- dd[, list(ocean, T , S, TA, pCO2atm, O2f = c(seq(0, O2i, by = 10), O2i) ), by=1:nrow(dd)]
  setkey(dd, NULL) #Reset smaller table key to get ready for the join
  setkey(dt, ocean, T, S, TA, pCO2atm) #Set keys of larger data.table to allow the merge in the next step
  dt <- dt[dd]   #All remaining data after NA-filtering the important columns. #Equivalent to merge(dd, dt, all.x = TRUE)
  dt[, c("dO2", "O2sat") := .(O2i - O2f, O2f/O2i*100)]
  dt[, deltaCO2 := dO2 * (Redfield.C.to.O) ] #5) calculate dCO2 = dO2 * (106/138) / (1e6 umol mmol-1) in umol kg-1 or (C:O) 117/170 from Anderson & Sarmiento 1994
  dt[, DICf := DICi + deltaCO2] #6) calculate DICf = DICi + dCO2
  #7) calculate carbonate chemistry
  newColNames <- c("pCO2", "pH", "CO2", "HCO3", "CO3", "Arag", "Calcite", "RF")
  seacarbOuts <- c("pCO2pot", "pH", "CO2", "HCO3", "CO3", "OmegaAragonite", "OmegaCalcite")
  #at surface: T.surf, S.surf, P=0
  dt[, paste0(head(newColNames, -1)) := carb(flag = 15, var1= TA*1e-6,      var2= DICf*1e-6, S= S,      T= T,      P= 0/10,      Patm= 1, Pt= 0e-6, Sit= 0e-6)[, seacarbOuts]]
  dt[,                RF := buffesm(flag = 15, var1= TA*1e-6,      var2= DICf*1e-6, S= S,      T= T,      P= 0/10,      Patm= 1, Pt= 0e-6, Sit= 0e-6)$R]
  dt[, c("betaDIC", "omegaDIC") := buffesm(flag = 15, var1= TA*1e-6,      var2= DICf*1e-6, S= S,      T= T,      P= 0/10,      Patm= 1, Pt= 0e-6, Sit= 0e-6)[, c("betaDIC", "omegaDIC")]]
  dt[, nrow := NULL]
  #convert mol kg-1 to umol kg-1
  for(j in which(names(dt) %in% c("CO2", "HCO3", "CO3", "betaDIC", "omegaDIC"))) { set(dt, i=NULL, j, value= dt[[j]] * 1e6) }
  dt[, paste0("d", newColNames) := list(pCO2 - pCO2i, pH - pHi, CO2 - CO2i, HCO3 - HCO3i, CO3 - CO3i, Arag-Aragi, Calcite - Calcitei, RF - RFi)] 
  dt[, c("dbetaDIC", "domegaDIC") := list(betaDIC - betaDICi, omegaDIC - omegaDICi)] 
  setorder(dt, ocean, RCP, O2sat) #order rows
  setcolorder(dt, c("ocean", "RCP", "T", "S", "pCO2atm", "TA", "DICi", "O2i", "O2f", "O2sat", "dO2", "deltaCO2", "DICf", 
                    c( paste0(newColNames, "i"), "betaDICi", "omegaDICi", newColNames, "betaDIC", "omegaDIC", paste0("d", newColNames), "dbetaDIC", "domegaDIC")) )
  return(dt)
}   ) 

#Download data
output$download_rcp_dO2_oceans <- downloadHandler( filename = function() {  paste0("rcp_respiration_oceans_", format(Sys.Date(), "%Y%m%d"), ".csv") },
  content = function(file) { write.table(rdt(), file, sep=",", row.names=FALSE)  })  

#plotA = RCP-depth ~ ocean plots
#Reactive UI for plotting
output$plotA_Options <- renderUI({
  #if(!is.null(input$inputFile)) {
  {
    namez <- isolate({ names(rdt())[!(names(rdt()) %in% c("ocean", "RCP", "T", "S", "pCO2atm", "TA", paste0(c("O2", "DIC", "pCO2", "pH", "CO2", "HCO3", "CO3", "Arag", "Calcite", "RF", "betaDIC", "omegaDIC"), "i")))] })
    flowLayout(
      selectInput(inputId = "x_rcpO2", label = "X", choices = namez, selected = "O2sat" ),
      selectInput(inputId = "y_rcpO2", label = "Y", choices = namez, selected = "pH" ),
      br(),
      downloadButton("download_rcp_dO2_oceans", "Download CSV")
    )} })

#Interactive plots
brush.ranges <- reactiveValues(plotA_x = NULL, plotA_y = NULL, plotB_x= NULL, plotB_y= NULL, plotC_x= NULL, plotC_y= NULL, plotD_x= NULL, plotD_y= NULL, plotE_x= NULL, plotE_y= NULL)
# When a double-click happens, check if there's a brush on the plot. If so, zoom to the brush bounds; if not, reset the zoom.
observeEvent(input$plotA_dblclick, {
  brush <- input$plotA_brush
  if (!is.null(brush)) {
    brush.ranges$plotA_x <- c(brush$xmin, brush$xmax)
    brush.ranges$plotA_y <- c(brush$ymin, brush$ymax)
    
  } else {
    brush.ranges$plotA_x <- NULL
    brush.ranges$plotA_y <- NULL
  }
})

#plot_colors <- reactiveValues(rcp = RCPz$color)
#plot_colors$rcp <- reactive(if(!is.null(input$Colour.RCP2)) { plot_colors$rcp <- sapply(1:5, function(i) eval(parse(text = paste0("input$Colour.RCP",i)))) })
#plot_colors$rcp[1] <- input$Colour.RCP1
#plot_colors$rcp[2] <- input$Colour.RCP2
#plot_colors$rcp[3] <- input$Colour.RCP3
#plot_colors$rcp[4] <- input$Colour.RCP4
#plot_colors$rcp[5] <- input$Colour.RCP5
#plot_colors$rcp <- c(input$Colour.RCP1, input$Colour.RCP2, input$Colour.RCP3, input$Colour.RCP4, input$Colour.RCP5)
plot_colors_rcp <- reactive(if(is.null(input$Colour.RCP2)) RCPz$color else c(input$Colour.RCP1, input$Colour.RCP2, input$Colour.RCP3, input$Colour.RCP4, input$Colour.RCP5))#sapply(1:5, function(i) eval(parse(text = paste0("input$Colour.RCP",i))))

myPlotA <- reactive({ makePlot(data = rdt(), x = input$x_rcpO2, y = input$y_rcpO2, color = "RCP", colorIsFactor = TRUE, colorValues = plot_colors_rcp(), colorLabels= c("present", RCPz$RCP[2:5]), facet = "ocean", myxlim = brush.ranges$plotA_x, myylim = brush.ranges$plotA_y) })
output$plotA <- renderPlot({ if(!is.null(rdt())) { myPlotA()[[2]] } })

output$Download_plotA <- downloadHandler(
  filename = paste0(paste0(input$y_rcpO2,"-",input$x_rcpO2),"_",format(Sys.Date(), "%Y%m%d"),".", input$plotA_filetype),
  content = function(file) {  ggsave(file, plot = myPlotA()[[2]], device = input$plotA_filetype) } )

#Calculate changes in surface conditions over increasing pCO2
rsurf <- eventReactive(hv_allRowsNotNA$check, {
  surf <- hv[["DF"]]
  surf <- surf[, list(ocean = ocean, T = T, S= S, DO = DO, TA = TA, pCO2atm = seq(400, 910, by = 10)), by=1:nrow(surf)] #gradient of pCO2
  surf[, pCO2uatm := x2pCO2(S= S, T= T, Patm=1.0, xCO2= pCO2atm ) ] #pCO2 in uatm
  surf <- cbind(surf, 
                surf[, carb(flag = 24, var1 = pCO2uatm, var2= TA*1e-6, S = S, T= T, P= 0/10)],
                RF = surf[, buffesm(flag = 24, var1 = pCO2uatm, var2= TA*1e-6, S = S, T= T, P= 0/10)$R],
                surf[, buffesm(flag = 24, var1 = pCO2uatm, var2= TA*1e-6, S = S, T= T, P= 0/10)[, c("betaDIC", "omegaDIC")]]
  ) 
  #convert mol kg-1 to umol kg-1
  colsToConvert <- c("CO2", "HCO3", "CO3", "DIC", "ALK", "betaDIC", "omegaDIC")
  system.time(for(j in which(names(surf) %in% colsToConvert)) { set(surf, i=NULL, j, value= surf[[j]] * 1e6) }) #faster than lapply(.SD) with .SDcols
  #Delta value differences at pCO2 - 400ppm
  for(j in tail(names(surf), -12)) { surf[, paste0("d",j) := eval(parse(text = j)) - eval(parse(text = j))[pCO2atm == 400], by= ocean ]}
  surf[, nrow := NULL][, flag := NULL]
  return(surf)
})
output$download_rsurf <- downloadHandler( filename = function() {  paste0("rcp_surface_oceans_", format(Sys.Date(), "%Y%m%d"), ".csv") },
                                                   content = function(file) { write.table(rdt(), file, sep=",", row.names=FALSE)  })  
#PlotB = chemistry ~ pCO2
#Reactive UI for plotting
output$plotB_Options <- renderUI({
  #if(!is.null(input$inputFile)) {
  {
    namez <- isolate({ names(rsurf())[!(names(rsurf()) %in% c("ocean", "T", "S", "Patm", "P", "fCO2pot", "pCO2pot", "pCO2insitu", "dfCO2pot", "dpCO2pot", "dpCO2insitu" ))] })
    flowLayout(
      selectInput(inputId = "x_plotB", label = "X", choices = namez, selected = "pCO2atm" ),
      selectInput(inputId = "y_plotB", label = "Y", choices = namez, selected = "pH" ),
      br(),
      downloadButton("download_rsurf", "Download CSV")
    )} })

myPlotB <- reactive({ 
  oceanColours <- unlist(sapply(1:length(unique(rsurf()$ocean)), function(i) eval(parse(text = paste0("input$Colour.ocean",i)))))
  Xtra <- list(labs(colour = NULL))
  makePlot(data = rsurf(), x = input$x_plotB, y = input$y_plotB, color = "ocean", colorIsFactor = TRUE, colorValues = oceanColours, extra_ggplot = Xtra) 
  })

#makePlot(data = surf, x = "pCO2uatm", y = "pH", color = "site2", colorIsFactor = TRUE, colorValues = NULL, colorLabels= NULL, facet = NULL, myxlim = NULL, myylim = NULL, extra_ggplot = Xtra)[[1]]
output$plotB <- renderPlot({ if(!is.null(rsurf())) { myPlotB()[[2]] } })

output$Download_plotB <- downloadHandler(
  filename = paste0(paste0(input$y_plotB,"-",input$x_plotB),"_",format(Sys.Date(), "%Y%m%d"),".", input$plotA_filetype),
  content = function(file) {  ggsave(file, plot = myPlotB()[[2]], device = input$plotA_filetype) } )



  
#CAI PROFILE plotter Tab  
  #This section allows for a user-defined input file.
  #dt <- reactive({if(is.null(input$inputFile)) return(NULL)
  #  qq <- read.csv(input$inputFile$datapath, header = TRUE, na.strings = c("NA","#DIV/0!"))
  #  qq$date <- as.POSIXct(strptime(qq$date, format = "%m/%d/%Y", tz="UTC"))
  #  qq$month <-  as.integer(format(as.Date(qq$date), "%m")) 
  #  qq$year <-   as.integer(format(as.Date(qq$date), "%Y")) 
  #  return(qq)
  #})
  
  dt <- reactive({
      qq <- read.csv("helper/Cai_GOM_processed_20161104.csv", header = TRUE, na.strings = c("NA","#DIV/0!"))
      qq$date <- as.POSIXct(strptime(qq$date, format = "%m/%d/%Y", tz="UTC"))
      qq$month <-  as.integer(format(as.Date(qq$date), "%m")) 
      qq$year <-   as.integer(format(as.Date(qq$date), "%Y"))
      qq$shallow <- qq$depth..m <= 5
      return(qq)
    })
  
  #Reactive data filtering
  data1 <- reactive({dt()[#dt()$date >= as.POSIXct(format(input$dateRange[1], tz= "UTC"), tz = "UTC") &
                          #dt()$date <= as.POSIXct(format(input$dateRange[2], tz= "UTC"), tz = "UTC") &
                          dt()$btl.S >= input$salinityRange[1] & dt()$btl.S <= input$salinityRange[2] &
                           dt()$month %in% input$months &
                           dt()$depth..m >= input$depthRange[1] & dt()$depth..m <= input$depthRange[2], ]
  })
  output$pr_plotOptions <- renderUI({
    #if(!is.null(input$inputFile)) {
    {
      namez <- names(data1())
      flowLayout(
        selectInput(inputId = "xvar", label = "X", choices = namez, selected = "depth..m" ),
        selectInput(inputId = "yvar", label = "Y", choices = namez, selected = "pH..calculated.from.TA...DIC." ),
        selectInput(inputId = "pr_colour", label = "color", choices = namez, selected = "ctemp.degC" ),
        helpText(HTML("Check for depth profiles (set x = depth).&nbsp;")),
        checkboxInput(inputId = "checkboxProfile", label = "Profile", value = TRUE),
        checkboxInput(inputId = "checkboxStation", label = "Station lines", value = TRUE),
        selectizeInput("cruises", "Cruises", choices = c("all", as.character(unique(dt()$cruise))), selected = "all", multiple = TRUE),
        br()
      )} })
  data2 <- reactive({
    names <- NULL
    if ("all" %in% input$cruises) {
      names <- as.character(unique(data1()$cruise))
    } else {
      names <- input$cruises
    }
    data1()[data1()$cruise %in% names, ]
  })  #data2 = cai data filtered by cruise
  
  #Download filtered Cai data
  output$downloadCaiData <- downloadHandler(
    filename = function() { 
      paste0("CaiData_filtered_", format(Sys.Date(), "%Y%m%d"), ".csv") 
    },
    content = function(file) {
      write.table(data2(), file, sep=",", row.names=FALSE)
    }
  )  
    
  output$str.dt <- renderPrint(str(dt() ))  #dataframe info for the meta tab
  myPlotC1 <- reactive({
    #data1() = reactive data.frame called in ggplot
    plot1 <- ggplot(data2(), aes_string(x=input$xvar, y=input$yvar, colour=input$pr_colour ), environment = environment()) + geom_point(size = 1.5  ) + theme_bw(base_size=16)
    #if(input$checkboxStation == TRUE) {
    #plot1 <- plot1 + aes(group = "sta") + geom_line(alpha = 0.2) 
    #}
      return(plot1)
  } )
  
  myPlotC2 <- reactive({
    if(input$checkboxStation == TRUE) {
      plot1 <- myPlotC1() + aes(group = "sta") + geom_line(alpha = 0.2) 
    } else { plot1 <- myPlotC1() }
    return(plot1)
  })
  
  output$plotC<- renderPlot({    
    if(input$checkboxProfile == TRUE) {
    plot2 <- myPlotC2() + coord_flip() + scale_x_reverse()
  } else { plot2 <- myPlotC2() }
  print(plot2)
  }, height = 700)

#Actual data vs model data
  template <- ggplot(modOA[T %in% seq(5,30, by=5)], aes(O2f, pHf, z = T, group = T, colour = T, fill = T)) + geom_line(size = 1.5) + theme_classic(base_size = 16) +
    scale_colour_gradientn(colours= viridis(256) ) + scale_fill_gradientn(colours= viridis(256) ) +
    labs(colour = bquote("T,"~degree*C), x = bquote("dissolved"~O[2]*","~mu*mol~kg^-1), y = bquote(italic("in situ")~"pH"), shape = NULL) + theme(legend.position = "right")+#c(0.88, 0.25)) +
    scale_shape_manual(values = c(24, 21)) + guides(fill = FALSE, shape = guide_legend(override.aes = list(shape = c(17,16)))  ) + 
    theme(axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(color="black", size = 1)) #Use this if axis lines don't appear (bug in ggplot2)

myPlotD <- reactive({
  #pH graphs
  gp <- template +
    geom_point(data = wc2, aes(DO, pH, colour = T, fill = T, shape = "West Coast"), size = 2) +
    geom_point(data = data2(), aes(DO.umol.kg, pH..calculated.from.TA...DIC., colour = ctemp.degC, fill = ctemp.degC, shape = "Gulf of Mexico"), size = 2)
  #geom_point(data = cai[site2 == "GOM"], aes(DO, pH, colour = T, fill = T, shape = "Gulf of Mexico"), size = 2) 
  #No shallow cai points so this line would cause an error: geom_point(data = cai[site2 == "GOM" & shallow == TRUE], aes(DO, pH, colour = T, fill = T, shape = "Gulf of Mexico"), fill = "white", size = 2) 
  gpShallow <- template + theme(legend.background = element_rect(fill = alpha("white", 0)) ) + #legend.position = c(0.85, 0.25), 
    geom_point(data = wc2[shallow == FALSE], aes(DO, pH, colour = T, fill = T, shape = "West Coast"), size = 2) +
    geom_point(data = wc2[shallow == TRUE], aes(DO, pH, colour = T, fill = T, shape = "West Coast"), fill = "white", size = 2) +
    geom_point(data = data2()[data2()$shallow == FALSE,], aes(DO.umol.kg, pH..calculated.from.TA...DIC., colour = ctemp.degC, fill = ctemp.degC, shape = "Gulf of Mexico"), size = 2) +
    if(any(data2()$shallow == TRUE)) {
    geom_point(data = data2()[data2()$shallow == TRUE,], aes(DO.umol.kg, pH..calculated.from.TA...DIC., colour = ctemp.degC, shape = "Gulf of Mexico"), fill = "white", size = 2) 
} else { NULL }
#  #Add gulf of alaska
#  gpShallow + scale_shape_manual(values = c(22, 24, 21)) + guides(fill = FALSE, shape = guide_legend(override.aes = list(shape = c(15, 17,16)))  ) +   theme(legend.position = c(0.82, 0.3)) +
#    geom_point(data = ga1[depth > 5], aes(DO0.ctd, pH, colour = T1, fill = T1, shape = "Gulf of Alaska"), size = 2) +
#    geom_point(data = ga1[depth <= 5], aes(DO0.ctd, pH, colour = T1, fill = T1, shape = "Gulf of Alaska"), fill = "white", size = 2) 
  return(gpShallow)
})
output$plotD <- renderPlot({  myPlotD() })
myPlotE <- reactive({ 
  #Arag graphs
  #for reference: gp %+% temp[T %in% seq(5,30, by=5)]  #update ggplot
  gp1 <- template + aes(y = Aragf) + labs(y = bquote(Omega[aragonite])) +#theme(legend.position = c(0.88, 0.25)) +
    geom_point(data = wc2, aes(DO, Arag, colour = T, shape = "West Coast"), size = 2) +
    geom_point(data = data2(), aes(DO.umol.kg, Arag..calculated.from.TA...DIC., colour = ctemp.degC, fill=ctemp.degC, shape = "Gulf of Mexico"), size = 2)
  #geom_point(data = cai[site2 == "GOM"], aes(DO, Arag, colour = T, shape = "Gulf of Mexico"), size = 2)
  gp1Shallow <- template + aes(y = Aragf) + theme(legend.background = element_rect(fill = alpha("white", 0)), legend.box.just = "right") + labs(y = bquote(Omega[aragonite])) + #legend.position = c(0.85, 0.25),
    #guides(colour = guide_colourbar(direction = "horizontal", title.position = "top")) +
    geom_point(data = wc2[shallow == FALSE], aes(DO, Arag, colour = T, fill = T, shape = "West Coast"), size = 2) +
    geom_point(data = wc2[shallow == TRUE], aes(DO, Arag, colour = T, fill = T, shape = "West Coast"), fill = "white", size = 2) +
    geom_point(data = data2()[data2()$shallow == FALSE,], aes(DO.umol.kg, Arag..calculated.from.TA...DIC., colour = ctemp.degC, fill = ctemp.degC, shape = "Gulf of Mexico"), size = 2) +
    if(any(data2()$shallow == TRUE)) {
      geom_point(data = data2()[data2()$shallow == TRUE,], aes(DO.umol.kg, Arag..calculated.from.TA...DIC., colour = ctemp.degC, shape = "Gulf of Mexico"), fill = "white", size = 2) 
    } else { NULL }
#  #include Gulf of Alaska.
#  gpa <- gp1Shallow + scale_shape_manual(values = c(22, 24, 21)) + guides(fill = FALSE, shape = guide_legend(override.aes = list(shape = c(15, 17,16))), colour = guide_colourbar(direction = "horizontal", title.position = "top", title.hjust=1)  ) +   theme(legend.position = c(0.82, 0.18)) +
#    geom_point(data = ga1[depth > 5], aes(DO0.ctd, Arag, colour = T1, fill = T1, shape = "Gulf of Alaska"), size = 2) +
#    geom_point(data = ga1[depth <= 5], aes(DO0.ctd, Arag, colour = T1, fill = T1, shape = "Gulf of Alaska"), fill = "white", size = 2) 
  return(gp1Shallow)
})  
output$plotE <- renderPlot({ myPlotE() })
output$Download_plotD <- downloadHandler(
  filename = paste0("pH-DO_modelVactual_",format(Sys.Date(), "%Y%m%d"),".", input$plotD_filetype),
  content = function(file) {  ggsave(file, plot = myPlotD()[[2]], device = input$plotD_filetype) } )
output$Download_plotE <- downloadHandler(
  filename = paste0("pH-Arag_modelVactual_",format(Sys.Date(), "%Y%m%d"),".", input$plotE_filetype),
  content = function(file) {  ggsave(file, plot = myPlotE()[[2]], device = input$plotE_filetype) } )

observeEvent(input$plotD_dblclick, {
  brush <- input$plotD_brush
  if (!is.null(brush)) {
    brush.ranges$plotD_x <- c(brush$xmin, brush$xmax)
    brush.ranges$plotD_y <- c(brush$ymin, brush$ymax)
    
  } else {
    brush.ranges$plotD_x <- NULL
    brush.ranges$plotD_y <- NULL
  }
})
observeEvent(input$plotE_dblclick, {
  brush <- input$plotE_brush
  if (!is.null(brush)) {
    brush.ranges$plotE_x <- c(brush$xmin, brush$xmax)
    brush.ranges$plotE_y <- c(brush$ymin, brush$ymax)
    
  } else {
    brush.ranges$plotE_x <- NULL
    brush.ranges$plotE_y <- NULL
  }
})
#Create table that interacts with plot
output$prDT <- DT::renderDataTable( data2(), filter = "top", options = list(scrollX = TRUE) )
prProxy <- dataTableProxy("prDT")  #Create a proxy table for interacting via external widgets


filteredDF <- reactiveValues( somedata = NULL, allRows = NULL, newRow = NULL)
observeEvent(data2(), { filteredDF$somedata <- data2() })

prNP_plotD <- reactive(nearPoints(df = filteredDF$somedata,
                            xvar = "DO.umol.kg", yvar = "pH..calculated.from.TA...DIC.", coordinfo = input$plotD_click, threshold = 10, maxpoints = 1, addDist = FALSE))
prNP_plotE <- reactive(nearPoints(df = filteredDF$somedata,
 xvar = "DO.umol.kg", yvar = "Arag..calculated.from.TA...DIC.", input$plotE_click, threshold = 10, maxpoints = 1, addDist = FALSE))
observeEvent(
  prNP_plotD(),
  {
    filteredDF$newRow <- which(duplicated(rbind(filteredDF$somedata, prNP_plotD()), fromLast = TRUE)) #Choose the row number by finding the duplicate
    if(!is.null(filteredDF$newRow) & length(filteredDF$newRow) > 0) {
      if(filteredDF$newRow %in% filteredDF$allRows) {
        filteredDF$allRows <- filteredDF$allRows[!(filteredDF$allRows %in% filteredDF$newRow)]
      } else {
        filteredDF$allRows <- c(filteredDF$allRows, filteredDF$newRow)
      }
      selectRows(prProxy, as.numeric(filteredDF$allRows)) 
    }
  })
observeEvent(
  prNP_plotE(),
  {
    filteredDF$newRow <- which(duplicated(rbind(filteredDF$somedata, prNP_plotE()), fromLast = TRUE)) #Choose the row number by finding the duplicate
    if(!is.null(filteredDF$newRow) & length(filteredDF$newRow) > 0) {
      if(filteredDF$newRow %in% filteredDF$allRows) {
        filteredDF$allRows <- filteredDF$allRows[!(filteredDF$allRows %in% filteredDF$newRow)]
      } else {
        filteredDF$allRows <- c(filteredDF$allRows, filteredDF$newRow)
      }
      selectRows(prProxy, as.numeric(filteredDF$allRows)) 
    }
  }) 



observeEvent(
  input$pr_resetSingleClick,
  {
    filteredDF$allRows <- NULL
    filteredDF$newRow <- NULL
    selectRows(prProxy, NULL)
  })

  
# TROUBLESHOOTING: Output text  
  output$feed1 <- renderPrint({ 
    list(unique(data2()$sta)  )
         })
  output$feedback <- renderPrint({ 
    list(str(data2()),
         nrow(data1()), 
         nrow(data2())
         )  })
  #output$feed2 <- renderPrint(sapply(1:length(unique(rsurf()$ocean)), function(i) eval(parse(text = paste0("input$Colour.ocean",i)))))
  
  
  }) #Close shinyServer(function() {