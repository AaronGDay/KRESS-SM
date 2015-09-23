
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Libraries
source('serverlibs.R')

# Global Options

# 2GiB max file size
options(shiny.maxRequestSize=2048*1024^2)
rasterOptions(standardnames = FALSE)


# Weight, RasterFile, RasterFilename, Layer number, CSVFile
variableList <<- list(
  list(), 
  stack(),
  vector(),
  stack(),
  data.table()
)


chosenLayer <<- numeric()

shinyServer(function(input, output) {
  
  # Variables
  output$VariableDisplay <- renderUI({
    addVariableToList()
    if(nlayers(stack(variableList[[2]])) == 0){
      HTML(paste("You currently have no variables added."))
    }
    else{
      selectInput(
        inputId = "selectedVariable",
        label = "Select variable to view and modify.",
        choices = variableList[[3]],
        multiple = FALSE
      )
    }
  })
  
  
  output$ShowSelected <- renderPlot({
    if(!is.null(input$selectedVariable)){
      toUse <- raster(variableList[[2]], layer = plotNumbers())
      plot(
        stretch(toUse),
        main = variableList[[3]][[plotNumbers()]]
      )
    }
  })
  
  
  output$Submit1 <- renderUI({
    if(!is.null(input$selectedVariable)){
      submitButton()
    }
  })
  
  output$weightButton <- renderUI({
    if(!is.null(input$AddVariables)){
      actionButton("AdjustWeightButton", "Adjust")
    }
  })    
  
  output$weightEdit <- renderUI({
    if(!is.null(input$selectedVariable)){
      numericInput(
        "weight1",
        label = "Weight",
        value = variableList[[1]][[plotNumbers()]],
        min = 0,
        max = 3,
        step = .1
      )
    }
  })
  
  output$listNames <- renderUI({
    if(!is.null(variableList)){
      HTML(paste(createList()))
    }
  })
  
  
  createList <- eventReactive(input$AdjustWeightButton, {
    foo <- paste("<b>", variableList[[1]], "</b>  ", variableList[[3]], "<br/>")
    return(foo)
  })
  
  createList2 <- eventReactive(input$AddVariables, {
    createList()
  })
  
  
  adjustWeight <- observeEvent(input$AdjustWeightButton, {
      variableList[[1]][[plotNumbers()]] <<- input$weight1
  })
  
  
  addVariableToList <- reactive({
    # Takes files uploaded and converts to raster stack
    if(!is.null(input$AddVariables)){
      for(i in 1:nrow(input$AddVariables)){
        # Default weight as 1
        variableList[[1]] <<- append(variableList[[1]], 1)
        # Add variable to stack
        variableList[[2]] <<- addLayer(variableList[[2]], raster(input$AddVariables[i, 4]))
        # Add raster filename
        variableList[[3]] <<- append(variableList[[3]], input$AddVariables[i, 1])
      }
      variableList[[4]] <<- addLayer(variableList[[4]], raster(input$AddVariables[1, 4]))
    }
  })
  
  addVariableToList2 <- reactive({
    # Takes files uploaded and converts to raster stack
    if(!is.null(input$AddVariables2)){
      for(i in 1:nrow(input$AddVariables2)){
        # Default weight as 1
        variableList[[1]] <<- append(variableList[[1]], 1)
        # Add variable to stack
        variableList[[2]] <<- addLayer(variableList[[2]], raster(input$AddVariables2[i, 4]))
        # Add raster filename
        variableList[[3]] <<- append(variableList[[3]], input$AddVariables2[i, 1])
      }
      variableList[[4]] <<- addLayer(variableList[[4]], raster(input$AddVariables2[1, 4]))
    }
  })
  
  
  rasterReactive <- reactive({
    return(raster(variableList[[2]], layer = plot))
  })
  
  getRows <- eventReactive(input$AddVariables, {
    return(nlayers(variableList[[2]]))
  })
  
  plotNumbers <- reactive({
    if(!is.null(variableList)){
      for(j in 1:getRows()){
        if(identical(input$selectedVariable, variableList[[3]][[j]]) == TRUE) {
          return(j)
        }
      }
    }
  })
  
  
  plotNumbers2 <- reactive({
    if(!is.null(variableList)){
      for(j in 1:getRows()){
        if(identical(input$showStats, variableList[[3]][[j]]) == TRUE) {
          return(j)
        }
      }
    }
  })
  
  
  choosePlots <- reactive({
    if(!is.null(input$AddVariables) && !is.null(input$selectedVariable)){
      fileList[[4]] <<- fileList[[2]]
      tempStack <- stack()
      for(i in 1:length(foo <- plotNumbers()))
        tempStack <- addLayer(tempStack, raster(variableList[[2]], layer = foo[i]))
      variableList[[4]] <<- tempStack
    }
  })
  
  output$LeftSide <- renderUI({
    createList()
    selectInput(
      inputId = "leftAlgebra",
      label = "Select Files",
      choices = paste("[",variableList[[1]], "]", variableList[[3]], ""),
      multiple = TRUE
    )
  })
  
  output$RightSide <- renderUI({
    createList()
    selectInput(
      inputId = "rightAlgebra",
      label = "Select Files",
      choices = paste("[",variableList[[1]], "]", variableList[[3]], ""),
      multiple = TRUE
    )
  })
  
#   output$NewFileNameUI <- renderUI({
#     textInput(
#       "NewFileName",
#       label = "Save file as",
#       value = paste('newraster-', Sys.Date(), sep='')
#     )
#   })
  
  output$DownloadButton <- renderUI({
    isolate({
      downloadButton("theDownload")
    })
  })
  
  output$theDownload <- downloadHandler(
    filename = input$NewFileName,
    content = function () {
      writeRaster(algebraResult, format = input$downloadFormat)
    }
  )
  
  output$VariableDisplay2 <- renderUI({
    
    if(nlayers(stack(variableList[[2]])) == 0){
      HTML(paste("You currently have no variables added. Add below or in the Variables tab."))
    }
    else{
      selectInput(
        inputId = "showStats",
        label = "Select variables",
        choices = variableList[[3]],
        multiple = FALSE
      )
    }
  })
  
  
  output$bar.plot <- renderPlot({
    if(!is.null(variableList)){
      toUse <- raster(variableList[[2]], layer = plotNumbers2())
      barplot(
        stretch(toUse),
        border = TRUE,
        xlab = "Value",
        ylab = "Frequency",
        col = "black",
        breaks = 255
      )
      
    }
  })
  
  output$stat.heat.map <- renderPlot({
    if(!is.null(variableList)){
      toUse <- raster(variableList[[2]], layer = plotNumbers2())
      plot(
        stretch(toUse),
        colNA = "black",
        col = colorRampPalette(c("white", "brown", "yellow", "green"), bias = .6) (255)
      )
    }
  })
  
  output$mapParam <- renderUI({
    # Displays map parameters
    if(!is.null(variableList)  && hasValues(map <- raster(variableList[[2]], layer = plotNumbers2()))){
      map.columns <- paste("Columns: ", ncol(map))
      map.rows <- paste("Rows: ", nrow(map))
      map.x.lower.left <- paste("X Lower Left: ", xmin(map))
      map.y.lower.left <- paste("Y Lower Left: ", ymin(map))
      map.cell.size <- paste("Cell Size: ", xres(map))
      
      HTML(paste(map.columns, map.rows, map.x.lower.left, map.y.lower.left, map.cell.size, sep = '<br/>'))
    }
    else
      HTML(paste("Columns:", "Rows:", "X Lower Left:", "Y Lower Left:", "Cell Size:", sep = '<br/>'))
  })
  
  
  output$mapStats <- renderText({
    # Displays map statistics
    if(!is.null(variableList) && hasValues(map <- raster(variableList[[2]], layer = plotNumbers2()))){
      map.min <- paste("Minimum: ", prettyNum(cellStats(map, "min"), drop0trailinng = TRUE, digits = 6))
      map.max <- paste("Maximum: ", prettyNum(cellStats(map, "max"), drop0trailinng = TRUE, digits = 6))
      map.mean <- paste("Mean: ", prettyNum(cellStats(map, "mean"), drop0trailinng = TRUE, digits = 8))
      map.sd <- paste("Standard Deviation: ", prettyNum(cellStats(map, "sd"), drop0trailing = TRUE, digits = 8))
      map.cell.count <- paste("Cell Count: ", ncell(map)-cellStats(map, "countNA"))
      map.cell.sum <- paste("Cell Sum: ", prettyNum(cellStats(map, "sum"),drop0trailing = TRUE, digits = 8))
      
      HTML(paste(map.min, map.max, map.mean, map.sd, map.cell.count, map.cell.sum, sep = '<br/>'))
    }
    else
      HTML(paste("Minimum:", "Maximum:", "Mean:", "Standard Deviation:", "Cell Count:", "Cell Sum:", sep = '<br/>'))
  })
  
  output$WSMVariables <- renderUI({
    if(!is.null(input$AddVariables)){
      selectInput(
        "WSMSelect",
        "Select variables to use",
        choices = variableList[[3]],
        multiple = TRUE
      )
    }
    else{
      HTML(paste("Add variables in the Variables tab"))
    }
  })
  
  
  output$XCoord <- renderUI({
    if(!is.null(input$WSMData)){
      selectInput(
        "xcoordx",
        "Longitude:",
        choices = names(dataFile()),
        multiple = FALSE
      )
    }
  })
  
  output$YCoord <- renderUI({
    if(!is.null(input$WSMData)){
      selectInput(
        "ycoordy",
        "Latitude:",
        choices = names(dataFile()),
        multiple = FALSE
      )
    }
  })
  
  dataFile <- reactive({
    foo <- read.csv(input$WSMData[1, 4], header = TRUE)
    return(foo)
  })
  
  getRowsForSelect <- eventReactive(input$WSMSelect, {
    return(length(input$WSMSelect))
  })
  
  generateModel <- eventReactive(input$WSMgenerate, {
    if(!is.null(input$WSMSelect) & !is.null(dataFile())){
      tempStack <- list(
        stack(),
        vector()
      )
      # wsmraster <- calc(variableList[[2]][[1]], fun1)
      wsmraster <- raster()
      foo <- as.data.frame(dataFile())
      count <- 0
      for(j in 1:getRowsForSelect()){
        for(i in 1:nlayers(variableList[[2]])){
          if(variableList[[3]][[i]] == input$WSMSelect[[j]]){
            tempStack[[1]] <- addLayer(tempStack[[1]], raster(variableList[[2]],layer = i))
            tempStack[[2]] <- append(tempStack[2], variableList[[1]][[i]])
            count <- count + 1
          }
        }
      }
      #wsmraster <- overlay(tempStack[[1]], fun = sum())
      wsmraster <- sum(tempStack[[1]])
      #foo <- foo[ , c(input$xcoordx, input$ycoordy), with = FALSE]
      foo <- data.frame(x= foo[input$xcoordx], y=  foo[input$ycoordy])
      foo <- rasterize(foo, wsmraster)
      
      wsmFinal <<- wsmraster
      return(foo)
    }
  })
  
  finalPlot <- eventReactive(input$WSMgenerate, {
    toUse <- generateModel()
    toUse2 <- wsmFinal
    
    maxi <- maxValue(toUse)
    
    
    toUse2 <- clamp(
      toUse2, 
      lower = (input$lOutlier * .01)*maxi, 
      upper = (input$uOutlier * .01)*maxi, 
      useValues = TRUE
    )
    
    maxi <- maxValue(toUse2)
    mini <- minValue(toUse2)
    return(stretch(toUse2 - mini))
  })
  
  output$testplot123 <- renderPlot({
    if(!is.null(variableList)){
      toUse <- finalPlot()
      plot(
        toUse,
        xlab = input$xcoordx,
        ylab = input$ycoordy,
        legend = TRUE,
        colNA = "black",
        col = colorRampPalette(c("brown", "red", "orange", "yellow", "green"), bias = 1)(255)
      )
    }
  })

  
  output$WSMNewFileName <- renderUI({
    textInput(
      "theWSMNewFileName",
      label = "Save file as",
      value = paste('newraster-', Sys.Date(), sep='')
    )
  })
  
  output$WSMDownloadButton <- renderUI({
      downloadButton("theWSMDownload", label = paste("Download ", updateFileName()))
  })

  
  
#   output$theWSMDownload <- downloadHandler(
#     filename = paste(updateFileName()),
#     content = function (file) {
#       foo <- writeRaster(finalPlot(),  filename = file, overwrite = TRUE, format = "raster")
#       blahhh <- foo
#       blah <<- paste(foo@file@name)
#       file.rename(foo@file@name, file)
#       #file.rename(file, foo@file@name)
#     },
#     contentType = "NA"
#   )
  
  output$theWSMDownload <- downloadHandler(
    filename = paste(updateFileName()),
    content = function(file){
      write.csv(as.data.table(as.array(finalPlot())), file)
    }
  )
  
  
  updateFileName <- reactive({
    #return(paste(input$theWSMNewFileName, ".grd", sep = ""))
    return(paste(input$theWSMNewFileName, ".csv", sep = ""))
  })
  
  
  
})
