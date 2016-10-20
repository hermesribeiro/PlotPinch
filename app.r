library(shiny)
library(png)
library(jpeg)
library(digest)
library(rdrop2)

# TODO: Magnifying glass
# Dropbox authentication
token <- readRDS(paste0(getwd(),"/droptoken.rds"))
drop_acc(dtoken = token)
# Temporary directory
temp.dir=tempdir()

ui <- shinyUI(fluidPage(
  titlePanel("Upload plot - [ALPHA] PlotPinch v0.1"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = 'fileplot', 
                label = 'Select an Image',
                multiple = F,
                accept=c('image/png', 'image/jpeg')),
      fluidRow(column(2,actionButton("origin","Origin"))),
      fluidRow(column(2),column(2,h5("From")),column(2,h5("To")),column(3,h5("Scale"))),
      fluidRow(column(2,actionButton("abscissa","X axis")),
               column(2,textInput(inputId = "abscissa.min",label = NA, value = "0")),
               column(2,textInput(inputId = "abscissa.max",label = NA, value = "1")),
               column(3,selectInput(inputId = "abscissa.scale",label=NA,c("Linear","Log")))),
      fluidRow(column(2),column(2,h5("From")),column(2,h5("To")),column(3,h5("Scale"))),
      fluidRow(column(2,actionButton("ordinate","Y axis")),
               column(2,textInput(inputId = "ordinate.min",label = NA, value = "0")),
               column(2,textInput(inputId = "ordinate.max",label = NA, value = "1")),
               column(3,selectInput(inputId = "ordinate.scale",label=NA,c("Linear","Log")))),
      fluidRow(column(2,h5(" "))),
      fluidRow(column(2,h5(" "))),
      fluidRow(column(2,actionButton("add.curve","Add")),
               column(3,selectInput(inputId = "select.curve",label=NA,c("Curve 1" = 1, "Curve 2" = 2, "Curve 3" = 3, "Curve 4" = 4)))),
      fluidRow(column(2,h5(" "))),
      fluidRow(column(2,h5(" "))),
      fluidRow(column(3,actionButton("delete","Delete points")),column(3,actionButton("confirm","Confirm deletion"))),
      fluidRow(column(2,h5(" "))),
      fluidRow(column(2,h5(" "))),
      fluidRow(column(2,downloadButton('downloadData', 'Download')))
    ),
    mainPanel(
      uiOutput("image.ui")  
    )
  )
))

server <- shinyServer(function(input, output) {
  # File upload
  files <- reactive({
    files <- input$fileplot
    files$datapath <- gsub("\\\\", "/", files$datapath)
    files
  })
  # Reads PNG
  ima <- reactive({
    if (is.null(input$fileplot)) {
      ima <- readPNG("WelcomeTutorial.png")
    }
    else{
      if (files()$type == "image/png") {
        ima <- readPNG(files()$datapath, native = T)
      } else if (files()$type == "image/jpeg") {
        ima <- readJPEG(files()$datapath, native = T)
      }
    }
  })
  # Extracts width and height
  ima.x <- reactive(dim(ima())[2])
  ima.y <- reactive(dim(ima())[1])
  # Substitute for plotPutput on ShinyUI() in order to dinamically allocate width and heigth
  output$image.ui <- renderUI({
    plotOutput("image1", width = ima.x(), height = ima.y(), click = clickOpts(id = "plot_click"),brush = "plot_brush")
  })
  # Render a zero margin plot with image as background
  output$image1 <- renderPlot({
    par(mar=c(0,0,0,0))
    par(xaxs='i',yaxs='i')
    plot(0:1,0:1,cex=0)
    lim <- par()
    rasterImage(ima(), lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    if (value$origin$x != 0 || value$origin$y != 0) {
      points(value$origin$x,value$origin$y,pch=3,cex=5, col="black",lwd=3)
    }
    if (value$abscissa$x != 1 || value$abscissa$y != 0) {
      points(value$abscissa$x,value$abscissa$y,pch=3,cex=5, col="blue",lwd=3)
    }
    if (value$ordinate$x != 0 || value$ordinate$y != 1) {
      points(value$ordinate$x,value$ordinate$y,pch=3,cex=5, col="green",lwd=3)
    }
    if (nrow(value$curve) != 1) {
      points(value$curve$x,value$curve$y,pch=10,cex=3, col=(value$curve$n+1),lwd=2)
    }
    input$confirm
  })
  # Axis handling
  # Set Default axis and save them to Value
  origin = data.frame(x=0 ,y=0)
  abscissa = data.frame(x=1, y=0)
  ordinate = data.frame(x=0, y=1)
  curve = data.frame(x=NA, y=NA, n=NA)
  delete = data.frame(x=c(-2,-2),y=c(-2,-2))
  value <- reactiveValues(origin.set=F, origin=origin, abscissa.set=F,
                          abscissa=abscissa, ordinate.set=F, ordinate=ordinate,
                          curve.set=F,curve=curve, delete.set=F,delete=delete)
  #Origin, X axis, Yaxis and add curve buttons as triggers
  observeEvent(input$origin,{ # Origin
    value$origin.set=T
    value$abscissa.set=F
    value$ordinate.set=F
    value$curve.set = F
    value$delete.set = F
  })
  observeEvent(input$abscissa,{ # X axis
    value$origin.set=F
    value$abscissa.set=T
    value$ordinate.set=F
    value$curve.set = F
    value$delete.set = F
  })
  observeEvent(input$ordinate,{ # Y axis
    value$origin.set=F
    value$abscissa.set=F
    value$ordinate.set=T
    value$curve.set = F
    value$delete.set = F
  })
  observeEvent(input$add.curve,{ # Add curve
    value$origin.set=F
    value$abscissa.set=F
    value$ordinate.set=F
    value$curve.set = T
    value$delete.set = F
  })
  observeEvent(input$delete,{ # Add curve
    value$origin.set=F
    value$abscissa.set=F
    value$ordinate.set=F
    value$curve.set = F
    value$delete.set = T
  })
  # Action button actions
  observeEvent(input$plot_click,{
    if (value$origin.set==T) { # Set Origin
      origin$x=input$plot_click[[1]]
      origin$y=input$plot_click[[2]]
      value$origin=origin
      value$origin.set=F
    } else if (value$abscissa.set==T) { # Set X axis
      abscissa$x=input$plot_click[[1]]
      abscissa$y=input$plot_click[[2]]
      value$abscissa=abscissa
      value$abscissa.set=F
    } else if (value$ordinate.set==T) { # Set Y axis
      ordinate$x=input$plot_click[[1]]
      ordinate$y=input$plot_click[[2]]
      value$ordinate=ordinate
      value$ordinate.set=F
    } else if (value$curve.set==T) { # Add points to a curve
      value$curve=rbind(value$curve,c(input$plot_click[[1]],input$plot_click[[2]],as.numeric(input$select.curve)))
    }
    else {
      return(NULL)
    }
  })
  observeEvent(input$confirm,{
    if (value$delete.set==T && !is.null(input$plot_brush)) {
      a=rep(F,nrow(value$curve))
      for (i in 2:nrow(value$curve)){
        if (value$curve$x[i]>=input$plot_brush$xmin && value$curve$y[i]>=input$plot_brush$ymin &&
            value$curve$x[i]<=input$plot_brush$xmax && value$curve$y[i]<=input$plot_brush$ymax) a[i]=T
      }
      value$curve=value$curve[which(a==F),]
    }
    value$delete.set = F
  })
  # Post Processing and download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('PlotPinch.csv', sep='')
    },
    content = function(con) {
      curve = value$curve[2:nrow(value$curve),]
      rownames(curve) = c(1:nrow(curve))
      colnames(curve) = c("X","Y","CURVE")
      xm = value$origin$x
      xM = value$abscissa$x
      ym = value$origin$y
      yM = value$ordinate$y
      Xmin=as.numeric(input$abscissa.min)
      Xmax=as.numeric(input$abscissa.max)
      Ymin=as.numeric(input$ordinate.min)
      Ymax=as.numeric(input$ordinate.max)
      x.offset = Xmin
      x.scale = Xmax-Xmin
      y.offset = Ymin
      y.scale = Ymax-Ymin
      if (input$abscissa.scale == "Log") {
        curve$X = (curve$X - xm)/(xM-xm)
        curve$X = Xmin*10.0^(curve$X*(log10(Xmax)-log10(Xmin)))
      }
      else {
        curve$X=(curve$X - xm)/(xM-xm)*x.scale+x.offset
      }
      if (input$ordinate.scale == "Log") {
        curve$Y=(curve$Y - ym)/(yM - ym)
        curve$Y = Ymin*10.0^(curve$Y*(log10(Ymax)-log10(Ymin)))
      } 
      else {
        curve$Y=(curve$Y - ym)/(yM-ym)*y.scale+y.offset
      }
      curve=curve[order(curve$CURVE,curve$X,curve$Y),]
      curve$X=format(curve$X,digits = 4, scientific = T)
      curve$Y=format(curve$Y,digits = 4, scientific = T)
      essentials=data.frame(xmin=xm,xmax=xM,Xmin=Xmin,Xmax=Xmax,Xscale=input$abscissa.scale,
                            ymin=ym,ymax=yM,Ymin=Ymin,Ymax=Ymax,Yscale=input$ordinate.scale)
      values=value$curve[2:nrow(value$curve),]
      # Save Files
      file.name=sprintf("/%s_%s", as.integer(Sys.time()), digest::digest(curve))
      png(file=paste0(temp.dir,file.name,".png"),ima.x(),ima.y(),"px")
      par(mar=c(0,0,0,0))
      par(xaxs='i',yaxs='i')
      lim <- par()
      plot(0:1,0:1,cex=0)
      rasterImage(ima(), lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
      dev.off()
      # Dropbox Upload
      outputDir="PlotPinch"
      drop_upload(paste0(temp.dir,file.name,".png"), dest = outputDir)
      write.table(essentials,file=paste0(temp.dir,file.name,"_META.csv"),row.names = F)
      drop_upload(paste0(temp.dir,file.name,"_META.csv"), dest = outputDir)
      write.table(values,file=paste0(temp.dir,file.name,"_POINTS.csv"),row.names = F)
      drop_upload(paste0(temp.dir,file.name,"_POINTS.csv"), dest = outputDir)
      # Data Download
      write.table(curve, con, row.names = F,sep = ";")
    })
  
  # DEBUG: (todo: delete it for code lightness)
  observe({
    if (is.null(input$plot_click)) return(NULL)
    #print(as.numeric(c(input$plot_click[[1]],input$plot_click[[2]])))
  })
})

shinyApp(ui=ui,server=server)
