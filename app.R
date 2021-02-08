#loading some baseline stuff
library(shiny)
library(ggplot2)
library(here)
library(ggpubr)
library(jpeg)
library(ggimage)

theme_meg <- function () {
  theme_bw(base_family="Times") %+replace%
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.title=element_blank(), legend.text=element_text(size=9),
          axis.text.x=element_text(size=10, face="bold"),axis.ticks.length.y=unit(-10, "pt"),
          axis.text.y.left=element_text(margin= margin(r=12), size=10, face="bold", color="black"),
          axis.text.y.right = element_blank(),
          plot.margin=grid::unit(c(0,8,0,5),"mm"))
}

background = "background2.png"


#ok so first let's do the ui and figure out what kind of layout to have -sidebar panel

ui <- fluidPage(
  titlePanel("The Role of DIC and pH in Copper Pipes"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("pH","pH", min=6,
                  max =9, value=7.5,step=.25),
      sliderInput("DIC","DIC (mg C/L)", min=0,
                  max =150, value=75,step=10)

    ),#this closes sidebar panel
    mainPanel(
      plotOutput("graph"),
      imageOutput("pitting"),
      imageOutput("highcopper")
    )#this closes main panel
  )#this closes sidebar layout
)#this closes fluid page

server <- function(input, output, session){
  pH <- reactive({
    input$pH
  })
  DIC <- reactive({
    input$DIC
  })
  output$graph <- renderPlot({
    p <- ggplot()+
      geom_point(aes(x=input$pH, y=input$DIC), shape=23, size=3, fill="blue")+
      scale_y_continuous(limits=c(0,150),breaks = seq(0, 150, 25))+
      scale_x_continuous(limits=c(6,9), breaks=seq(6,9,.5))+
      geom_text(aes(x=6.5, y=140, label="Potentially High\nCopper Levels", family="Times"))+
      geom_text(aes(x=8.5, y=10, label="Copper Pitting Likely", family="Times"))+
      labs(x="pH", y="DIC (mg C/L)")+
      theme_meg()
    ggbackground(p, background)
  })
  output$pitting <- renderImage({

  if ( pH() >= 7.5 & DIC() <= 50) {
    return(list(
      src = "pitting.png",
      contentType = "image/png",
      width= 400,
      height = 300,
      alt = ""
    ))
  } else if ( pH() < 7.5 & DIC() > 50) {
    return(list(
      src = "copperwater.png",
      contentType = "image/png",
      width= 400,
      height = 300,
      alt = ""
    ))
  } else {
    return(list(
      src = "unsure.png",
      contentType = "image/png",
      width= 400,
      height = 300,
      alt = "Uncertain"
    ))
  }
  }, deleteFile = FALSE)
}#closes server

shinyApp(ui, server)




