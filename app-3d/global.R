library(Rtsne)
library(rgl)
library(FactoMineR)
library(vegan)
library(car)
library(shiny)
train <- read.csv("train.csv", as.is = TRUE)[, -1]
valid <- read.csv("valid.csv", as.is = TRUE)[, -1]
km <- kmeans(train,5,10000)
# run principle component analysis
pc <- prcomp(train)
# plot original data on projection
dim <- c("two dimensional" = "a", "three dimensional" = "b")

ui <- pageWithSidebar(
  headerPanel('Exploratory Data Analysis'),
  sidebarPanel(
    selectInput("xcol", 'Choose dimension', dim)),
  mainPanel(plotOutput('plot1'))
)

plota <- function(){
  plot(pc$x[,1], pc$x[,2],col=km$cluster,pch=16, xlab = "principle component 1", ylab = "principle component 2")
  pc_bind<-cbind(pc$x[,1], pc$x[,2])
  ordispider(pc_bind, factor(km$cluster), label = TRUE)
  ordihull(pc_bind, factor(km$cluster), lty = "dotted")
}

plotb <- function(){
  scatter3d(x = pc$x[,1], y = pc$x[,2], z = pc$x[,3], groups = as.factor(km$cluster),
                                        surface=FALSE, ellipsoid = TRUE)
}

server <- function(input, output) {
  output$myPlot <- renderPlot({
  plottype <- switch(input$xcol,
                     a = plota,
                     b = plotb)
  plottype()})
}

# Run the application 
shinyApp(ui = ui, server = server)


