server <- function(input, output, session) {
  len <- reactive({input$xcol})
  if (len() == "two-dimensional")){
    output$plot1 <- renderPlot({
      plot(pc$x[,1], pc$x[,2],col=km$cluster,pch=16, xlab = "principle component 1", ylab = "principle component 2")
      pc_bind<-cbind(pc$x[,1], pc$x[,2])
      ordispider(pc_bind, factor(km$cluster), label = TRUE)
      ordihull(pc_bind, factor(km$cluster), lty = "dotted")
    })
  } else{
      # three dimensional
      output$plot1 <- renderPlot({scatter3d(x = pc$x[,1], y = pc$x[,2], z = pc$x[,3], groups = as.factor(km$cluster),
                surface=FALSE, ellipsoid = TRUE)})
      print(len)
      
      }
}
