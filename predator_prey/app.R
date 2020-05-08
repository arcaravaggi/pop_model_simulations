#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Lotka-Volterra models"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("prey_density","Prey density:",min = 1,max = 100,value = 5),
         sliderInput("predator_density","Predator density:",min = 1,max = 100,value = 2),
         sliderInput("prey_birth","Prey birth rate:",min = 0,max = 5,value = 1),
         sliderInput("consumption","Cosumption rate of prey by predators:",min = 0, max = 1, value = 0.2, step = 0.05),
         sliderInput("predator_birth","Predator birth rate:", min = 0, max = 1, value = 0.05, step = 0.05),
         sliderInput("predator_death","Predator death rate in the absence of prey:",min = 0,max = 2,value = 0.5, step = 0.1),
         sliderInput("iterations","Iterations:",min = 1,max = 50000,value = 5000)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot1"),
         plotOutput("plot2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  LVData <- reactive({
  Lotka_Volterra<-function(x_start=input$prey_density,y_start=input$predator_density,A=input$prey_birth,B=input$consumption,C=input$predator_birth,D=input$predator_death,iterations=input$iterations,time_step=0.01){
    x<-x_start
    y<-y_start
    dt<-time_step
    graph_frame<-c(0,x,y)
    for( i in 1:iterations)
    {
      dx<-(A*x-B*x*y)*dt
      dy<-(C*y*x-D*y)*dt
      
      x_new<-x+dx
      y_new<-y+dy
      
      new_data<-c(i*dt,x,y)
      
      graph_frame<-rbind(graph_frame,new_data)
      
      x<-x_new
      y<-y_new
    }
    return(graph_frame)
  } 
  tmp <- as.data.frame(Lotka_Volterra())
  })
  
  
  
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    ggplot(LVData(), aes(V1)) +
      geom_line(aes(y = V2, colour = "Prey")) + 
      geom_line(aes(y = V3, colour = "Predators")) +
      theme_classic() +
      theme(text = element_text(size=20)) +
      ylab("Population size") +
      xlab("Iterations (i.e. time)") +
      scale_color_discrete(name = "Population")
    
  })
  
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    ggplot(LVData(), aes(V2, V3)) +
      geom_point() +
      theme_classic() +
      theme(text = element_text(size=20)) +
      xlab("Prey population size") +
      ylab("Predator population size") 
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

