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

data <- read.csv(file="movies.csv", header=TRUE, sep=",")
data <- na.omit(data)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("range","Slider:", 
                        min = 0,
                        max = 1000,
                        value = c(100,600)),
            
            textOutput("SliderText"),
            
            selectInput("x","X-axis", 
                        #c('Year', 'Body_Count', 'Length_Minutes', 'IMDB_Rating'), #
                        colnames(data), 
                        "Year"),
            
            selectInput("y","Y-axis", 
                        #c('Year', 'Body_Count', 'Length_Minutes', 'IMDB_Rating'),#
                        colnames(data), 
                        "Body_Count"),
            
            actionButton("go", "Go")
            ),
        
        # Show a plot of the generated distribution
        mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Tab 1", plotOutput("scatterplot1")),
                    tabPanel("Tab 2", tableOutput("table1")),
                    tabPanel("Tab 3", plotOutput("scatterplot2")),
                    
                    tabPanel("Tab 4", sidebarPanel(textOutput("t1"), 
                                                   textOutput("t2"),
                                                   tableOutput("table2")))
                    
                    )
        
            #plotOutput("scatterplot")
        ) # main
        
    ) # sidebarLayout
    
) # fuidpage

# Define server logic required to draw a histogram
server <- function(input, output) {
    d <- reactive({
        dist <- switch(input$x,
                       input$y)
        
        #dist(input$n)
    })
    
    output$table1 <- renderTable(head(data))
    #
    
    my_subset <- reactive({ cbind(input$range[1]:input$range[2])})
    output$SliderText <- renderText(nrow(subset(data, data$Body_Count %in% my_subset())))
    x_values<-eventReactive (input$go, {input$x})
    y_values<-eventReactive (input$go, {input$y})
    
   
    output$scatterplot1 <- renderPlot(ggplot(data=subset(data, data$Body_Count %in% my_subset()), 
                                            aes_string(x=x_values(), y=y_values())) +
                                            geom_point(size=3, color='blue') +                                  
                                            geom_smooth(method=lm, se=FALSE, fullrange=TRUE))
    
    output$scatterplot2 <- renderPlot(ggplot(data=data[ which(data$Body_Count > input$range[1] & data$Body_Count < input$range[2]) , ], 
                                            aes_string(x=input$x, y=input$y)) +
                                            geom_point(size=3, color='blue'))
    #data <- data[complete.cases(data), ]
    
    mean_values_t1 <- eventReactive (input$go, {u<-data[,input$x] 
                                     ifelse(is.numeric(u), mean(u), 'Non numeric')})
    mean_values_t2 <- eventReactive (input$go, {u<-data[,input$y] 
                                     ifelse(is.numeric(u), mean(u), 'Non numeric')})
    #mean_values_t2 <- eventReactive (input$go, {mean(data[,input$y])})
    
    
    
    output$t1<- renderText (mean_values_t1())
    output$t2<- renderText (mean_values_t2())
     
    head_table <- eventReactive (input$go, {(c(subset(data, data$Body_Count %in% my_subset())[input$x], 
                                               subset(data, data$Body_Count %in% my_subset())[input$y]))})
    output$table2 <- renderTable(head_table())
    
    #output$SliderText <- renderText(nrow(data[ which(data$Body_Count > input$range[1] & data$Body_Count < input$range[2]) , ]))
    #output$scatterplot <- renderPlot(ggplot(data=data[ which(data$Body_Count > input$range[1] & data$Body_Count < input$range[2]) , ], 
    #                                  aes_string(x=input$x, y=input$y)) +
    #                                  geom_point(size=3, color='blue'))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
