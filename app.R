library(shiny)
library(rsconnect)
source("ETL.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Titanic EDA"),
      radioButtons("radio_survive", "Survived (1=Survived)", c(1,0)),

  sidebarLayout(
        sidebarPanel(
            sliderInput("slider", "Keep the passengers who are older than", 1, 100,50)),
        
            mainPanel(
              plotOutput("hist")
)),
sidebarLayout(
  sidebarPanel(
    radioButtons("radio", "Gender", c('male','female'))),
    mainPanel(
      plotOutput("dist"),
      tableOutput("table")))
  
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$hist <- renderPlot({
     train_sub <- filter(train, Age>=input$slider  & Survived==input$radio_survive)
     p <- ggplot(train_sub,aes(Sex))
     p + geom_bar() + theme_classic()+labs(
                                           title ="Nbr of people by gender")

   })
   output$dist <- renderPlot({
     train_sex <- filter(train, Age>=input$slider & Sex==input$radio & Survived==input$radio_survive)
     dist <- ggplot(train_sex,aes(Age))
     dist + geom_histogram(binwidth = 5) + theme_classic()+labs(
       title ="Distribution of age")
   })
   output$table <- renderTable({
     train_sex <- filter(train, Age>=input$slider & Sex==input$radio & Survived==input$radio_survive)
     train_sex
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

