library(shiny)
library(plyr)
library(dplyr)

load(".RData")
ui <- tagList(
  
  navbarPage(
    title = "Fraud detection",
    tabPanel("Client",
             h3("Consult client profile"),
             fluidRow(
               column(3,
                      selectInput('correct_fill', label="Did the client fill in all the data correctly?",choices=c(0,1), width = NULL
                      )),
               column(3,
                      selectInput('cpf_died', label="Is the CPF of a person already deceased?",choices=c(0,1), width = NULL
                      ))),
             fluidRow(column(3,
                      selectInput('cpf_dirty', label="Is the CPF dirty in the market?",choices=c(0,1), width = NULL )),
                      column(2,textInput('days_last', label="How many days ago was the last purchase?", width = NULL,
                                       placeholder = "0"))),
             fluidRow(column(3,
                             textInput('max_value', label="What is the maximum value of previous purchases?", width = NULL,
                                       placeholder = "0")),
               column(3,textInput('amount', label="What is the purchase price?", width = NULL,
                                placeholder = "0"))),
             
             textOutput("text"))))


server <- function(input, output, session) {

  data_client <-reactive({
    data.frame(correct_fill=as.numeric(input$correct_fill),
    cpf_died=as.numeric(input$cpf_died),
    cpf_dirty=as.numeric(input$cpf_dirty),
    days_last=as.numeric(input$days_last),
    max_value=as.numeric(input$max_value),
    amount=as.numeric(input$amount),
    Class="NA")
  })
  
  

  probability <- reactive({ log_mod2 %>% predict(data_client(), type = "response")})
  
  output$text <- renderText({ paste0("The probability of being a fraud is ",probability()) })
}

shinyApp(ui = ui, server = server)
