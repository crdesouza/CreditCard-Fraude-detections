library(shiny)
library(plyr)
library(dplyr)

setwd("~/ShinyApps")
load("~/ShinyApps/.RData")

ui <- tagList(
  
  navbarPage(
    title = "Detecção de Fraude",
    tabPanel("Cliente",
             h3("Consultar perfil do cliente"),
             fluidRow(
               column(3,
                      selectInput('dados_corretos', label="O cliente preencheu todos os dados corretamente?",choices=c(0,1), width = NULL
                      )),
               column(3,
                      selectInput('cpf_falecida', label="O CPF é de uma pessoa já falecida?",choices=c(0,1), width = NULL
                      )),
               column(3,
                      selectInput('cpf_sujo', label="O CPF está sujo na praça?",choices=c(0,1), width = NULL
                      ))
             ),
             
             fluidRow(column(3,
                             textInput('valor_max', label="Qual o valor máximo das compras anteriores?", width = NULL,
                                       placeholder = "0")),
                      column(3,
                             textInput('dias_ultima', label="A última compra foi há quantos dias?", width = NULL,
                                       placeholder = "0"))),
             fluidRow(
               column(3,
                      textInput('charge_back_dias', label="Quantos pedidos de chargeback no último ano?", width = NULL,
                                placeholder = "0" )),
               column(3,
                      textInput('valor', label="Qual o valor da compra?", width = NULL,
                                placeholder = "0")))
             ,
             
             textOutput("text")
             
    )
    
  )
)


server <- function(input, output, session) {

  dados_cliente <-reactive({
    data.frame(
    preencheu_corretamente=as.numeric(input$dados_corretos),
    cpf_falecida=as.numeric(input$cpf_falecida),
    cpf_sujo=as.numeric(input$cpf_sujo),
    valor_max=as.numeric(input$valor_max),
    dias_ultima=as.numeric(input$dias_ultima),
    charge_back=as.numeric(input$charge_back_dias),
    Valor=as.numeric(input$valor),
    Class="NA"
  )
  })
  
  
  probabilidade <- reactive({ modelo_logistico %>% predict(dados_cliente(), type = "response")})
  
  output$text <- renderText({ paste0("A probabilidade de ser uma fraude é de ",probabilidade()) })
}

shinyApp(ui = ui, server = server)
