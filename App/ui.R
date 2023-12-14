
library(shiny)
library(DT)
library(shinyjs)
#install.packages("shinyjs")
library(shinythemes)
library(shinydashboard)


setwd("C:/Users/Evelyn/Documents/TIR/")

source("main.R")

ui <- dashboardPage(
    dashboardHeader(title = "Exibição de DataFrames"),
    dashboardSidebar(), 
    
    dashboardBody(

        
        fluidRow(
            column(width = 6,
                   h3("Tabela de Pagamentos:"),
                   dataTableOutput("table1")
            ),
            column(width = 6,
                   h3("Tabela de Entrada:"),
                   dataTableOutput("table2")
            ),
            column(width = 6,
                   h3("Tabela de Saldo no Fim do Exercício:"),
                   dataTableOutput("table3")
            )
        ),
        
        fluidRow(
            column(width = 6),
            column(width = 12,
                   h3("Apropriação:"),
                   dataTableOutput("table4")
            ),
            column(width = 12,
                   h3("Recálculo:"),
                   dataTableOutput("table5")
            )
        )
    )
)
server <- function(input, output) {
    output$table1 <- renderDataTable({
        tabela_pagamentos
    })
    
    output$table2 <- renderDataTable({
        df_input
    })
    
    output$table3 <- renderDataTable({
        saldo_fim_exercicio
    })
    
    output$table4 <- renderDataTable({
        tabela_apropriacao
    })
    
    output$table5 <- renderDataTable({
        recalculo
    })
}

shinyApp(ui = ui, server = server)
