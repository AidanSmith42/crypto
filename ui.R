library(tidyverse)
library(quantmod)

library(shiny)
library(shinydashboard)
library(shinythemes)
library(data.table)
library(DT)


navbarPage("Portfolio Balance", theme = shinytheme("flatly"),
           
           
           tabPanel(title="Total",
                    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
                    tags$head(HTML('<style>* {font-size: 98%; font-family: Roboto Mono;}</style>')),
                    box(width="100%",
                        fluidRow(
                          column(6,
                                 wellPanel(style='background--color: #c1cbd4; border-color:#2c3e50;',
                                           plotOutput('balance', width = 800, height= "auto"))),
                          column(6,
                                 wellPanel(style='background--color: #c1cbd4; border-color:#2c3e50;',
                                           #selectInput("sections","Select Number of Sections: ", choices = 4, selected= 4),
                                           DTOutput("chart")))
                          
                        )
                    )
           )
)