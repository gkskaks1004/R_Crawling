# This is the user-interface definition of a Shiny web application.# You can find out more about building applications with Shiny here:
## http://shiny.rstudio.com
library(shiny)

shinyUI(
  fluidPage(
    #theme = "bootstrap.css",
    headerPanel("The Best Team D&M"),
    #titlePanel("Dong-jin & Mun-hwa Internship Project 2017.8"),
    
    sidebarLayout(
      sidebarPanel(
        tableOutput("dotcom")
      ),
  
    mainPanel(
  tableOutput("naver_shopping")
  )
    )
)
)
