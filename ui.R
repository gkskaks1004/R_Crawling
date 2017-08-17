# This is the user-interface definition of a Shiny web application.# You can find out more about building applications with Shiny here:
## http://shiny.rstudio.com
library(shiny)

shinyUI(
  fluidPage(
    #theme = "bootstrap.css",
    headerPanel("동진♥문화의 크롤링 이야기"),
    #titlePanel("Dong-jin & Mun-hwa Internship Project 2017.8"),
    sidebarLayout( 
      tabsetPanel(
        div(tabPanel("Table", tableOutput("dotcom")), style = 'width:1000px;') 
      ), 
      
    mainPanel(
      selectInput("var", label = "상품명을 고르세요.", choices = c("1위 상품", "2위 상품", "3위 상품", "4위 상품", "5위 상품", "6위 상품", "7위 상품", "8위 상품", "9위 상품","10위 상품")),
      tableOutput("naver_shopping")
      )
    )
  )
)
