##라이브러리 전처리
library(DBI)
library(RMySQL)
library(XML)
library(RCurl)
library(stringr)
library(rsconnect)
library(rvest)
library(shiny)
library(xtable)
source("test.R")

shinyServer(function(input, output, session) {
  colnames(dotcomresult) <- c("순위", "상품명", "가격")
  colnames(naverresult) <- c("채널명", "가격", "상품명")
  colnames(naverresult2) <- c("채널명", "가격", "상품명")
  colnames(naverresult3) <- c("채널명", "가격", "상품명")
  colnames(naverresult4) <- c("채널명", "가격", "상품명")
  colnames(naverresult5) <- c("채널명", "가격", "상품명")
  colnames(naverresult6) <- c("채널명", "가격", "상품명")
  colnames(naverresult7) <- c("채널명", "가격", "상품명")
  colnames(naverresult8) <- c("채널명", "가격", "상품명")
  colnames(naverresult9) <- c("채널명", "가격", "상품명")
  colnames(naverresult10) <- c("채널명", "가격", "상품명")
  #셀렉트 버튼 설정하여
  #네이버 쇼핑 상품 크롤링
  output$naver_shopping <- renderTable({
    mydata = naverresult
    if(input$var == "1위 상품"){
      mydata = naverresult
      naverresult[1:5,]
    }else if(input$var == "2위 상품"){
      mydata = naverresult2
      data(mydata)
      naverresult2[1:5,]
    }else if(input$var == "3위 상품"){
      mydata = naverresult3
      data(mydata)
      naverresult3[1:5,]
    }else if(input$var == "4위 상품"){
      mydata = naverresult4
      data(mydata)
      naverresult4[1:5,]
    }else if(input$var == "5위 상품"){
      mydata = naverresult5
      data(mydata)
      naverresult5[1:5,]
    }else if(input$var == "6위 상품"){
      mydata = naverresult6
      data(mydata)
      naverresult6[1:5,]
    }else if(input$var == "7위 상품"){
      mydata = naverresult7
      data(mydata)
      naverresult7[1:5,]
    }else if(input$var == "8위 상품"){
      mydata = naverresult8
      data(mydata)
      naverresult8[1:5,]
    }else if(input$var == "9위 상품"){
      mydata = naverresult9
      data(mydata)
      naverresult9[1:5,]
    }else if(input$var == "10위 상품"){
      mydata = naverresult10
      data(mydata)
      naverresult10[1:5,]
    }
    #data(mystr)
    #mystr[1:1,]
  }, caption = "네이버 쇼핑 최저가 쇼핑몰 목록",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL)
  )
  #닷컴 인기 랭킹 상품 100개 크롤링
  output$dotcom <- renderTable({
    data(dotcomresult)
    dotcomresult[1:10,]
  }, caption = "닷컴 반려동물 BEST 10 상품",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  
  
})
