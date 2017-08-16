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
  colnames(dotcomresult) <- c("상품명", "가격")
  colnames(naverresult) <- c("채널명", "가격")
  #닷컴 인기 랭킹 상품 100개 크롤링
  output$dotcom <- renderTable({
    data(dotcomresult)
    dotcomresult[1:100,]
  }, caption = "Dotcom Crawling Data",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  #네이버 쇼핑 상품 크롤링
  output$naver_shopping <- renderTable({
    data(naverresult)
    naverresult[1:5,]
    #data(mystr)
    #mystr[1:1,]
  }, caption = "Naver_shopping Crawling Data",
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
})
