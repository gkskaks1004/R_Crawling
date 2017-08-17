# This is the user-interface definition of a Shiny web application.# You can find out more about building applications with Shiny here:
## http://shiny.rstudio.com
##라이브러리 전처리
library(DBI)
library(RMySQL)
library(XML)
library(RCurl)
library(stringr)
library(rsconnect)
library(shiny)
library(rvest)
#닷컴 전처리
dotcomurl = "http://www.lotte.com/display/viewRankingZoneMain.lotte?disp_no=5548852&disp_grp_nm=%EC%84%9C%EB%B9%84%EC%8A%A4&upr_disp_no=&spick_disp_no=0&goods_sum_sct_cd=P1&goods_rnk_sct_cd=S&gen_sct_cd=A&age_sct_cd=A&dept_yn=&type=pc&tracking=RANKING_GCB_03#rankMiddle"
dotcomSOURCE = getURL(dotcomurl)
dotcomPARSED = htmlParse(dotcomSOURCE)
#결과를 담을 매트리스
dotcomresult <- matrix(nrow = 100, ncol = 2)
#상품명을 담는다
for(i in 1:100){  
  namexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[3]/a/p/text()")    
#닷컴 상품명 xpath 크롤링  
  dotcomname <- xpathSApply(dotcomPARSED, namexpath, xmlValue)  
#2가지 xpath 존재  
  if(!is.null(dotcomname)){     
    dotcomresult[i,1] <- dotcomname  
  }else{    
    namexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[2]/a/p/text()")    
    dotcomname <- xpathSApply(dotcomPARSED, namexpath, xmlValue)    
    dotcomresult[i,1] <- dotcomname  
  }}
for(i in 1:100){        
  #판매가의 경우  
  #pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[4]/p[1]/del")        
  #닷컴 상품 가격 xpath 크롤링  
  pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[4]/p[2]/strong")  
  dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)  
  if(is.list(dotcomprice)){    
    pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[4]/p[2]/span")    
    print(i)    
    dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)  
  }  
  if(is.null(dotcomprice)){    
    pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[3]/p[2]/strong")    
    dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)    
    if(is.null(dotcomprice)){      
      pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[4]/p[2]/span")      
      print(i)      
      dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)          
    }    
    if(is.null(dotcomprice)){      
      pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[4]/p/span")      
      dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)      
      print(i)      
      if(is.null(dotcomprice)){        
        pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[3]/p/span")        
        dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)        
        print(i)        
        if(is.null(dotcomprice)){          
          pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[3]/p[2]/strong")          
          dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)          
          print(i)        
        }      
      }    
    }  
  }  
  dotcomresult[i,2] <- dotcomprice}
shinyUI(
  fluidPage(  
    titlePanel("메인타이틀"),    
    sidebarLayout(    
      sidebarPanel(
        h2("메뉴"),                 
        br(),                 
        h4("실행버튼"),                 
        actionButton("per", label = "실행"),                 
        br(),                 
        h4("제출"),                 
        submitButton("제출"),                 
        h4("Single CheckBox"),                 
        checkboxInput("checkbox", label = "선택 1", value = TRUE),                 
        checkboxGroupInput("checkGroup",label = h4("CheckBox Group"),choices = list ("선택1" = 1, "선택2" = 2, "선택3" = 3),                                    selected = 2)),        mainPanel(h1("제목1"),              br(),              br(),              p(a("wOBA", href = "http://www.fangraphs.com/library/offense/woba/"),"는 타율의 아쉬운 부분을 보완해주는", strong("통계지표"), "로 '타석당 득점기대치'를 말한다."),              br(),              h2(dotcomresult[1,1])    )  )))
