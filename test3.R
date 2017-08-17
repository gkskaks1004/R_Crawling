library(DBI)
library(RMySQL)
library(XML)
library(RCurl)
library(stringr)
library(rsconnect)
library(rvest)
library(shiny)
library(xtable)
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
    dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)  
  }  
  if(is.null(dotcomprice)){    
    pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[3]/p[2]/strong")    
    dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)    
    if(is.null(dotcomprice)){      
      pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[4]/p[2]/span")      
      dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)          
    }    
    if(is.null(dotcomprice)){      
      pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[4]/p/span")      
      dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)      
      if(is.null(dotcomprice)){        
        pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[3]/p/span")        
        dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)        
        if(is.null(dotcomprice)){          
          pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[3]/p[2]/strong")          
          dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)          
        }      
      }    
    }  
  }  
  dotcomresult[i,2] <- dotcomprice
}
url = "http://shopping.naver.com/search/all.nhn?query=dodocool%20usb&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATCC"
mystring = dotcomresult[1,1]
naverurl = paste0("http://shopping.naver.com/search/all.nhn?query=", mystring, "&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC")
SOURCE = getURL(naverurl)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(naverurl, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)
x = 1

#크롤링한 결과를 저장하는 매트릭스
naverresult <- matrix(nrow = 5, ncol = 2)
for(i in 1:5){
  mystr <- str_locate(channel[i], "title")
  mystr2 <- str_locate(channel[i], "span")
  name <- str_sub(channel[i], start = mystr[2]+1, end = mystr2[1]-1)
  name2 <- str_sub(name, start = 1, end = length(name)-8)
  xpath <- paste0(xpath1, i, xpath2)
  price = xpathSApply(PARSED, xpath, xmlValue)
  #print(name2)
  #print(price)
  naverresult[i,1] <- name2
  naverresult[i,2] <- price
} 
