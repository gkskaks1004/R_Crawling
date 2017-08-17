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

dotcomresult <- matrix(nrow = 100, ncol = 3)
#1~10등 리스트
for(k in 1:100){
  dotcomresult[k,1] <- paste0(k,"등 상품")
}
#상품명을 담는다
for(i in 1:100){  
  namexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[3]/a/p/text()")    
  #닷컴 상품명 xpath 크롤링  
  dotcomname <- xpathSApply(dotcomPARSED, namexpath, xmlValue)  
  #2가지 xpath 존재  
  if(!is.null(dotcomname)){
    #dotcomname = str_replace(dotcomname, "◆", "")
    dotcomresult[i,2] <- dotcomname  
  }else{    
    namexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[2]/a/p/text()")    
    dotcomname <- xpathSApply(dotcomPARSED, namexpath, xmlValue)    
    #dotcomname = str_replace(dotcomname, "◆", "")
    dotcomresult[i,2] <- dotcomname  
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
  dotcomresult[i,3] <- dotcomprice
}
#네이버 상품명 코드
mypath1 = "//*[@id='_search_list']/div[1]/ul/li["
mypath2 = "]/div[2]/a"
#1등
url = "http://shopping.naver.com/search/all.nhn?query=%EA%B3%A0%EC%96%91%EC%9D%B4%20%EC%B1%A0%EC%98%A4%EC%B8%84%EB%A5%B4%20%EB%8C%80%EC%9A%A9%EB%9F%89%2060p%20%EC%B0%B8%EC%B9%98%20%EB%98%90%EB%8A%94%20%EA%B0%80%EB%8B%A4%EB%9E%91%EC%96%B4%20%ED%83%9D1&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult[1,3] <- item
naverresult[1,1] <- name
naverresult[1,2] <- price

#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult[2,3] <- item
naverresult[2,1] <- name
naverresult[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult[3,3] <- item
naverresult[3,1] <- name
naverresult[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult[4,3] <- item
naverresult[4,1] <- name
naverresult[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult[5,3] <- item
  naverresult[5,1] <- name
  naverresult[5,2] <- price
}
#2등
url = "http://shopping.naver.com/search/all.nhn?query=%EC%9A%94%EC%9A%94%EC%89%AC%20%ED%8C%A8%EB%93%9C%2050%EB%A7%A4%201%2B1&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC&sps=N"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult2 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult2[1,3] <- item
naverresult2[1,1] <- name
naverresult2[1,2] <- price
#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult2[2,3] <- item
naverresult2[2,1] <- name
naverresult2[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult2[3,3] <- item
naverresult2[3,1] <- name
naverresult2[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult2[4,3] <- item
naverresult2[4,1] <- name
naverresult2[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult2[5,3] <- item
  naverresult2[5,1] <- name
  naverresult2[5,2] <- price
}
#3등
url = "http://shopping.naver.com/search/all.nhn?query=%EB%B0%94%EB%B9%84%EC%98%A8%20SBC-7710%20%EB%A6%AC%ED%8A%AC%EB%94%94%EC%A7%80%ED%84%B8%20%EC%95%A0%EA%B2%AC%EC%9D%B4%EB%B0%9C%EA%B8%B0&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult3 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult3[1,3] <- item
naverresult3[1,1] <- name
naverresult3[1,2] <- price
#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult3[2,3] <- item
naverresult3[2,1] <- name
naverresult3[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult3[3,3] <- item
naverresult3[3,1] <- name
naverresult3[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult3[4,3] <- item
naverresult3[4,1] <- name
naverresult3[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult3[5,3] <- item
  naverresult3[5,1] <- name
  naverresult3[5,2] <- price
}
#4등
url = "http://shopping.naver.com/search/all.nhn?query=%ED%8C%A8%EB%93%9C%EC%97%86%EC%9D%B4+%EC%93%B0%EB%8A%94+%EC%84%B8%EC%9B%80%EB%B0%B0%EB%B3%80%ED%8C%90+%EC%A4%91%EB%8C%80%ED%98%95+%EC%82%AC%EC%9D%B4%EC%A6%88&cat_id=&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult4 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
if(!is.na(name)){
  xpath <- paste0(xpath1, 1, xpath2)
  price = xpathSApply(PARSED, xpath, xmlValue)
  naverresult4[1,1] <- name
  naverresult4[1,2] <- price
  #2
  mystr <- str_locate(channel[2], "title")
  mystr2 <- str_locate(channel[2], "상")
  name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
  #name2 <- str_sub(name, start = 1, end = length(name)-8)
  xpath <- paste0(xpath1, 2, xpath2)
  price = xpathSApply(PARSED, xpath, xmlValue)
  naverresult4[2,1] <- name
  naverresult4[2,2] <- price
  #3
  mystr <- str_locate(channel[3], "title")
  mystr2 <- str_locate(channel[3], "상")
  name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
  #name2 <- str_sub(name, start = 1, end = length(name)-8)
  xpath <- paste0(xpath1, 3, xpath2)
  price = xpathSApply(PARSED, xpath, xmlValue)
  naverresult4[3,1] <- name
  naverresult4[3,2] <- price
  #4
  mystr <- str_locate(channel[4], "title")
  mystr2 <- str_locate(channel[4], "상")
  name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
  #name2 <- str_sub(name, start = 1, end = length(name)-8)
  xpath <- paste0(xpath1, 4, xpath2)
  price = xpathSApply(PARSED, xpath, xmlValue)
  naverresult4[4,1] <- name
  naverresult4[4,2] <- price
  #5
  mystr <- str_locate(channel[5], "title")
  mystr2 <- str_locate(channel[5], "상")
  name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
  #name2 <- str_sub(name, start = 1, end = length(name)-8)
  xpath <- paste0(xpath1, 5, xpath2)
  price = xpathSApply(PARSED, xpath, xmlValue)
  if(!is.null(price)){
    naverresult4[5,1] <- name
    naverresult4[5,2] <- price
  }
}

#5등
url = "http://shopping.naver.com/search/all.nhn?query=CIAO%20%EC%B1%A0%EC%98%A4%EC%B8%84%EB%A5%B4%20%EB%8C%80%EC%9A%A9%EB%9F%89%20%EB%AA%A8%EC%9D%8C%EC%A0%84%20%EA%B3%A0%EC%96%91%EC%9D%B4%20%EB%A7%88%EC%95%BD%EA%B0%84%EC%8B%9D&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult5 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult5[1,3] <- item
naverresult5[1,1] <- name
naverresult5[1,2] <- price
#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult5[2,3] <- item
naverresult5[2,1] <- name
naverresult5[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult5[3,3] <- item
naverresult5[3,1] <- name
naverresult5[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult5[4,3] <- item
naverresult5[4,1] <- name
naverresult5[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult5[5,3] <- item
  naverresult5[5,1] <- name
  naverresult5[5,2] <- price
}
#6등
url = "http://shopping.naver.com/search/all.nhn?query=%EC%BA%A3%EB%AA%A8%EB%82%98%EC%9D%B4%ED%8A%B8%20%EC%8A%A4%ED%81%AC%EB%9E%98%EC%B3%90&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult6 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult6[1,3] <- item
naverresult6[1,1] <- name
naverresult6[1,2] <- price
#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult6[2,3] <- item
naverresult6[2,1] <- name
naverresult6[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult6[3,3] <- item
naverresult6[3,1] <- name
naverresult6[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult6[4,3] <- item
naverresult6[4,1] <- name
naverresult6[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult6[5,3] <- item
  naverresult6[5,1] <- name
  naverresult6[5,2] <- price
}
#7등
url = "http://shopping.naver.com/search/all.nhn?query=%5B%ED%8C%A8%EB%93%9C4P%2B%EB%A7%88%EB%AF%B8%ED%8F%AC%EC%BD%94%20%EB%AC%BC%ED%8B%B0%EC%8A%884%ED%8C%A9%20%EC%A6%9D%EC%A0%95%5D%20%EC%9C%A0%EB%8B%88%EC%B0%B8%20%EB%8D%B0%EC%98%A4%ED%86%A0%EC%9D%BC%EB%A0%9B%20%ED%9B%84%EB%93%9C%ED%98%95%20%EC%8B%9C%EC%8A%A4%ED%85%9C%20%ED%99%94%EC%9E%A5%EC%8B%A4&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult7 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult7[1,3] <- item
naverresult7[1,1] <- name
naverresult7[1,2] <- price
#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult7[2,3] <- item
naverresult7[2,1] <- name
naverresult7[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult7[3,3] <- item
naverresult7[3,1] <- name
naverresult7[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult7[4,3] <- item
naverresult7[4,1] <- name
naverresult7[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult7[5,3] <- item
  naverresult7[5,1] <- name
  naverresult7[5,2] <- price
}

#8등
url = "http://shopping.naver.com/search/all.nhn?query=%EC%9C%A0%EB%8B%88%EC%B0%B8%20%EA%B0%90%EC%82%AC%EC%A0%9C%20%EB%8D%B0%EC%98%A4%ED%86%A0%EC%9D%BC%EB%A0%9B%20%EC%82%AC%EB%A7%89%ED%99%94%20%EB%B0%A9%EC%A7%80%20%EC%86%8C%EC%B7%A8%20%ED%95%AD%EA%B7%A0%20%EB%AA%A8%EB%9E%98%204L%20X%202EA%20%ED%8C%A8%EB%93%9C%204P%20%EC%A6%9D%EC%A0%95&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult8 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult8[1,3] <- item
naverresult8[1,1] <- name
naverresult8[1,2] <- price
#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult8[2,3] <- item
naverresult8[2,1] <- name
naverresult8[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult8[3,3] <- item
naverresult8[3,1] <- name
naverresult8[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult8[4,3] <- item
naverresult8[4,1] <- name
naverresult8[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult8[5,3] <- item
  naverresult8[5,1] <- name
  naverresult8[5,2] <- price
}
#9등
url = "http://shopping.naver.com/search/all.nhn?query=%EB%B2%A0%EC%9D%B4%EC%A7%812%EB%8B%A8%EC%8A%A4%ED%85%9D&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult9 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult9[1,3] <- item
naverresult9[1,1] <- name
naverresult9[1,2] <- price
#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult9[2,3] <- item
naverresult9[2,1] <- name
naverresult9[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult9[3,3] <- item
naverresult9[3,1] <- name
naverresult9[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult9[4,3] <- item
naverresult9[4,1] <- name
naverresult9[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult9[5,3] <- item
  naverresult9[5,1] <- name
  naverresult9[5,2] <- price
}
#10등
url = "http://shopping.naver.com/search/all.nhn?query=%ED%93%A8%EB%A6%AC%EB%82%98%20%EC%95%8C%ED%8F%AC%201%EC%84%B8%EC%9D%B4%EC%83%81%20%EC%84%B1%EA%B2%AC%EC%9A%A9%20%EC%86%8C%EA%B3%A0%EA%B8%B0%2C%EA%B0%84%EA%B3%BC%20%EC%95%BC%EC%B1%84%EB%A7%9B%2010KG&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
#once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)

#크롤링한 결과를 저장하는 매트릭스
naverresult10 <- matrix(nrow = 5, ncol = 3)
#1
mystr <- str_locate(channel[1], "title")
mystr2 <- str_locate(channel[1], "상")
name <- str_sub(channel[1], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 1, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 1, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult10[1,3] <- item
naverresult10[1,1] <- name
naverresult10[1,2] <- price
#2
mystr <- str_locate(channel[2], "title")
mystr2 <- str_locate(channel[2], "상")
name <- str_sub(channel[2], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 2, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 2, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult10[2,3] <- item
naverresult10[2,1] <- name
naverresult10[2,2] <- price
#3
mystr <- str_locate(channel[3], "title")
mystr2 <- str_locate(channel[3], "상")
name <- str_sub(channel[3], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 3, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 3, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult10[3,3] <- item
naverresult10[3,1] <- name
naverresult10[3,2] <- price
#4
mystr <- str_locate(channel[4], "title")
mystr2 <- str_locate(channel[4], "상")
name <- str_sub(channel[4], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 4, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
myxpath = paste0(mypath1, 4, mypath2)
item = xpathSApply(PARSED, myxpath, xmlValue)
naverresult10[4,3] <- item
naverresult10[4,1] <- name
naverresult10[4,2] <- price
#5
mystr <- str_locate(channel[5], "title")
mystr2 <- str_locate(channel[5], "상")
name <- str_sub(channel[5], start = mystr[2]+1, end = mystr2[1]-1)
#name2 <- str_sub(name, start = 1, end = length(name)-8)
xpath <- paste0(xpath1, 5, xpath2)
price = xpathSApply(PARSED, xpath, xmlValue)
if(!is.null(price)){
  myxpath = paste0(mypath1, 5, mypath2)
  item = xpathSApply(PARSED, myxpath, xmlValue)
  naverresult10[5,3] <- item
  naverresult10[5,1] <- name
  naverresult10[5,2] <- price
}
