#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(xtable)

#네이버 쇼핑 크롤링
url = "http://shopping.naver.com/search/all.nhn?query=%5B%EB%89%B4%ED%8A%B8%EB%A6%AC%EB%82%98%5D%E2%97%86%5B%EA%B1%B4%EA%B0%95%EB%B0%B1%EC%84%9C%5D%20%EC%A0%84%EC%97%B0%EB%A0%B9%EC%9A%A9%20%EB%A7%90%ED%8B%B0%EC%A6%88%206KG&pagingIndex=1&pagingSize=40&viewType=list&sort=price_asc&frm=NVSHATC"
SOURCE = getURL(url)
PARSED = htmlParse(SOURCE)
once <- "//*[@id='_search_list']/div[1]/ul/li[2]/div[2]/span[1]"

xpath1 <- "//*[@id='_search_list']/div[1]/ul/li["
xpath2 <- "]/div[2]/span[1]/em/span"
line <- readLines(url, encoding = "UTF-8")
channel <- line[str_detect(line, "<a href=\"#\" class=\"mall_more _mall")]
channel <- gsub("<|>|\"|=|_|!|:|.-|#|\t", "", channel)
x = 1
for(i in 1:5){
  mystr <- str_locate(channel[i], "title")
  mystr2 <- str_locate(channel[i], "상")
  name <- str_sub(channel[i], start = mystr[2]+1, end = mystr2[1]-1)
  
  xpath <- paste0(xpath1, i+1, xpath2)
  price = xpathSApply(PARSED, xpath, xmlValue)
  print(name)
  if(x<2){
    price = xpathSApply(PARSED, once, xmlValue)
    price <- gsub("\n|\t", "", price)
    print(price)
    x = 2
  }
  else{
    print(price)
  }
}

#닷컴 크롤링
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
  }
}

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
  dotcomresult[i,2] <- dotcomprice
}

#DB 연결
con <- dbConnect(MySQL(), user = "root", password = "0000", dbname = "crawler")
dbListTables(con)
dbListFields(con, "naver_shopping")
a <- con %>%dbGetQuery("select * from naver_shopping")
a[[1]] <- iconv(as.character(a[[1]]), from='UTF-8')
a[[2]] <- iconv(as.character(a[[2]]), from='UTF-8')

query2 <- "insert into naver_shopping values('11st', 'mango', 6000)"
dbSendStatement(con, query2)


#shiny 서버
shinyServer(function(input, output, session) {
  
  #닷컴 인기 랭킹 상품 100개 크롤링
  output$dotcom <- renderTable({
    data(dotcomresult)
    dotcomresult[1:100,]
  }, caption = "Dotcom Crawling data",
     caption.placement = getOption("xtable.caption.placement", "top"),
     caption.width = getOption("xtable.caption.width", NULL)
  )
  
  #네이버 쇼핑 상품 크롤링
  output$naver_shopping <- renderTable({
    data(mystr)
    mystr[1:1,]
  }, caption = "Naver_shopping Crawling data",
     caption.placement = getOption("xtable.caption.placement", "top"),
     caption.width = getOption("xtable.caption.width", NULL)
  )
  
})
