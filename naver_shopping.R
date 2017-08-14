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

con <- dbConnect(MySQL(), user = "root", password = "0000", dbname = "crawler")
dbListTables(con)
dbListFields(con, "naver_shopping")
a <- con %>%dbGetQuery("select * from naver_shopping")
a[[1]] <- iconv(as.character(a[[1]]), from='UTF-8')
a[[2]] <- iconv(as.character(a[[2]]), from='UTF-8')

query2 <- "insert into naver_shopping values('11st', 'mango', 6000)"
dbSendStatement(con, query2)
