#닷컴 인기 랭킹 상품 크롤링-반려동물
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
  
  #컬럼명 수정
  colnames(dotcomresult) <- c("상품명", "가격")
  colnames(result) <- c("채널명", "가격")
}
#i = 11
#pricexpath <- paste0("//*[@id='contItem']/ul/li/div/ol/li[", i, "]/div/div[2]/div[4]/p[2]/span")
#dotcomprice <- xpathSApply(dotcomPARSED, pricexpath, xmlValue)
#dotcomresult[i,2] <- dotcomprice
