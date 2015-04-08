library(XML)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(stringr)
library(rjson)
library(shiny)
library(bitops)
library(RCurl)
parseIt <- function(x){
  txt = getURL(x)
  htmlTreeParse(txt,asText = TRUE,useInternalNodes = TRUE)
}
shinyServer(function(input, output) {
  output$jdtable <- renderDataTable({
    if (input$goButton == 0)
      return()
    input$goButton
    qry = isolate(input$search)
    cty = isolate(gsub(" ", "", input$city, fixed = TRUE))
    numpage = isolate(input$numpages)
    rt = data.frame(Name=character(),Number= character(),Address=character())
    withProgress(message = 'Extracting Table', value = 0, {
      
      for(j in 1:numpage){
        link = paste("http://www.justdial.com/",
                     cty,"/",
                     qry,"/page-",j,
                     sep = "")
        ps = parseIt(link)
        tc = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlAttrs)
        tt = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlValue)
        dt = data.frame(Name=character(),Number= character(),Address=character())
        for(i in 1:(length(tc)-7)){
          if(tc[[i]][1]=='jcnwrp'){
            name=as.character(tt[[i]][1])
            if(tc[[i+1]][1]=='jrcw')
              numb = as.character(tt[[i+1]][1])
            if(tc[[i+2]][1]=='jaid')
              addr = as.character(tt[[i+2]][1])
            i = i+2
            dt = rbind(dt,cbind(name,numb,addr))
            name<-numb<-addr<-NULL
          }
        }
        rt = rbind(rt,dt)
        incProgress(j/numpage, detail = paste("Doing part",100*j/numpage,"%"))
        Sys.sleep(0.1)
      }
      rt <- tryCatch({
        rt %>% separate(addr,into=c("A","B"),sep="More...")
      },
      error = function(e) return(rt)
      )
      rt
    })
  },
  options = list(pageLength = 100))
})
