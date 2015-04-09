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
  output$downloadData <- downloadHandler(
    filename = function() {
      qry = isolate(input$search)
      cty = isolate(gsub(" ", "", input$city, fixed = TRUE))
      numpage = isolate(input$numpages)
      paste(paste("Extract",qry,cty,numpage,sep = "-"),".csv",sep="")
    },
    content = function(con) {
      write.csv(datatab(), con)
    }
  )
  
  datatab <- reactive({
    if (input$goButton == 0)
      return()
    input$goButton
    qry = isolate(input$search)
    cty = isolate(gsub(" ", "", input$city, fixed = TRUE))
    numpage = isolate(input$numpages)
    rt = data.frame(Name=character(),Number= character(),Address=character(), Links = character(), Site = character())
    withProgress(message = 'Extracting Table', value = 0, {
      
      for(j in 1:numpage){
        link = paste("http://www.justdial.com/",
                     cty,"/",
                     qry,"/page-",j,
                     sep = "")
        ps = parseIt(link)
        tc = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlAttrs)
        tt = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlValue)
        tlink = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlChildren)
        dt = data.frame(Name=character(),Number= character(),Address=character(), Links = character(), Site = character())
        for(i in 1:(length(tc)-7)){
          if(tc[[i]][1]=='jcnwrp'){
            name=as.character(tt[[i]][1])
            rr = tryCatch(xmlAttrs(xmlChildren(tlink[[i]]$span)$a) %>% as.data.frame,
                          error = function(e) NULL)
            site = ""
            cat(name,site)
            if(!is.null(rr)){
              links = tryCatch(rr["href",] %>% as.character,
                               error = function(e) NULL)
              if(!is.null(links)){
                newparse = tryCatch(parseIt(links),error = function(e) site="")
                newp1 = tryCatch(xpathApply(newparse,'//section/section/div/section/section/section/section/section/aside/p/a',fun = xmlValue),
                                 error = function(e) tryCatch(xpathApply(newparse,'//section/section/div/section/section/section/section/section/section/aside/p/a',fun = xmlValue),
                                                              error = function(e) site=list("")
                                 )
                )
                if(length(newp1)==0){
                  newp1 = tryCatch(xpathApply(newparse,'//section/section/div/section/section/section/section/section/section/aside/p/a',fun = xmlValue),
                                   error = function(e) site=list("")
                  )
                }
                site = as.character(unlist(newp1))
                if(is.null(site))
                  site = ""
              }
            }
            cat(site)
            if(tc[[i+1]][1]=='jrcw')
              numb = as.character(tt[[i+1]][1])
            if(tc[[i+2]][1]=='jaid')
              addr = as.character(tt[[i+2]][1])
            i = i+2
            dt = rbind(dt,cbind(name,numb,addr,links,site))
            name<-numb<-addr<-links<-NULL
          }
          
          incProgress(i*24/100*numpage*24, detail = paste("Doing part",round(i*(24)/(numpage*24),2),"%"))
          Sys.sleep(0.1)
        }
        rt = rbind(rt,dt)
      }
      rt <- tryCatch({
        rt %>% separate(addr,into=c("A","B"),sep="More...")
      },
      error = function(e) return(rt)
      )
      apply(rt,2,str_trim)
    })
  })
  
  output$jdtable <- renderDataTable(
    datatab(),
    options = list(pageLength = 100))
})