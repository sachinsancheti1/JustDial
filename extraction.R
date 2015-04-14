library(XML)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(stringr)
library(shiny)
library(bitops)
parseIt <- function(x){
  htmlTreeParse(x,useInternalNodes = TRUE)
}

datatab1 <- function(qry,cty){
  numpage = 100
  rt = data.frame(name=character(),numb= character(),address=character(), links = character(), site = character())    
  for(j in 1:numpage){
    link = paste("http://www.justdial.com/",
                 cty,"/",
                 qry,"/page-",j,
                 sep = "")
    cat("Parsing from:",link,"\n")
    ps = tryCatch(parseIt(link), error = function(e) {cat("Unable to parse",link);break})
    tc = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlAttrs)
    tt = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlValue)
    tlink = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlChildren)
    dt = data.frame(name=character(),numb= character(),address=character(), links = character(), site = character())
    if(length(tc)<4) break
    for(i in 1:(max(1,length(tc)-3))){
      if(tc[[i]][1]=='jcnwrp'){
        name=as.character(tt[[i]][1])
        rr = tryCatch(xmlAttrs(xmlChildren(tlink[[i]]$span)$a) %>% as.data.frame,
                      error = function(e) NULL)
        site = "_"
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
          }
        }
        if(is.null(site) | length(site)==0)
          site = "_"
        if(tc[[i+1]][1]=='jrcw')
          numb = tryCatch(as.character(tt[[i+1]][1]),error = function(e) return("_"))
        if(tc[[i+2]][1]=='jaid')
          addr = tryCatch(as.character(tt[[i+2]][1]),error = function(e) return("_"))
        i = i+2            
        #             cat(paste("name:",str_trim(name),
        #                       "numb:",str_trim(numb),
        #                       "addr:",str_trim(addr),
        #                       "links:",str_trim(links),
        #                       "site:",unlist(str_trim(site)),
        #                       "the end",sep="\n"))
        #             cat("_____________________________ \n")
        if(is.null(name) | length(name)==0){
          cat("Name is empty\n");name=""
        }
        
        if(is.null(numb) | length(numb)==0){
          cat("Num is empty\n");numb = ""
        }
        if(is.null(addr) | length(addr)==0)
          cat("Address is empty\n")
        if(is.null(links) | length(links)==0)
          cat("Links is empty\n")
        if(is.null(site) | length(site)==0)
          cat("Site is empty\n")
        dt = rbind(dt,cbind(name,numb,addr,links,site))
        Sys.sleep(0.1)
      }
    }
    rt = plyr::rbind.fill(rt,dt)
    rt <- tryCatch(rt %>% separate(addr,into=c("A","B"),sep="More..."),
                   error = function(e) return(rt))
  }
  return(apply(rt,2,str_trim) %>% as.data.frame)
}

professions <- c("Architects",
                 "Lawyers",
                 "Advocates",
                 "Civil-Engineers",
                 "Lorry-Transport",
                 "Clubs",
                 "Associations",
                 "Banks",
                 "Surveyors",
                 "Industries",
                 "Factory",
                 "Forest-Department",
                 "Public-Hospitals",
                 "Private-Hospitals",
                 "Private-Tutors",
                 "Welfare-Organisations",
                 "Samaj",
                 "Resorts",
                 "Hotels",
                 "Colleges",
                 "Schools",
                 "Teachers",
                 "Mills",
                 "Government-Organisations",
                 "BPO",
                 "Transporters",
                 "Body-Massage-Centres",
                 "Beauty-Spas",
                 "Insurance-Agents",
                 "Mutual-Fund-Agents",
                 "Car-Dealers",
                 "Doctors",
                 "Universities",
                 "Study Centre",
                 "Tea Manufacturers",
                 "Tea Bag Manufacturers",
                 "Mobile Phone Dealers",
                 "Auditors",
                 "Carpenters",
                 "Hostels",
                 "Supermarkets",
                 "Libraries",
                 "HR-Consulting",
                 "Advertising-Agencies",
                 "Bakeries",
                 "Post-Office-Services",
                 "Domestic Courier Services",
                 "Jewellers",
                 "Real-Estate-Agents",
                 "Estate-Agents-For-Residence",
                 "Estate-Agents-For-Plot",
                 "Computer-Dealers",
                 "Electricians",
                 "Security-Services",
                 "Air-Hostess-Training-Institutes",
                 "Taxi-Services",
                 "Photo-Shops",
                 "Clothing",
                 "Textile-Manufacturers",
                 "Iron-Manufacturers",
                 "Cast-Iron-Manufacturers")
locs <- c("Coonoor","Coimbatore","Ooty")


for(i in 45:length(professions)){
  for(j in 1:length(locs)){
    tab1 = datatab1(professions[i],locs[j])
    if(dim(tab1)[1]>0){
      tab1$profession = as.character(professions[i])
      tab1$locs = as.character(locs[j])
      tab2 = plyr::rbind.fill(tab2,tab1) %>% as.data.frame
    }
  }
}

#Run 1 i = 9;j=2
#Run 2 i = 9:41;j=1 to 1
#Run 3 i = 9:44;j=1 to 1
#Run 4 i = 45:58;j=1:3
#Run 5 i = 59:61;j=1:3
