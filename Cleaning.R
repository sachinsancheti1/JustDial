library(dplyr)
library(stringr)
library(tidyr)
load("tab3updated.RData")
tab2[sample(nrow(tab2),3),]
# 
unique(tab2$address)
# [1] <NA>
#   Levels: 
cleaning_cols = function(x){
  cleaned.table = matrix(NA,ncol = dim(x)[2],nrow = dim(x)[1])
  col_names = names(x)
  colnames(cleaned.table) = col_names
  cleaned.table = as.data.frame(cleaned.table)
  for(i in 1:dim(cleaned.table)[2]){
    cleaned.table[,i] = str_trim(x[,i])
    cleaned.table[,i] = str_replace_all(string = cleaned.table[,i],
                                        pattern = c("\t"),
                                        replacement = "")
    cleaned.table[,i] = str_replace_all(string = cleaned.table[,i],
                                        pattern = c("\n"),
                                        replacement = "")
  }
  # Stage 1 of trimming edges done
  return(cleaned.table)
}

cleaning_addr = function(x,address = "addr",a = "A", b = "B"){
  rele.x = x[,c(address,a,b)]
  rele.x$Status = NA
  rele.x$Pincode = NA
  for(i in 1:dim(rele.x)[1]){
    if(!is.na(rele.x[i,address])){
      temp = strsplit(as.character(rele.x[i,address]), "|More...", fixed = T)
      rele.x[i,a] = temp[[1]][1]
      rele.x[i,b] = tryCatch(temp[[1]][2],error = function(e) return(""))
      rele.x[i,"Status"] = "OK"
      rele.x[i,"Pincode"]  = as.character(str_extract(rele.x[i,b],"[0-9][0-9][0-9][0-9][0-9][0-9]+")[[1]][1])
    }else{
      if(!is.na(rele.x[i,a])){
        rele.x[i,"Status"] = "OK"
        rele.x[i,"Pincode"]  = as.character(str_extract(rele.x[i,b],"[0-9][0-9][0-9][0-9][0-9][0-9]+")[[1]][1])
      }
    }
  }
  drops <- c(address,a,b)
  return(cbind(x[,!names(x) %in% drops],rele.x))
}

tab2.cleaned = tab2 %>%
  cleaning_cols %>%
  cleaning_addr