require(Rcpp)
require(ggplot2)
require(ggmap)
require(gdata)
require(dplyr)
require(ggvis)
require(leaflet)
require(stringr)
convert.magic <- function(obj,types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}

distfromGG <- function(x){
  if(sum(class(x) %in% "data.frame")>0){
    totalruns = dim(x)[1]
  }else{totalruns = length(x)}
  gokul.gardens=c(76.9842621,11.2094802)
  all.data.dist=data.frame(
    from=character(),
    to=character(),
    m=character(),
    km=character(),
    miles=character(),
    seconds=character(),
    minutes=character(),
    hours=character())
  for(i in 1:totalruns){
    data.dist = tryCatch(mapdist(gokul.gardens,x[i],mode = 'driving'),
                         error = function(e) data.frame(
                           from=as.character(""),
                           to=as.character(""),
                           m=as.character(""),
                           km=as.character(""),
                           miles=as.character(""),
                           seconds=as.character(""),
                           minutes=as.character(""),
                           hours=as.character(""))
    )
    all.data.dist = plyr::rbind.fill(all.data.dist,data.dist)
  }
  all.data.dist
}

last <- function(x) { tail(x, n = 1) }

findlocation <- function(x,place,city,pincode){
  if(sum(class(x) %in% "data.frame")>0){
    totalruns = dim(x)[1]
  }else{totalruns = length(x)}
  all.data.location = data.frame(
    Address = character(),
    Status = character(),
    Town = character(),
    Latitude = character(),
    Longitude = character())
  for(i in 1:totalruns){
    apiget = geocode(as.character(x[i]),output="all")
    #cat("1 done")
    if(is.null(apiget)){
      data.location = data.frame(
        Address = x[i],
        Status="VOID",
        Town ="",
        Latitude = "",
        Longitude = "")
    }else{
      if(apiget$status!="OK"){
        newtown = str_split(str_split(x[i],"( - [0-9][0-9][0-9][0-9][0-9][0-9])")[[1]][1],",")[[1]] %>% last
        apiget = geocode(as.character(newtown,paste(place[i],city[i],pincode[i])),output="all")
        #cat("2 done")
        if(apiget$status!="OK"){
          apiget = geocode(as.character(paste(newtown,city[i],pincode[i])),output="all")
          # cat("3 done")
        }
        data.location = data.frame(
          Address = x[i],
          Status=apiget$status,
          Town =tryCatch(as.character(apiget$results[[1]]$formatted_address),
                         error = function(e) return("")),
          Latitude = tryCatch(as.character(apiget$results[[1]]$geometry$location$lat),
                              error = function(e) return("")),
          Longitude = tryCatch(as.character(apiget$results[[1]]$geometry$location$lng),
                               error = function(e) return("")))
      }else{
        data.location = data.frame(
          Address = x[i],
          Status = apiget$status,
          Town = as.character(apiget$results[[1]]$formatted_address),
          Latitude = as.character(apiget$results[[1]]$geometry$location$lat),
          Longitude = as.character(apiget$results[[1]]$geometry$location$lng))
      }  
    }
    all.data.location = rbind(all.data.location,data.location)
    cat(data.location$Status)
  }  
  return(all.data.location)
}

dataset = tab2.cleaned
uniquedataset = dataset %>% tbl_df %>%
  select(name,numb,A,B,links,Pincode) %>% unique %>%
  mutate(Address = B, City = "India") %>% filter(row_number()<=2657)

latlon <- tbl_df(findlocation(uniquedataset$Address,
                              uniquedataset$A,
                              uniquedataset$City,
                              uniquedataset$Pincode))
latlon.it1 <- latlon
uniquedataset = dataset %>% tbl_df %>%
  select(name,numb,A,B,links,Pincode) %>% unique %>%
  mutate(Address = B, City = "India") %>% filter(row_number()>2657 &row_number()<=2680)
latlon.it2 <- tbl_df(findlocation(uniquedataset$Address,
                                  uniquedataset$A,
                                  uniquedataset$City,
                                  uniquedataset$Pincode))
latlon.it2<-rbind(latlon.it1,latlon.it2)
table(latlon.it2$Status)

#saveRDS(latlon.it2, "latlon.it2.rds")
latlon.it2 <- readRDS("latlon.it2.rds")
uniquedataset = dataset %>% tbl_df %>%
  select(name,numb,A,B,links,Pincode) %>% unique %>%
  mutate(Address = B, City = "India") %>% filter(row_number()>2680 & row_number()<=5480)
latlon.it3 <- tbl_df(findlocation(uniquedataset$Address,
                                  uniquedataset$A,
                                  uniquedataset$City,
                                  uniquedataset$Pincode))
latlon.it3<-rbind(latlon.it2,latlon.it3)
table(latlon.it3$Status)

latlon.it3<- readRDS("latlon.it3.rds")
uniquedataset = dataset %>% tbl_df %>%
  select(name,numb,A,B,links,Pincode) %>% unique %>%
  mutate(Address = B, City = "India") %>% filter(row_number()>5480 & row_number()<=5800)
latlon.it4 <- tbl_df(findlocation(uniquedataset$Address,
                                  uniquedataset$A,
                                  uniquedataset$City,
                                  uniquedataset$Pincode))
View(latlon.it4)
latlon.it4<-rbind(latlon.it3,latlon.it4)

uniquedataset = dataset %>% tbl_df %>%
  select(name,numb,A,B,links,Pincode) %>% unique %>%
  mutate(Address = B, City = "India") %>% filter(row_number()>5800 & row_number()<=7800)
latlon.it5 <- tbl_df(findlocation(uniquedataset$Address,
                                  uniquedataset$A,
                                  uniquedataset$City,
                                  uniquedataset$Pincode))
latlon.it5<-rbind(latlon.it4,latlon.it5)

uniquedataset = dataset %>% tbl_df %>%
  select(name,numb,A,B,links,Pincode) %>% unique %>%
  mutate(Address = B, City = "India") %>% filter(row_number()>7800 & row_number()<=9000)
latlon.it6 <- tbl_df(findlocation(uniquedataset$Address,
                                  uniquedataset$A,
                                  uniquedataset$City,
                                  uniquedataset$Pincode))
latlon.it6<-rbind(latlon.it5,latlon.it6)
saveRDS(latlon.it6, "latlon.it6.rds")


uniquedataset = dataset %>% tbl_df %>%
  select(name,numb,A,B,links,Pincode) %>% unique %>%
  mutate(Address = B, City = "India") %>% filter(row_number()>9000 & row_number()<=11700)
latlon.it7 <- tbl_df(findlocation(uniquedataset$Address,
                                  uniquedataset$A,
                                  uniquedataset$City,
                                  uniquedataset$Pincode))
latlon.it7<-rbind(latlon.it6,latlon.it7)
#saveRDS(latlon.it7, "latlon.it7.rds")
latlon.it7 <- readRDS("latlon.it7.rds")

uniquedataset = dataset %>% tbl_df %>%
  select(name,numb,A,B,links,Pincode) %>% unique %>%
  mutate(Address = B, City = "India") %>% filter(row_number()>11700 & row_number()<=14200)
latlon.it8 <- tbl_df(findlocation(uniquedataset$Address,
                                  uniquedataset$A,
                                  uniquedataset$City,
                                  uniquedataset$Pincode))

View(latlon)
latlon %>% write.csv("check.csv")


