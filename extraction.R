library(RCurl)
library(gdata)
library(reshape2)

# Function to parse the url from the html


field = "Teaching-staff"
link = paste("http://www.justdial.com/Hyderabad/",field,sep = "")
ps = parseIt(link)

sink("ps.html")
ps
sink()

json_data <- fromJSON(file="places.json")
places = as.data.frame(json_data)

#tt = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlValue)

tc = xpathApply(ps,'//section/section/section/section/section/section/aside/p/span',fun = xmlAttrs)
tt = xpathApply(ps,'//section/section/section/section/section/section/aside/p/span',fun = xmlValue)

for(i in 1:length(tc)){
  if(tc[[i]][1]=='jcn dcomclass' | tc[[i]][1]=='jcn ')
  cat(tt[[i]][1])
}
 ps=parseIt("http://www.justdial.com/Hyderabad/Teaching-staff/page-2")


#type 2
tc = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlAttrs)
tt = xpathApply(ps,'//section/section/section/section/section/section/aside/p',fun = xmlValue)
dt = data.frame(Name=character(),Number= character(),Address=character())
for(i in 1:(length(tc)-3)){
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
dt <- dt %>% separate(addr,into=c("A","B"),sep="More...")
