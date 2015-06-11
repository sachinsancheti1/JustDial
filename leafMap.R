
# finalized.dataset <- cbind(uniquedataset,latlon)
# finalized.dataset$Latitude = as.numeric(as.character(finalized.dataset$Latitude))
# finalized.dataset$Longitude = as.numeric(as.character(finalized.dataset$Longitude))
# #finalized.dataset %>% write.csv("check2.csv")
# 
# finalized.dataset$Town %>% head
# 
# #distfromGG("Coimbatore Airport")
# finalized.dist = distfromGG(finalized.dataset$Town)
# complete.dataset <- cbind(finalized.dataset,finalized.dist)
# names(complete.dataset)
# write.csv(complete.dataset,"check3.csv")
# 
# complete.dataset %>%
#   select(km) %>%
#   as.numeric %>%
#   na.omit %>%
#   ggvis(props(x=~km)) 

# GGMAPS + GGPLOT2 package usage for drawing
bologna <- "Coimbatore"
bologna.map <- get_map(bologna, zoom = 10,
                       source = "google", 
                       maptype = "roadmap")
nationalmap <- ggmap(bologna.map,extent = "device")
ggmap(bologna.map)
#nationalmap
#nationalmap + geom_point(size = I(1), alpha = 1/3,col = "red")
nationalmap + geom_point(size = I(3), alpha = 1/3,col = "red")
qplot(x = Longitude,
      y = Latitude,
      data = finalized.dataset,
      colour = PinCode,
      geom = c("point","density2d"))


temp=as.data.frame(cbind(as.numeric(as.character(latlon.it7$Longitude)),
                     as.numeric(as.character(latlon.it7$Latitude))))
nationalmap + stat_density2d(
  aes(
    x = V1,
    y = V2,
    fill = ..level..,
    alpha = ..level..),
  size = 10, 
  bins = 20,
  data = temp, 
  geom = "polygon", 
  show_guide = FALSE)

clean.dataset <- complete.dataset[complete.cases(complete.dataset[,c("Latitude","Longitude","km")]),]


# LEAFLET Package usage
m = leaflet(clean.dataset) %>% addGeoJSON(shapes)
new.map = m %>% setView(76.9842621,11.2094802, zoom = 10) %>%
  addTiles(
    'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
    attribution = 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community') %>%
  addMarkers(76.9842621,11.2094802, icon = JS("L.icon({
                                              iconUrl: 'http://imageshack.com/a/img537/7941/C1TLjw.png',
                                              iconSize: [68, 40]
                                              })")) %>%
  addPopups(76.9842621,11.2094802, '<b>Gokul Gardens</b>, Mathampalayam') %>%
  addCircles(lat = ~Latitude,
             lng = ~Longitude,
             popup = ~km,
             opacity = 0.7,
             radius = 30)


shapes = list(
  list(
    type = 'Feature',
    properties = list(
      style = list(color = 'red', fillColor = 'yellow'),
      popup = 'Here is a polygon, or perhaps a flower...'
    ),
    geometry = list(
      type = 'Polygon',
      coordinates = list({
        x = 76.9842621 # center x
        y = 11.2094802 # center y
        n = 10000 # nr of pts
        r = .1 # radius
        pts = seq(0, 2 * pi, length.out = n)
        cbind(r*sin(pts)+x,r*cos(pts)+y)
      })
    )
  )
)

new.map 



leaflet(finalized.dataset) %>% setView(76.9842621,11.2094802, zoom = 10)  %>% addTiles(
  'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
  attribution = 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community') %>%
  addMarkers(76.9842621,11.2094802, icon = JS("L.icon({
                                              iconUrl: 'http://imageshack.com/a/img537/7941/C1TLjw.png',
                                              iconSize: [68, 40]
                                              })")) %>%
  addPopups(76.9842621,11.2094802, '<b>Gokul Gardens</b>, Mathampalayam') %>% addGeoJSON(shapes) %>%
  addCircles(lat = ~Latitude,
             lng = ~Longitude,
             opacity = 0.7,
             radius = 12,
             color = ~PinCode,
             popup = ~name) 
#######################################################
# dataset = tbl_df(read.xls("Existing Clients.xlsx",header = T,stringsAsFactors=FALSE))
# latlon <- tbl_df(dataset$Address %>% findlocation)
# table(latlon$Status)
# latlon %>%
#   filter(Status =="OK") %>%
#   write.csv("check.csv")
