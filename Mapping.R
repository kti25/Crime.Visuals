df<-read.table("C:/Users/Kevin Rossi/Desktop/Important Docs/Old Documents/Crime Project/NEW_seattle.csv", header = TRUE, sep = ",")
names(df)
#take samples of the seattle2 data for the heat map of all of the years
library(sampling)
library(ggmap)
library(dplyr)

df <- df[sample(1:nrow(df), 10000, replace=FALSE),]
dim(df)

############################################################################################
#
#uses the lattitude and longitude to create a heat map
#
############################################################################################
Both<-qmplot(Longitude, Latitude, data = df, color = factor(Crime_Type)) + guides(colour = guide_legend(override = list(alpha=1.0, size=6.0),
                                  title="Type of Crime")) +
     ggtitle("Seattle Crimes") +
     theme_light(base_size=5)
Both

Nonvi<-qmplot(Longitude, Latitude, data = df %>% filter(Crime_Type == c("Narcotics", "Theft", "Criminal Damage")), color = factor(Crime_Type)) + guides(colour = guide_legend(override = list(alpha=1.0, size=6.0),
                                  title="NON-VIOLENT CRIME")) +
     ggtitle("Seattle Crimes") +
     theme_light(base_size=10) 
Nonvi

Violent<-qmplot(Longitude, Latitude, data = df %>% filter(Crime_Type == c("Assault and Battery", "Homicide", "Robbery")), color = factor(Crime_Type)) + 
  guides(colour = guide_legend(override = list(alpha=1.0, size=6.0), title="VIOLENT CRIME")) +
  ggtitle("Seattle Crimes") +
  theme_light(base_size=10) 
Violent

violent_crime <- df %>% filter(Crime_Type == c("Assault and Battery", "Homicide", "Robbery"))
non_violent_crime <- df %>% filter(Crime_Type == c("Narcotics", "Theft", "Criminal Damage"))

heatv<-qmplot(Longitude, Latitude, data = violent_crime, geom = "blank", zoom = 12, maptype = "toner-lite") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Violent", low = "white", mid = "yellow", high = "red", midpoint = 10)
heatv

heatn<-qmplot(Longitude, Latitude, data = non_violent_crime, geom = "blank", zoom = 12, maptype = "toner-lite") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Non-Violent", low = "white", mid = "yellow", high = "red", midpoint = 10)
heatn
############################################################################################
#
#       USes the JSON vector file to create census
#
#############################################################################################
library(leaflet)
TopoData <- readLines("C:/Users/Kevin Rossi/Desktop/Important Docs/Old Documents/Crime Project/census.geojson") %>% paste(collapse = "\n")

CensusMapAll <- leaflet(df) %>% 
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite")%>%
addProviderTiles("CartoDB.Positron", group='Prettier Tiles') %>%  
setView(lng = -122.335167, lat = 47.608013, zoom = 9) %>%
addGeoJSON(TopoData, weight = 1, color = "#f00", fill = TRUE, group="Choropleth") %>%
addCircles(~Longitude, ~Latitude, ~Community.Area**2, stroke = F, group = "Crime") %>%
 addLayersControl(
    baseGroups = c("Toner Lite", "Prettier Tiles"),
    overlayGroups = c("Choropleth"),
    options = layersControlOptions(collapsed = FALSE)
  )
CensusMapAll

CensusMapVnt <- leaflet(violent_crime) %>% 
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite")%>%
  addProviderTiles("CartoDB.Positron", group='Prettier Tiles') %>%  
  setView(lng = -122.335167, lat = 47.608013, zoom = 9) %>%
  addGeoJSON(TopoData, weight = 1, color = "#f00", fill = TRUE, group="Choropleth") %>%
  addCircles(~Longitude, ~Latitude, ~Community.Area**2, stroke = F, group = "Crime") %>%
  addLayersControl(
    baseGroups = c("Toner Lite", "Prettier Tiles"),
    overlayGroups = c("Choropleth"),
    options = layersControlOptions(collapsed = FALSE)
  )

CensusMapVnt

CensusMapNon <- leaflet(violent_crime) %>% 
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite")%>%
  addProviderTiles("CartoDB.Positron", group='Prettier Tiles') %>%  
  setView(lng = -122.335167, lat = 47.608013, zoom = 9) %>%
  addGeoJSON(TopoData, weight = 1, color = "#f00", fill = TRUE, group="Choropleth") %>%
  addCircles(~Longitude, ~Latitude, ~Community.Area**2, stroke = F, group = "Crime") %>%
  addLayersControl(
    baseGroups = c("Toner Lite", "Prettier Tiles"),
    overlayGroups = c("Choropleth"),
    options = layersControlOptions(collapsed = FALSE)
  )
CensusMapNon

#################################################################################################
#
#  Does not work on regular R, not quite sure what this does
#
#################################################################################################

tf = 'mymap.html'
htmlwidgets::saveWidget(m, file = tf, selfcontained = F)
IRdisplay::display_html(paste("<iframe height=500 width=800 src=' ", tf, " ' ","/>"))

TopoData <- readLines("C:/Users/Kevin Rossi/Desktop/Crime Project/census.geojson") %>% paste(collapse = "\n")

m <- leaflet(df) %>% 
addProviderTiles("CartoDB.Positron", group='Prettier Tiles') %>%  
setView(lng = -122.335167, lat = 47.608013, zoom = 9) %>%
  addCircles(~Longitude, ~Latitude, ~Ward**2, stroke = F, group = "Crime")


tf = 'my_leaflet_map.html'
htmlwidgets::saveWidget(m, file = tf, selfcontained = F)
IRdisplay::display_html(paste("<iframe height=500 width=800 src=' ", tf, " ' ","/>"))

select(df, Primary.Type) %>% distinct()

#############################################################
# Bar Plot
#############################################################
cluster<-read.table("C:/Users/Kevin Rossi/Desktop/Crime Project/cluster.csv", header = TRUE, sep = ",")
counts2011 <- table(cluster$cluster_2011)
y<-barplot(counts2011, main="Cluster Distribution 2011", col=c(heat.colors(8)),
        xlab="Cluster Number", ylab="Community Count", ylim=c(0, 40)) 

counts2015 <- table(cluster$cluster_2015)
x<-barplot(counts2015, main="Cluster Distribution 2015", col=c(heat.colors(8)),
      xlab="Cluster Number", ylab="Community Area Count", ylim=c(0, 30))
text(x, 0, round(counts2015, 1),cex=1,pos=3) 
text(y, 0, round(counts2011, 1),cex=1,pos=3) 
