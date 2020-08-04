library(forcats)
library(ggplot2)
library(dplyr)
library(treemap)
library(maps)
library(leaflet)

data <- read.csv('../input/unesco-world-heritage-sites/whc-sites-2019.csv')
head(data, n = 3L)

temp <- as.data.frame(table(data$states_name_en)) # Create a temporary variable to keep all country names
temp <- temp[order(-temp$Freq),] #Ordered Desc

#Plot the graph
#Then, Pipe Baby...
head(temp, n = 15L) %>% #I just want the top 15 countries in relation to the heritage Sites quantity
  mutate(Var1 = fct_reorder(Var1, Freq)) %>%
  ggplot( aes(x = Var1, y = Freq)) +
  geom_bar(stat="identity", fill="#6360f6", alpha=.7, width=.4) +
  coord_flip() + #Flip the graph
  xlab("Country") +
  ylab("Heritage Sites Quantity")+
  theme_bw()

summary(as.logical(data$danger)) #We have 53 WHS in Danger

row_to_keep <- as.data.frame(table(data$states_name[as.logical(data$danger)]))
row_to_keep <- row_to_keep %>% 
                filter(as.logical(Freq))
row_to_keep <- row_to_keep[order(-row_to_keep$Freq),]

#Plot the graph
head(row_to_keep, n = 10L) %>% 
  mutate(Var1 = fct_reorder(Var1, Freq)) %>%
  ggplot( aes(x = Var1, y = Freq)) +
  geom_bar(stat="identity", fill="#6360f6", alpha=.7, width=.4) +
  coord_flip() +
  xlab("Country") +
  ylab("Quantity of Heritage Sites in Danger ")+
  theme_bw()
  
#Create a dataframe with the Country and the Quantity of Natural HS
row_to_keep <- as.data.frame(table(data$states_name[data$category == "Natural"]))
row_to_keep <- row_to_keep %>% 
                filter(as.logical(Freq))
row_to_keep <- row_to_keep[order(-row_to_keep$Freq),]

head(row_to_keep, n = 10L) %>%
  mutate(Var1 = fct_reorder(Var1, Freq)) %>%
  ggplot( aes(x = Var1, y = Freq)) +
  geom_bar(stat="identity", fill="#6360f6", alpha=.7, width=.4) +
  coord_flip() +
  xlab("Country") +
  ylab("Natural Heritage Sites Quantity")+
  theme_bw()
  
row_to_keep <- as.data.frame(table(data$states_name[data$category == "Cultural"]))
row_to_keep <- row_to_keep %>% 
                filter(as.logical(Freq))
row_to_keep <- row_to_keep[order(-row_to_keep$Freq),]

head(row_to_keep, n = 10L) %>%
  mutate(Var1 = fct_reorder(Var1, Freq)) %>%
  ggplot( aes(x = Var1, y = Freq)) +
  geom_bar(stat="identity", fill="#6360f6", alpha=.7, width=.4) +
  coord_flip() +
  xlab("Country") +
  ylab("Cultural Heritage Sites Quantity")+
  theme_bw()
  
row_to_keep <- as.data.frame(table(data$states_name[data$category == "Mixed"]))
row_to_keep <- row_to_keep %>% 
                filter(as.logical(Freq))
row_to_keep <- row_to_keep[order(-row_to_keep$Freq),]

head(row_to_keep, n = 10L) %>%
  mutate(Var1 = fct_reorder(Var1, Freq)) %>%
  ggplot( aes(x = Var1, y = Freq)) +
  geom_bar(stat="identity", fill="#6360f6", alpha=.7, width=.4) +
  coord_flip() +
  xlab("Country") +
  ylab("Mixed Heritage Sites Quantity")+
  theme_bw()
  
row_to_keep <- data.frame(Country = data$states_name_en[data$area_hectares >= 5000000], hec = data$area_hectares[data$area_hectares >= 5000000])
row_to_keep <- row_to_keep[order(-row_to_keep$hec),]
row_to_keep <- row_to_keep[!is.na(row_to_keep$Country),] #Delete rows == NA from the dataframe

# To enlarge the map:  save a png locally with a new width + height
# Not sure why the scale of the map is not the same as R Studio
treemap(row_to_keep,
        index="Country",
        vSize="hec",
        type="index",
        palette = rainbow(7),
        fontsize.labels = 13,
        title="World Heritage Sites by Area"
)

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld 

mp <- mp+ geom_point(aes(x=data$longitude, y=data$latitude), color="blue", size=2) #Plot the dots
mp

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot(data, aes(x=longitude, y=latitude)) +   mapWorld

mp <- mp+ geom_point(aes(colour = factor(region_en)), size=2) 
mp <- mp + theme(plot.background = element_rect(fill = "transparent", color = NA),
             panel.border = element_blank(),
             panel.background = element_rect(fill = "lightblue", color = NA),
             panel.grid = element_blank(),
             axis.text = element_blank(),
             axis.ticks = element_blank(),
             legend.position = "bottom")
mp

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot(data, aes(x=longitude, y=latitude)) +   mapWorld

mp <- mp+ geom_point(aes(colour = factor(category)), size=2) 
mp <- mp + theme(plot.background = element_rect(fill = "transparent", color = NA),
                 panel.border = element_blank(),
                 panel.background = element_rect(fill = "lightblue", color = NA),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = "bottom") 
mp

danger_filter <- data %>% 
  filter(as.logical(data$danger))
danger_filter <- danger_filter[c("name_en", "longitude", "latitude")]
danger_filter <- rename(danger_filter, lat=latitude, lng=longitude)

#Same here, if the map are not appear, i don't know why
m <- danger_filter %>%
    leaflet() %>%
    addTiles() %>% 
    addMarkers(clusterOptions = markerClusterOptions(), popup=danger_filter$name_en)
m  # Print the map
