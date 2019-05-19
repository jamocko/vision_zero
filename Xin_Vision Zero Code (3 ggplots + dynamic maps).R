library(ggplot2)
library(dplyr)
library(stringr)
library(knitr)
library(xml2)
library(nnet)
library(imager)
library(waffle)
library(scales)
library(forecast)
library(fpp2)
library(sjPlot)
library(sjmisc)
library(rstan)
library(finalfit)
library(plotrix)
library(sp)
library(rgdal)
library(maps)
library(randomForest)
library(leaflet)
library(leaflet.extras)

## Original Data files
data11 = read.csv("https://raw.githubusercontent.com/jamocko/vision_zero/master/data/crash_data/txdot_cris_crashes_hgac_201804_201806.csv")

data12 = read.csv("https://raw.githubusercontent.com/jamocko/vision_zero/master/data/crash_data/txdot_cris_crashes_hgac_201807_201808.csv")

data13 = read.csv("https://raw.githubusercontent.com/jamocko/vision_zero/master/data/crash_data/txdot_cris_crashes_hgac_201809.csv")

data14 = read.csv("https://raw.githubusercontent.com/jamocko/vision_zero/master/data/crash_data/txdot_cris_crashes_hgac_201810_201901.csv")


## Select specific colummns
## Reduce file size
var_list = c("RoadClass","RoadBaseType","CrashTime","Longitude","Latitude",
             "WeatherCondition","SpeedLimit","LightCondition","n_cars","CrashSeverity")
data11 = subset(data11, select = var_list)
data12 = subset(data12, select = var_list)
data13 = subset(data13, select = var_list)
data14 = subset(data14, select = var_list)

## Generalize category names
## Different data files have slightly different categorical names

data14[data14$CrashSeverity == "99 - UNKNOWN",]$CrashSeverity = "Unknow"
data14[data14$CrashSeverity == "A - SUSPECTED SERIOUS INJURY",]$CrashSeverity = "Suspected Serious Injury"
data14[data14$CrashSeverity == "B - NON-INCAPACITATING INJURY",]$CrashSeverity = "Non-Incapacitating Injury"
data14[data14$CrashSeverity == "N - NOT INJURED",]$CrashSeverity = "Not Injured"
data14[data14$CrashSeverity == "C - POSSIBLE INJURY",]$CrashSeverity = "Possible Injury"
data14[data14$CrashSeverity == "K - KILLED",]$CrashSeverity = "Killed"

## Merge data into a single file
data_plot = rbind(data11, data12, data13,data14)

## Basic Exploration
summary(data_plot$CrashSeverity)


## Waffle plot for category of injuries
parts = c(`Incapacitating Injury` = 585,
          `Non-Incapitating Injury` = 5261,
          `Killed` = 206 + 186,
          `Not Injured` = 48523 + 29833,
          `Possible Injury` = 7521 + 12287,
          `Unknow` = 2263 + 1601,
          `Suspected Serious Injury` = 571 + 695)

# Turn into percentage
parts = 100*parts/112613
waffle(parts, rows = 7, glyph_size = 5, title = "Accident Severity Distribution")+
  theme(plot.title = element_text(size=14))


# Second waffle plot of stree category
parts <- c(`City Street` = 37.8, `US & State Highways`=20, `Interstate`= 17.7,
           `Couty Road` = 13.2 , `Farm to Market` = 9.5,`Tollway`=2)

waffle(parts, rows = 7, glyph_size = 5, title = "Accident Severity Distribution")+
  theme(plot.title = element_text(size=14))

# Crash vs time od a day
# Gaussian smoothed

data_plot$CrashTime = as.POSIXct(data_plot$CrashTime,origin = "2019-10-01")

p = ggplot(data_plot, aes(x=CrashTime)) +
  geom_density(kernel = "gaussian") +
  xlab("Time") +
  ylab("Distribution of Accidents") +
  theme(axis.text.x = element_text(angle=45, hjust=1,size = 9),
        strip.text = element_text(size=13)) +
  labs(title = "Time vs Number of Accidents by Time of Day")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

p

# Dynamic leaflet maps

data2 = read.csv("https://raw.githubusercontent.com/jamocko/vision_zero/master/data/crash_data/sampleCompiledDataset.csv")

## subset columns and reduce file size
data2 = subset(data2, select = c("CrashSeverity","Latitude","Longitude",
                                 "CrashTotalInjuryCount","n_cars","n_peds",
                                 "n_bike","IntersectionRelated"))

## serious injuries
level1 = subset(data2, CrashSeverity %in% c("A - SUSPECTED SERIOUS INJURY",
                                            "B - NON-INCAPACITATING INJURY ",
                                            "POSSIBLE INJURY","Incapacitating Injury",
                                            "K - KILLED","Non-Incapacitating Injury",
                                            "Suspected Serious Injury") & IntersectionRelated %in% c("Intersection", "Intersection Related"))

## not serious injuries
level2 = subset(data2, CrashSeverity %in% c("Not Injured",
                                            "Possible Injury",
                                            "POSSIBLE INJURY")& IntersectionRelated %in% c("Intersection", "Intersection Related"))

## Identify coordinates system
coordinates(level1) <- c("Longitude", "Latitude")
coordinates(level2) <- c("Longitude", "Latitude")

## Align coordinates with super neighborhood geo-jason
## Houston super neighborhoods shapefile
super_nei <- geojsonio::geojson_read("Super_Neighborhoods.shp", what = "sp")

proj4string(level1) <- proj4string(super_nei)
proj4string(level2) <- proj4string(super_nei)

## Make dynamic maps
## color bins
bins <- c(0, 0.1,0.5,1, 2, 3, 4, 5, Inf)
pal <- colorBin("YlOrRd", domain = level1, bins = bins)

data3 = read.csv("https://raw.githubusercontent.com/jamocko/vision_zero/master/data/crash_data/limited_clean_txdot_dataset.csv")

## Basic Exploration
dim(data3)


labels1 <- sprintf(
  "Total Injury: %s
  <br/> Num of Cars: %s
  <br/> Num of Bikes: %s
  <br/> Num of Pedestrians: %s",
  level1$CrashTotalInjuryCount,
  level1$n_cars,
  level1$n_bike,
  level1$n_peds
) %>% lapply(htmltools::HTML)

labels2 <- sprintf(
  "Total Injury: %s
  <br/> Num of Cars: %s
  <br/> Num of Bikes: %s
  <br/> Num of Pedestrians: %s",
  level1$CrashTotalInjuryCount,
  level1$n_cars,
  level1$n_bike,
  level1$n_peds
) %>% lapply(htmltools::HTML)


map <- leaflet(level2) %>%
  setView(-95.3831, 29.7627, 9) %>%
  
  ## Base Maps
  addProviderTiles(providers$CartoDB.Positron)%>%
  
  ## Heat maps layer
  addHeatmap(blur = 15, max = 3, radius = 4,gradient = "YlOrRd",group = "General")%>%
  
  ## Add circiles for serious injuries
  addCircleMarkers(data = level1,
                   weight = 4,radius = 4, color = ~pal(CrashTotalInjuryCount),
                   label = labels1,stroke = FALSE,
                   labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                            padding = "5px 8px"), 
                                               textsize = "13px", direction = "auto"),
                   group = "Serious")%>%
  
  ## Add circiles for non-serious injuries
  addCircleMarkers(data = level2,
                   weight = 4,radius = 4, color = ~pal(CrashTotalInjuryCount),
                   label = labels2,stroke = FALSE,
                   labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                            padding = "5px 8px"), 
                                               textsize = "13px", direction = "auto",
                                               transparent = TRUE, fillOpacity = 0.1),
                   group = "Not Serious") %>%
  
  ## Layer control
  addLayersControl(
    overlayGroups = c("Serious", "Not Serious","General"),
    options = layersControlOptions(collapsed = FALSE))

## Output the final map
map
