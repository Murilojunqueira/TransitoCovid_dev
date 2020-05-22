# Script monitorar a eficácia da quarentena usando dados de congestionamentos do Googlpe Maps

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-04-02


################# To do ########################


################# Documentação ########################

# Documentação do API directions do Google
# https://developers.google.com/maps/documentation/directions/intro

# Página do meu Google Cloud Plataform
# https://console.cloud.google.com/apis/dashboard?folder=&organizationId=&project=apt-impact-243819

# Documentação do pacote mapsapi
# https://cran.rstudio.com/web/packages/mapsapi/vignettes/intro.html

# Documentação do pacote ggmap
# https://github.com/dkahle/ggmap

# Time stamp to date
# https://www.epochconverter.com/

# Última busca bem sucedida:
# https://maps.googleapis.com/maps/api/directions/json?origin=Rua+Sen.+Manoel+Barata,+1254+-+Reduto,+Bel%C3%A9m+-+PA,+Brasil&destination=Avenida+Perimetral,+298+-+Guam%C3%A1,+Bel%C3%A9m+-+PA,+Brasil&units=metric&departure_time=now&key=AIzaSyDYLR4pOKwLnppV9FQsvOdz0ErIxsqHrMc

# https://www.google.com.br/maps/dir/R.+Sen.+Manoel+Barata,+1254+-+Reduto,+Bel%C3%A9m+-+PA,+66053-320/Av.+Perimetral,+298+-+Guam%C3%A1,+Bel%C3%A9m+-+PA/@-1.4636548,-48.4880257,14.71z/data=!4m14!4m13!1m5!1m1!1s0x92a48e941f0bc1ef:0x6be3a159c08ef27a!2m2!1d-48.4937538!2d-1.4480169!1m5!1m1!1s0x92a48dbe4e0e2647:0xc82fe4d619088fa2!2m2!1d-48.4559289!2d-1.4722753!3e0?hl=pt-BR&authuser=0


################# Setup Working Space ########################

library(tidyverse)
library(data.table)
library(lubridate)

source("src/generalFunctions/GDirectionsCall.R")
source("src/generalFunctions/GDirectionsWay_DF.R")


# apiKey = readLines("D:/Users/Murilo/Dropbox/Política e Comunidade/2020 - Pesquisa Quarentena/KeySecret/token.txt", 
#                    warn = FALSE)


################# Search Travels ########################

MunicGeoPoints <- fread(normalizePath("data/dataset/MunicGeoPoints.csv"), 
                        sep = ";", dec = ",", encoding = "Latin-1")

GeoSel <- MunicGeoPoints %>% 
  dplyr::filter(ExtremWest_Adress != "") 
# %>% 
#   slice(1:5)


raw_results <- list()

for (i in seq_len(nrow(GeoSel))) {
  
  # i <- 1
  message("Loading city: ", GeoSel$Munic_Nome[i], " - ", i, "/", nrow(GeoSel))
  message("Route: EastWest")
  
  x <-  GDirectionsCall(from = paste0(GeoSel$ExtremEast_Lat[i], ",", GeoSel$ExtremEast_Lon[i]),
                        to = paste0(GeoSel$ExtremWest_Lat[i], ",", GeoSel$ExtremWest_Lon[i]),
                        apiKey = apiKey,
                        parameters = c("units", "departure_time", "waypoints"),
                        parametersValues = c("metric", "now", 
                                             paste0("via:",
                                                    GeoSel$Center_Lat[i], ",",
                                                    GeoSel$Center_Lon[i]))) %>% 
    c(NowTime = as.character(now()))
  
  Sys.sleep(sample(1:3, 1))
  
  message("Route: NorthSouth")
  
  y <- GDirectionsCall(from = paste0(GeoSel$ExtremNorth_Lat[i], ",", GeoSel$ExtremNorth_Lon[i]),
                       to = paste0(GeoSel$ExtremSouth_Lat[i], ",", GeoSel$ExtremSouth_Lon[i]),
                       apiKey = apiKey,
                       parameters = c("units", "departure_time", "waypoints"),
                       parametersValues = c("metric", "now", 
                                            paste0("via:",
                                                   GeoSel$Center_Lat[i], ",",
                                                   GeoSel$Center_Lon[i]))) %>% 
    c(NowTime = as.character(now()))
  
  Sys.sleep(sample(1:3, 1))
  
  raw_results[[i]] <- list(EastWest = x, 
                           NorthSouth = y)
  
  rm(x, y)
}
rm(i)


################# Tidying up Directions API data ########################

QueriesIndex <- tibble()
geocoded_waypoints <- tibble()
Routes <- tibble()
Steps <- tibble()


for (i in seq_len(length(raw_results))) {
  
  # i <- 1
  message("Tidying up data: ", GeoSel$Munic_Nome[i], " - ", i, "/", nrow(GeoSel))
  
  Out_Dir_EW <- GDirectionsWay_DF(raw_result = raw_results[[i]][["EastWest"]],
                                  CityCode = GeoSel$Munic_Id[[i]], 
                                  Cityname = GeoSel$Munic_Nome[[i]], 
                                  NowTime = raw_results[[i]][["EastWest"]][["NowTime"]],
                                  Directions = "EW")
  
  
  Out_Dir_NS <- GDirectionsWay_DF(raw_result = raw_results[[i]][["NorthSouth"]],
                                  CityCode = GeoSel$Munic_Id[[i]], 
                                  Cityname = GeoSel$Munic_Nome[[i]], 
                                  NowTime = raw_results[[i]][["NorthSouth"]][["NowTime"]],
                                  Directions = "NS")
  
  QueriesIndex_New <- Out_Dir_EW[[1]] %>% 
    mutate(directions = "EastWest") %>% 
    bind_rows(Out_Dir_NS[[1]]) %>% 
    mutate(directions = ifelse(is.na(directions), "NorthSouth", directions))
  
  QueriesIndex <- bind_rows(QueriesIndex, QueriesIndex_New)
  geocoded_waypoints <- bind_rows(geocoded_waypoints, Out_Dir_EW[[2]], Out_Dir_NS[[2]])
  Routes <- bind_rows(Routes, Out_Dir_EW[[3]], Out_Dir_NS[[3]])
  Steps <- bind_rows(Steps, Out_Dir_EW[[4]], Out_Dir_NS[[4]])
  
  rm(QueriesIndex_New, Out_Dir_EW, Out_Dir_NS)
}
rm(i)


TimeFlag <- now() %>% 
  as.character() %>% 
  str_replace_all(":", "-") %>% 
  str_replace_all(" ", "_")

# Write temp data

f <- function(input, output) write.csv(input, row.names = FALSE, file = output)

gcs_upload(QueriesIndex, 
           object_function = f,
           type = "text/csv",
           name = paste0("QueriesIndex", "-", TimeFlag, ".csv"))

gcs_upload(geocoded_waypoints, 
           object_function = f,
           type = "text/csv",
           name = paste0("geocoded_waypoints", "-", TimeFlag, ".csv"))

gcs_upload(Routes, 
           object_function = f,
           type = "text/csv",
           name = paste0("Routes", "-", TimeFlag, ".csv"))

gcs_upload(Steps, 
           object_function = f,
           type = "text/csv",
           name = paste0("Steps", "-", TimeFlag, ".csv"))


rm(f)
rm(TimeFlag, QueriesIndex, geocoded_waypoints, Routes, Steps)
rm(GeoSel, MunicGeoPoints, raw_results)
rm(GDirectionsWay_DF, GDirectionsCall)

DoneFlag <- TRUE

# Fim