# Checa as localizações no mapa

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-04-04


################# Check ########################

library(tidyverse)
library(data.table)
library(readxl)
library(stringi)

MunicGeoPoints <- fread("data/dataset/MunicGeoPoints.csv", 
                        sep = ";", dec = ",")

UFs <- fread("data/dataset/UFs.csv", sep = ";", dec = ",")

names(MunicGeoPoints)
names(UFs)


for(i in 42:50) {
  
  # i <- 7
  
  # 
  # MunicGeoPoints <- fread("data/raw/Murilo Junqueira/2020-04-05 Revisão Lugares/MergedData.csv", 
  #                         sep = ";", dec = ",")
  
  # MunicGeoPoints <- read_xlsx("data/raw/Murilo Junqueira/MunicGeoPoints-Revisão.xlsx", 
  #                             sheet = "MunicGeoPoints-Revisão")
  #
  
  # MunicGeoPoints <- read_xlsx("data/raw/Lucas Almeida/MunicGeoPoints-Revisado-Lucas.xlsx",
  #                             sheet = "Planilha1")
  
  MunicGeoPoints <- read_xlsx("data/raw/Murilo Junqueira/2020-04-05 Revisão Lugares/Check-MergedData.xlsx",
                              sheet = "Planilha1")

  
  MunicGeoPoints <- MunicGeoPoints %>% left_join(UFs, by = "UF_Id") 
  
  MunicGeoPoints$Munic_Nome2 <- iconv(MunicGeoPoints$Munic_Nome, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  
  NSView <- paste0("https://www.google.com.br/maps/dir/",
                   paste0(MunicGeoPoints$ExtremNorth_Lat[i], ",", MunicGeoPoints$ExtremNorth_Lon[i]),
                   "/",
                   paste0(MunicGeoPoints$Center_Lat[i], ", ", MunicGeoPoints$Center_Lon[i]),
                   "/",
                   paste0(MunicGeoPoints$ExtremSouth_Lat[i], ",", MunicGeoPoints$ExtremSouth_Lon[i]))
  
  
  EWView <- paste0("https://www.google.com.br/maps/dir/",
                   paste0(MunicGeoPoints$ExtremEast_Lat[i], ", ", MunicGeoPoints$ExtremEast_Lon[i]),
                   "/",
                   paste0(MunicGeoPoints$Center_Lat[i], ", ", MunicGeoPoints$Center_Lon[i]),
                   "/",
                   paste0(MunicGeoPoints$ExtremWest_Lat[i], ", ", MunicGeoPoints$ExtremWest_Lon[i]))
  
  
  browseURL(paste0("https://www.google.com.br/maps/place/", MunicGeoPoints$Munic_Nome2[i], ", ", MunicGeoPoints$UF_Sigla[i]))
  Sys.sleep(sample(1:3, 1))
  browseURL(NSView)
  Sys.sleep(sample(1:3, 1))
  browseURL(EWView)
  Sys.sleep(sample(1:3, 1))
}


# writeClipboard(paste0(MunicGeoPoints$ExtremWest_Lat[i], ", ", MunicGeoPoints$ExtremWest_Lon[i]))
# writeClipboard(paste0(MunicGeoPoints$ExtremEast_Lat[i], ", ", MunicGeoPoints$ExtremEast_Lon[i]))
# writeClipboard(paste0(MunicGeoPoints$ExtremNorth_Lat[i], ", ", MunicGeoPoints$ExtremNorth_Lon[i]))
# writeClipboard(paste0(MunicGeoPoints$ExtremSouth_Lat[i], ", ", MunicGeoPoints$ExtremSouth_Lon[i]))


# Fim