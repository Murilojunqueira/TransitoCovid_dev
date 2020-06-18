# Projeto Monitoramento da quarentena

# Script Para Chamar o robô que busca os dados de trânsito do Google

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-05-13


################# To do ########################


################# Dados Google ########################

library(tidyverse)
library(data.table)
library(lubridate)
library(googleCloudStorageR)

setwd(rstudioapi::getActiveProject())

apiKey <-  read_lines("data/keys/token_APIGDirections.txt")

# Busca Dados do Google
DoneFlag <- FALSE
while(!DoneFlag) {
  print(now())
  try(source("src/specifcFuntions/TrafficIndicator.R"))
  print(now())
}

# Fim