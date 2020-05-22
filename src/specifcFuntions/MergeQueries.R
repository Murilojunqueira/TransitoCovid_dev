# Projeto Monitoramento da quarentena

# Consolida as buscas feitas no Google

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-04-08


################# Setup ########################

library(tidyverse)
library(data.table)
library(lubridate)
library(zip)

################# Lê todos os dados ########################

# Lista dos Arquivos
File_List <- list.files("data/QueryData", pattern = "csv$")

# Transforma Lista de Arquivos em base de dados
File_List_df <- File_List %>%
  str_replace("-", "----0000") %>% 
  tibble::enframe(name = NULL, value = "File_List") %>% 
  separate(File_List, into = c("df_Name", "Data_extension"), sep = "----0000")

# table(File_List_df$df_Name)

# Lê todos os dados

FullList <- list() # Lista para agregar todos os dados por tipo

for(i in seq_len(nrow(File_List_df))) {
  # i <- 1
  message("Adding ", File_List[i])
  NewData_path <- paste0("data/QueryData/", File_List[i]) 
  # file.exists(NewData_path)
  NewData <- fread(NewData_path, sep = ";", dec = ",")
  # Vou colocar todos os dados em uma lista e depois separar
  FullList[[File_List_df$df_Name[i]]] <- bind_rows(FullList[[File_List_df$df_Name[i]]], NewData)
  rm(NewData_path, NewData)
}
rm(i)

################# Agrega todos os dados ########################

# names(FullList)

# Função para a correção de enconding
CorrectEnconding <- function(table_df, ToEnconding = "UTF-8") {
  
  table_Char <- names(table_df)[map_chr(table_df, class) == "character"]
  
  for (i in seq_len(length(table_Char))) {
    # i <- 1
    CurrentEncoding <- guess_encoding(table_df[[table_Char[i]]])[[1,1]]
    table_df[[table_Char[i]]] <- iconv(table_df[[table_Char[i]]], 
                                       from = CurrentEncoding, 
                                       to = ToEnconding)
  }
  return(table_df)
}

QueriesIndex <- FullList$QueriesIndex %>% bind_rows() %>% as_tibble() %>% CorrectEnconding()
geocoded_waypoints <- FullList$geocoded_waypoints %>% bind_rows() %>% as_tibble() %>% CorrectEnconding()
Routes <- FullList$Routes %>% bind_rows() %>% as_tibble() %>% CorrectEnconding()
Step <- FullList$Step %>% bind_rows() %>% as_tibble() %>% CorrectEnconding()

rm(FullList)

################# Salva uma versão Backup dos dados ########################

# marca a data do backup
nowFix <- now() %>% as.character() %>% 
  str_replace_all(":", "-") %>% 
  str_replace_all(" ", "_")

# Nomes dos arquivos CSV
QueriesIndex_csvName <- paste0("data/backups/queries/Backup-QueriesIndex - ", nowFix, ".csv")
geocoded_waypoints_csvName <- paste0("data/backups/queries/Backup-geocoded_waypoints - ", nowFix, ".csv")
Routes_csvName <- paste0("data/backups/queries/Backup-Routes - ", nowFix, ".csv")
Step_csvName <- paste0("data/backups/queries/Backup-Step - ", nowFix, ".csv")

# Nomes dos arquivos zip
# QueriesIndex_zipName <- paste0("data/backups/queries/Backup-QueriesIndex - ", nowFix, ".zip")
# geocoded_waypoints_zipName <- paste0("data/backups/queries/Backup-geocoded_waypoints - ", nowFix, ".zip")
# Routes_zipName <- paste0("data/backups/queries/Backup-Routes - ", nowFix, ".zip")
# Step_zipName <- paste0("data/backups/queries/Backup-Step - ", nowFix, ".zip")

# Grava Arquivos CSV
fwrite(QueriesIndex, QueriesIndex_csvName, sep = ";", dec = ",")
fwrite(geocoded_waypoints, geocoded_waypoints_csvName, sep = ";", dec = ",")
fwrite(Routes, Routes_csvName, sep = ";", dec = ",")
fwrite(Step, Step_csvName, sep = ";", dec = ",")

# Não sei porque não consigo usar o zipr no drive D, então jogo os arquivos para o drive C
# zip::zipr(QueriesIndex_zipName, QueriesIndex_csvName) # Not working

# Salva arquivos zipados
# Dir_Temp <- tempdir()
# 
# # Função para salvar temporariamente em um arquivo temporário e depois zipar
# ZipTempCsv <- function(zipfile, file, Dir_Temp) {
#   Temp_csv <- paste0(Dir_Temp, "\\temp.csv")
#   file.copy(file, Temp_csv, overwrite = TRUE)
#   zip::zipr(zipfile, Temp_csv, include_directories = FALSE)
#   unlink(Temp_csv)
# }

# ZipTempCsv(QueriesIndex_zipName, QueriesIndex_csvName, Dir_Temp)
# ZipTempCsv(geocoded_waypoints_zipName, geocoded_waypoints_csvName, Dir_Temp)
# ZipTempCsv(Routes_zipName, Routes_csvName, Dir_Temp)
# ZipTempCsv(Step_zipName, Step_csvName, Dir_Temp)

# Deleta arquivos CSV
# unlink(c(QueriesIndex_csvName, geocoded_waypoints_csvName, Routes_csvName, Step_csvName))

# Limpa a memória
rm(QueriesIndex_csvName, geocoded_waypoints_csvName, Routes_csvName, Step_csvName)
# rm(QueriesIndex_zipName, geocoded_waypoints_zipName, Routes_zipName, Step_zipName)
# rm(Dir_Temp, ZipTempCsv)
rm(nowFix)

################# Lê os dados anteriores do banco ########################

tables <- c("QueriesIndex", "geocoded_waypoints", "Routes", "Step") 

for (i in seq_len(length(tables))) {
  # i <- 1
  if(file.exists(paste0("data/dataset/", tables[i], ".csv"))){
    assign(paste0(tables[i], "_Old"), fread(paste0("data/dataset/", tables[i], ".csv"), 
                                            sep = ";", dec = ",", encoding = "UTF-8"))
  }
}
rm(i)

# ls()

################# Elimina Dados Repetidos ########################

# QueriesIndex 
# geocoded_waypoints 
# Routes 
# Step 

# Tabela com as chaves primárias do banco de dados
tables <- c("QueriesIndex", "geocoded_waypoints", "Routes", "Step") 

pk_vars <- list("QueryKey", 
                c("QueryKey", "place_id"), 
                c("QueryKey", "route"), 
                c("QueryKey", "route", "start_location_lat", "start_location_lng"))

Codebook <- tibble()

for (j in seq_len(length(tables))) {
  # j <- 2
  New_table <- rep(tables[j], length(pk_vars[[j]])) %>% 
    enframe(name = NULL, value = "table") %>% 
    bind_cols(enframe(pk_vars[[j]], name = NULL, value = "pk_vars"))
  
  Codebook <- bind_rows(Codebook, New_table)
}
rm(j, pk_vars, New_table)

Codebook <- Codebook %>% 
  mutate(Table_Old = paste0(table, "_Old"))

# Codebook

# Função Para evitar repetições em um banco de dados de acordo com 
# as algumas variáveis de chave primária
distinct_key <- function(x, Keys, .keep_all = TRUE) {
  Output <- x %>% 
    unite(UniteKey, one_of(Keys), sep = "___") %>% 
    distinct(UniteKey, .keep_all = .keep_all) %>% 
    separate(UniteKey, Keys, sep = "___") %>% 
    select(one_of(Keys), everything())
  return(Output)
}

# ls()

# Cria backups dos dados, just in case...
# QueriesIndex_bck <- QueriesIndex
# geocoded_waypoints_bck <- geocoded_waypoints
# Routes_bck <- Routes
# Step_bck <- Step

# QueriesIndex <- QueriesIndex_bck
# geocoded_waypoints <- geocoded_waypoints_bck
# Routes <- Routes_bck
# Step <- Step_bck

# Remove duplicata dos bancos
if(exists("QueriesIndex_Old")) {
  QueriesIndex <- bind_rows(QueriesIndex, QueriesIndex_Old) %>% 
    distinct_key(Codebook$pk_vars[Codebook == "QueriesIndex"])
  rm(QueriesIndex_Old)
}

if(exists("geocoded_waypoints_Old")) {
  geocoded_waypoints <- bind_rows(geocoded_waypoints, geocoded_waypoints_Old) %>% 
    distinct_key(Codebook$pk_vars[Codebook == "geocoded_waypoints"])
  rm(geocoded_waypoints_Old)
}

if(exists("Routes_Old")) {
  Routes <- bind_rows(Routes, Routes_Old) %>% 
    distinct_key(Codebook$pk_vars[Codebook == "Routes"])
  rm(Routes_Old)
}

if(exists("Step_Old")) {
  
  Step_Old <- Step_Old %>% 
    mutate(start_location_lat = as.character(start_location_lng)) %>% 
    mutate(start_location_lng = as.character(start_location_lat))
  
  Step <- Step %>%
    mutate(start_location_lat = as.character(start_location_lng)) %>% 
    mutate(start_location_lng = as.character(start_location_lat)) %>% 
    bind_rows(Step_Old) %>% 
    distinct_key(Codebook$pk_vars[Codebook == "Step"]) %>%
    mutate(start_location_lat = as.numeric(start_location_lng)) %>% 
    mutate(start_location_lng = as.numeric(start_location_lat))
  rm(Step_Old)
}



################# Salva dados Atualizados ########################

# Salva Arquivos

fwrite(QueriesIndex, paste0("data/dataset/QueriesIndex.csv"), sep = ";", dec = ",")
fwrite(geocoded_waypoints, paste0("data/dataset/geocoded_waypoints.csv"), sep = ";", dec = ",")
fwrite(Routes, paste0("data/dataset/Routes.csv"), sep = ";", dec = ",")
fwrite(Step, paste0("data/dataset/Step.csv"), sep = ";", dec = ",")

################# Limpa Arquivos temporários ########################


# file.exists(paste0("data/QueryData/", File_List))

unlink(paste0("data/QueryData/", File_List))

# Limpa Memória
rm(QueriesIndex, geocoded_waypoints, Routes, Step)
rm(File_List, File_List_df, tables)
rm(Codebook, distinct_key, CorrectEnconding)


# End