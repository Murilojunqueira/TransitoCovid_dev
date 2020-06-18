# Script para a tabela resumo com as informações mínimas
# para a atualização do site.

# Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data da criação: 2020-06-15

################### Setup #####################

library(googleCloudStorageR) # verificar se está logado
library(tidyverse)
library(data.table)
library(lubridate)

################### Baixa dados atuais #####################

# # Lê da tabela resumo atual
# TabResumo_Old <- fread("data/dataset/TabResumo.csv", 
#                    sep = ";", dec = ",", encoding = "UTF-8")
# 
# # Valores de referência das cidades
# CityRef <- fread("data/dataset/CityRef.csv",
#                    sep = ";", dec = ",", encoding = "UTF-8")

# Baixa dados atuais

gcs_global_bucket("quarantine-monitor-bd-dev")

f <- function(x) {
  suppressMessages(
    suppressWarnings(
      httr::content(x, encoding = "UTF-8")
    )
  )
}

# Dados dos gráficos
TabResumo_Old <- suppressWarnings(
  suppressMessages(
    gcs_get_object("TabResumo.csv")))

# Lista de municípios
Municipios <- suppressWarnings(
  suppressMessages(
    gcs_get_object("Municipios.csv"))) 


TabResumo_Old <- TabResumo_Old %>% dplyr::select(-X1)
Municipios <- Municipios %>% dplyr::select(-X1)

rm(f)

################### Atualizar tabela resumo #####################

# Lista de arquivos do bucket
gcs_global_bucket("quarantine-monitor-cloud")

# names(BucketFiles_BD)

BucketFiles <- gcs_list_objects(detail = "more") 
# Cria mais variáveis descritivas dos arquivos
BucketFiles_BD <- BucketFiles %>% 
  # Retira arquivos que não são dados
  filter(contentType == "text/csv") %>% 
  # Variável com o nome simples (e não completo) do arquivo
  mutate(FileName = str_remove(name, ".*/")) %>%  
  # Separa a data de extração do nome do arquivo
  mutate(Temp = str_replace(FileName, "-", "----0000")) %>% 
  separate(Temp, into = c("df_Name", "Date_extension"), 
           sep = "----0000") %>% 
  mutate(Date_extension = str_remove(Date_extension, ".csv")) %>% 
  mutate(Date_extension = ymd_hms(Date_extension)) %>% 
  mutate(Day_df = lubridate::as_date(Date_extension)) %>% 
  mutate(Hour_df = lubridate::hour(Date_extension))

# Dias já processados
DoneDays <- unique(TabResumo_Old$Day_df) %>% ymd()

# Lista dos arquivos com dias não integrados
ToUpdate <- BucketFiles_BD %>% 
  # Filtra os dados das rotas
  filter(df_Name == "Routes") %>% 
  # Exclui variáveis do período de referência e de extrações erradas
  dplyr::filter((Hour_df %in% c(8, 9, 12, 17, 18, 19))) %>% 
  # Filtra dias incompletos
  group_by(Day_df) %>% 
  mutate(flag = ifelse(Hour_df == 19, 1, 0)) %>% 
  mutate(flag = max(flag, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(flag == 1) %>% 
  select(-flag) %>% 
  # Seleciona variáveis relevantes
  select(name, Day_df, Hour_df) %>% 
  # Compara dias da tabela resumo os dias dos arquivos do bucket
  mutate(ProcessedFile = Day_df %in% DoneDays) %>% 
  # Filtra dias já processados
  filter(!ProcessedFile) 

rm(BucketFiles, BucketFiles_BD, DoneDays)


# Insere dados novos

TabResumo_New <- tibble()

for (i in seq_len(nrow(ToUpdate))) {
  # i <- 3
  
  message("Downloading ", ToUpdate$name[[i]])
  
  # Sistema para tornar a busca mais robusta
  for (j in 1:5) {
    suppressMessages(
      parsed_download <- try(gcs_get_object(ToUpdate$name[[i]])))
    
    if(class(parsed_download)[1] == "try-error" || 
       isTRUE(nrow(parsed_download) == 0)) {
      
      message("Error in communication with Google Storage. Trying again...")
      Sys.sleep(3)
      
    } else {
      break
    }
  }
  # Mensagem de erro se não foi possível baixar o arquivo após 5 tentativas
  if(class(parsed_download)[1] == "try-error" || 
     isTRUE(nrow(parsed_download) == 0)) {
    
    message("Unable to get file ", objects$name[[i]])
    next
  }
  
  # Extrai os dados relevantes
  MonitorReport <- parsed_download %>% 
    # Seleção de dados
    select(QueryKey, RoteTrafficValue) %>% 
    # Coloca variáveis de herança
    mutate(Day_df = ToUpdate$Day_df[i]) %>% 
    mutate(Hour_df = ToUpdate$Hour_df[i]) %>% 
    # Extrai dados do QueryKey
    mutate(CityCode = as.integer(str_sub(QueryKey, 1, 7))) %>% 
    mutate(Directions = str_sub(QueryKey, -2, -1)) %>% 
    # Une os dados de referência
    left_join(CityRef, by = c("CityCode", "Directions")) %>% 
    # Cria o indicador de quarentena
    mutate(TransitIndicator = RoteTrafficValue/CityRef)
  
  # Une aos dados principais
  TabResumo_New <- bind_rows(TabResumo_New, MonitorReport)
  
  rm(j, parsed_download, MonitorReport)

  # Espaçamento entre uma busca e outra
  Sys.sleep(sample(1:3, 1))
}
rm(i, ToUpdate)


if(nrow(TabResumo_New) > 0) {
  TabResumo_Join <- TabResumo_New %>% 
    # Para evitar flutuações, usarei a média aparada.
    group_by(CityCode, Day_df) %>%
    summarise(TransitIndicatorDay = mean(TransitIndicator,
                                         trim = 0.17, na.rm = TRUE)) %>% 
    left_join(select(Municipios, Munic_Id, Munic_Nome), 
              by = c("CityCode" = "Munic_Id")) %>% 
    rename(Cityname = Munic_Nome) %>% 
    mutate(Day_df = as.character(Day_df)) %>% 
    select(CityCode, Cityname, Day_df, TransitIndicatorDay) %>% 
    ungroup() %>%
    bind_rows(TabResumo_Old) %>% 
    arrange(Day_df, Cityname)
} else {
  TabResumo_Join <- TabResumo_Old %>% 
    arrange(Day_df, Cityname)
}
  

# # Salva a tabela resumo
# fwrite(TabResumo_Join, "data/dataset/TabResumo.csv",
#        sep = ";", dec = ",")

gcs_global_bucket("quarantine-monitor-bd-dev")

x <- suppressMessages(gcs_upload(TabResumo_Join, name = "TabResumo.csv"))

rm(x, TabResumo_Join, TabResumo_New, TabResumo_Old, Municipios, CityRef)

# Fim