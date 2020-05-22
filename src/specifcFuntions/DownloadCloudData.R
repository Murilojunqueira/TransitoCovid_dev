# Script para baixar os dados 


# Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data da criação: 2020-14-05

################### Setup #####################


# página do Google Cloud Console: https://console.cloud.google.com/?hl=pt-br

library(googleCloudStorageR) # verificar se está logado
library(tidyverse)
library(data.table)
library(lubridate)

# 

# Checa o bucket
# gcs_get_global_bucket()
# gcs_get_bucket()

################### Download dos dados #####################

# Lista de arquivos do bucket
objects <- gcs_list_objects(detail = "more") %>% 
  dplyr::filter(!str_detect(name, "^Old/")) %>% 
  mutate(DownloadTime = NA) %>% 
  mutate(BucketDeleted = NA)

for (i in seq_len(nrow(objects))) {
  
  # i <- 5
  # get data
  message("Downloading ", objects$name[[i]])
  suppressMessages(
    parsed_download <- gcs_get_object(objects$name[[i]]))
  
  # Guarda a hora do download
  objects$DownloadTime[[i]] <- as.character(lubridate::now())
  
  # Save data
  fwrite(parsed_download, 
         file = paste0("data/QueryData/", objects$name[[i]]),
         sep = ";", dec = ",")
  
  # Copia o objeto para a pasta Old/
  x <- gcs_copy_object(objects$name[[i]], 
                  paste0("Old/", objects$name[[i]]))
  
  # Deleta o arquivo e guarda a mensagem de retorno
  objects$BucketDeleted[[i]] <- gcs_delete_object(objects$name[[i]])
}
rm(i, x)

# Salva controle dos arquivos baixados
fwrite(objects, "data/dataset/GoogleCloudFiles.csv", 
       append = TRUE)

rm(objects, parsed_download)


# Fim