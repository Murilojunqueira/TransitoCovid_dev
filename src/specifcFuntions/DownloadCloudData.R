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
  
  # Sistema para tornar a busca mais robusta
  for (j in 1:5) {
    suppressMessages(
      parsed_download <- try(gcs_get_object(objects$name[[i]])))
    
    if(class(parsed_download)[1] == "try-error" || 
       isTRUE(nrow(parsed_download) == 0)) {
      
      message("Error in communication with Google Storage. Trying again...")
      Sys.sleep(3)
      
    } else {
      break
    }
  }
  # Mensagem de erro se não foi possível baixar o arquivo após 5 tentativas
  if(class(parsed_download)[1] == "try-error" || isTRUE(nrow(parsed_download) == 0)) {
    
    message("Unable to get file ", objects$name[[i]])
    next
  }
  
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
  
  # Espaçamento entre uma busca e outra
  Sys.sleep(sample(1:3, 1))
}
rm(i, j, x)

# Salva controle dos arquivos baixados
fwrite(objects, "data/dataset/GoogleCloudFiles.csv", 
       append = TRUE)

rm(objects, parsed_download)


# Fim