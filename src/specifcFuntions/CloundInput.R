# Script para realizar o input inicial de dados no bucket 
# "quarantine-monitor-bd-dev" do GCS


# Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data da criação: 2020-06-16


############## Setup ###########

library(googleCloudStorageR)
# library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)

## get your project name from the API console
proj <- "rare-style-276702" # é o ID do projeto e não o nome

## get bucket info
buckets <- gcs_list_buckets(proj)
bucket <- "quarantine-monitor-bd-dev"
bucket_info <- gcs_get_bucket(bucket)
bucket_info

# Torna o bucket "quarantine-monitor-bd-dev" default
gcs_global_bucket(bucket)
gcs_get_global_bucket()


############## Upload data ###########


?gcs_upload

UploadFiles <- c("data/dataset/TabResumo.csv",
                 "data/dataset/Municipios.csv",
                 "data/dataset/Holidays.csv")

file.exists(UploadFiles)

FileNames <- str_remove(UploadFiles, "data/dataset/")

for (i in seq_along(UploadFiles)) {
  # i <-  1
  message("Uploading file ", FileNames[i])
  dataUpload <- fread(UploadFiles[i], sep = ";", dec = ",")
  suppressMessages(gcs_upload(dataUpload, name = FileNames[i]))
}
rm(i, dataUpload)

# Ckech uploaded
objects <- gcs_list_objects(detail = "more")
objects

rm(objects, FileNames, UploadFiles, bucket_info, bucket, buckets, proj)

# Fim