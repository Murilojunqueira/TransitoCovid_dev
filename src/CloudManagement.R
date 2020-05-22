
# Rotina para acertar as partes da nuvem

# Autor: Murilo Junqueira (m.junqueira@yahoo.com.br)

################### Setup #####################

# quarantine-monitor-cloud

# .Renviron
# GCE_AUTH_FILE="data/keys/gce_key.json"
# GCE_DEFAULT_PROJECT_ID="rare-style-276702"
# GCE_DEFAULT_ZONE="southamerica-east1-a"
# GCS_AUTH_FILE="data/keys/gcs_key.json"
# GCS_DEFAULT_BUCKET=quarantine-monitor-cloud

# Página no Google Cloud Console:
# https://console.cloud.google.com/home/dashboard?project=apt-impact-243819&hl=pt-br


library(googleComputeEngineR)
library(googleCloudStorageR)


################### Criando Máquina Virtual (VM) #####################

# Checa projeto
gce_global_project()
project_obj <- gce_get_project()
project_obj$kind
project_obj$name
project_obj$id
project_obj$commonInstanceMetadata
project_obj$xpnProjectStatus


# Iniciando uma Instância

## rstudio template, but with custom rstudio build with cron, googleAuthR etc. 
tag <- gce_tag_container("google-auth-r-cron-tidy", project = "gcer-public")

vm <- gce_vm(name = "quarantinemonitor", 
             predefined_type = "n1-standard-1",
             template = "rstudio", 
             dynamic_image = tag, 
             username = "murilojunqueira", 
             password = "ihBCm8L12MK*")

# Check VM
vm$status
vm$name


# Reinicia instâncias
job <- gce_vm_start("quarantinemonitor")

# Chega as instâncias
gce_list_instances()

# Finaliza instâncias
# job <- gce_vm_stop("quarantinemonitor")


# Fim