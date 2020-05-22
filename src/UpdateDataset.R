# Atualiza o banco de dados

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-05-15


# file.exists("src/specifcFuntions/MergeQueries.R")

# Baixa dados do Google Storage
source("src/specifcFuntions/DownloadCloudData.R")

# Fundi os dados de diferentes buscas no banco de dados principal.
source("src/specifcFuntions/MergeQueries.R")
