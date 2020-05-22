# Atualiza as informações do site

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-05-15

library(rmarkdown)

# Consolida informações do banco
source("src/UpdateDataset.R")

# Atualiza as figuras do site
source("src/specifcFuntions/FiguresWebSite.R")

# Atualiza o HTML gerado pelos arquivos Rmd

render_site("index.Rmd")
render_site("Bycity.Rmd")
render_site("PersonaInfo.Rmd")

# Atualiza o GitHub
# install.packages("git2r")

library(git2r)

