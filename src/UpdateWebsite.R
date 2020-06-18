# Atualiza as informações do site

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-05-15

# Consolida informações do banco
source("src/specifcFuntions/TabResumo_Update.R")

# Atualiza as figuras do site
source("src/specifcFuntions/FiguresWebSiteCloud.R")

# Atualiza o HTML gerado pelos arquivos Rmd
library(rmarkdown)
render_site("index.Rmd")
render_site("Bycity.Rmd")
render_site("PersonaInfo.Rmd")

# Commit and push to github
source("src/specifcFuntions/Push2GitHub.R")


