# Configura a instalação do git2R

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-05-15

# Feito basicamente com esse tutorial (caps 6-11)
# https://happygitwithr.com/install-git.html


## install if needed (do this exactly once):
## install.packages("usethis")

library(git2r)
library(usethis)

# Colocar as credenciais no git (só fazer uma vez)
use_git_config(user.name = "Murilojunqueira", 
               user.email = "m.junqueira@yahoo.com.br")

# https://happygitwithr.com/
# instalar o git (cap 6) (ok)
# Colocar as configurações básicas (cap 7) (ok)
# Instalar chaves SSH (cap 11) (ok)

# Tell git2r explicitly where to find your public and private key. 
file.exists("data/keys/id_rsa.pub", "data/keys/id_rsa")

cred <- git2r::cred_ssh_key(
  publickey = "data/keys/id_rsa.pub",
  privatekey = "data/keys/id_rsa"
)

usethis::use_github(credentials = cred)

# Deu certo! :-) !!




# Fim
