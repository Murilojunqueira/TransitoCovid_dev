# Configura a instalação do git2R

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-05-15

# Feito basicamente com esse tutorial (caps 6-11)
# https://happygitwithr.com/install-git.html


## install if needed (do this exactly once):
## install.packages("usethis")

library(git2r)
library(usethis)

###### Instala o git, github repo e o git2R #######

# Colocar as credenciais no git (só fazer uma vez)
use_git_config(user.name = "Murilojunqueira", 
               user.email = "m.junqueira@yahoo.com.br",
               gui.encoding = "uft8")

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

# Testar as chaves:
# usethis::use_github(credentials = cred) # Deu certo! :-) !!

# Adiciona a ssh no Sheel (apenas uma vez):
# eval `ssh-agent` 
# ssh-add

## Initialize the repository
repo <- repository()

## Display a brief summary of the new repository
repo

# Check our remotes
remotes(repo)

# Cria Repotes se não existir nenhum:
if(length(remotes(repo)) == 0) {
  # track an upstream branch (apenas uma vez por projeto)
  remote_add(repo, # adiciona um repositório aqui
             name = "origin", # nome do novo repositório
             url = "https://github.com/Murilojunqueira/TransitoCovid_dev.git")
}

# Agora é preciso determinar para o git usar ssh e não https 
# (apenas uma vez por projeto)
# ver: https://help.github.com/en/github/using-git/changing-a-remotes-url

# git remote -v
# git remote set-url origin git@github.com:Murilojunqueira/TransitoCovid_dev.git
# git remote -v

###### Checa a Instalação #######

# List all references in repository
references(repo)

# List all branches in repository
branches(repo)

# Workdir of repository
workdir(repo)

# List all commits in repository
commits(repo)

# Check our remotes
remotes(repo)

# Get HEAD of repository
repository_head(repo)

# Check if HEAD is head
is_head(repository_head(repo))

# Check if HEAD is local
is_local(repository_head(repo))

# List all tags in repository
tags(repo)

# Display configuration
config(repo)

###### Um primeiro commit & push para teste #######

# Create a new file
# writeLines("Hello world!", "test0.txt")

status(repo)

## Add file and commit
add(repo, "*") # colocar o asterisco é um truque para adicionar todos os arquivos

status(repo)

commit(repo, message = "Commit from RStudio 2020-06-18 12h38", all = TRUE)
commits(repo)

# Push
push(repo, 
     name = "origin", 
     refspec = "refs/heads/master", 
     credentials = cred)


# Fim
