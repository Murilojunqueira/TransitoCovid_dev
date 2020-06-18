# Push do projeto para o GitHub

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-05-18

library(git2r)
library(lubridate)

# Tell git2r explicitly where to find your public and private key. 
# file.exists("data/keys/id_rsa.pub", "data/keys/id_rsa")

cred <- git2r::cred_ssh_key(
  publickey = "data/keys/id_rsa.pub",
  privatekey = "data/keys/id_rsa"
)

## Initialize the repository
repo <- repository()

# status(repo)

## Add file and commit
add(repo, "*") # colocar o asterisco é um truque para adicionar todos os arquivos

commitMessage = paste0("Commit from RStudio ", gsub(":", "-", now()))

commit(repo, 
       message = commitMessage, 
       all = TRUE)

message(commitMessage)

# commits(repo)

# Push
push(repo, 
     name = "origin", 
     refspec = "refs/heads/master", 
     credentials = cred)

message("Commit pushed")

# Fim
