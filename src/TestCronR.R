# TestCronR

####### Principais comandos #######

# Cria tarefas no cron
# ?cron_add

# Exemplo de comando cron (não é R, é linux!)
# Rscript /home/mark/demo-schedule.R

# lista o número de tarefas agendadas
# cronR::cron_njobs()

# Lista as tarefas agendadas
# cronR::cron_ls()

# Cria códigos do r para serem executados com cron_add
# ?cronR::cron_rscript()

# Remove tarecas do cron
# ?cronR::cron_rm()

# Exemplo de remoção de tarefa (tem que passar o id da tarefa)
# cronR::cron_rm("9778c3e4794748680cf85ee37316f382")

# Limpa todas as tarefas
# cronR::cron_clear()

# Backup das tarefas agendadas:
# cron_save(file="crontab_backup", overwrite=TRUE)

####### Agendamento com Google Cloud Storage #######

library(cronR)
library(lubridate)
library(stringr)

now()

library(googleCloudStorageR)
googleAuthR::gar_auth_service("data/keys/gce_key.json")

# Cria tarefa

# Checa tarefas
cronR::cron_njobs()
cronR::cron_ls()

Comando <- cron_rscript("src/ping2.R") # cria comando para executar o script
Comando <- cron_rscript(normalizePath("src/ping2.R")) # cria comando para executar o script

# Agenda tarefa
cron_add(Comando, frequency = "minutely", id = 'GCS_test1', description = 'Testedepload1')

# deleta tarefa
cronR::cron_rm("GCS_test1")


?cron_add

# End
