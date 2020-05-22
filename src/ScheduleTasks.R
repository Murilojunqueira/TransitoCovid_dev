# Programa para agendar tarefas

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-05-13


################# Setup ########################


# Para Iniciar o cron:
# sudo apt-get update
# sudo apt-get install -y cron
# sudo cron start

library(cronR)
library(lubridate)
library(stringr)

################# Teste cron ########################

# Checa tarefas
cronR::cron_njobs()
cronR::cron_ls()

# Testa o cron com o ping

Comando <- cron_rscript("src/ping2.R") # cria comando para executar o script

# Agenda tarefa
cron_add(Comando, frequency = "minutely", id = 'GCS_test1', description = 'Testedepload1')

# deleta tarefa
cronR::cron_rm("GCS_test1")

# Testa o cron com o MonitorFlowControlCloud.R

# Criar um comando para rodar o script
file.exists("src/MonitorFlowControlCloud.R")
Comando <- cron_rscript("src/MonitorFlowControlCloud.R") 

# Hora: dois minutos na frente de agora
ComandoTime <- paste0(str_pad(hour(now()) + 3, width = 2, pad = 0), ":", 
                      str_pad(as.integer(minute(now())) + 2, width = 2, pad = 0))

# ComandoTime <- "16:50" # insere hora manualmente

now()

# Tarefa com hora agendada (por algum motivo a frequencia por minuto não é respeitada)
cron_add(Comando, frequency = "daily", at = ComandoTime, id = 'test1', description = 'Testando agendamento')


# Checa tarefas
cronR::cron_njobs()
cronR::cron_ls()


# deleta tarefa
cronR::cron_rm("test1")

rm(ComandoTime, Comando)


################# Agendamento de rotinas ########################

library(cronR)

# Checagem de arquivo
file.exists("src/MonitorFlowControlCloud.R")
file.exists(normalizePath("src/MonitorFlowControlCloud.R"))

# Criar um comando para rodar o script
Comando <- cron_rscript("src/MonitorFlowControlCloud.R") 


# Horários: 8, 9, 10, 12, 15, 16, 17, 18, 19, 20
# Como o horário é GMT, sempre colocar 3 horas a mais (9hs -> 12hs)

# 8AM
cron_add(Comando, frequency = "daily", at = "11:00", id = 'Quarentena8hs', description = 'Monitoramento quarentena 8hs')

# 9AM
cron_add(Comando, frequency = "daily", at = "12:00", id = 'Quarentena9hs', description = 'Monitoramento quarentena 9hs')

# 10AM
cron_add(Comando, frequency = "daily", at = "13:00", id = 'Quarentena10hs', description = 'Monitoramento quarentena 10hs')

# 12AM
cron_add(Comando, frequency = "daily", at = "15:00", id = 'Quarentena12hs', description = 'Monitoramento quarentena 12hs')

# 15AM
cron_add(Comando, frequency = "daily", at = "18:00", id = 'Quarentena15hs', description = 'Monitoramento quarentena 15')

# 16AM
cron_add(Comando, frequency = "daily", at = "19:00", id = 'Quarentena16hs', description = 'Monitoramento quarentena 16')

# 17AM
cron_add(Comando, frequency = "daily", at = "20:00", id = 'Quarentena17hs', description = 'Monitoramento quarentena 17')

# 18AM
cron_add(Comando, frequency = "daily", at = "21:00", id = 'Quarentena18hs', description = 'Monitoramento quarentena 18')

# 19AM
cron_add(Comando, frequency = "daily", at = "22:00", id = 'Quarentena19hs', description = 'Monitoramento quarentena 19')

# 20AM
cron_add(Comando, frequency = "daily", at = "23:00", id = 'Quarentena20hs', description = 'Monitoramento quarentena 20')

# Checa tarefas
cronR::cron_njobs()
cronR::cron_ls()


# Fim