# Script para criar uma tabela resumo com as informações mínimas
# para a atualização do site.

# Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data da criação: 2020-06-15

################### Setup #####################

library(googleCloudStorageR) # verificar se está logado
library(tidyverse)
library(data.table)
library(lubridate)


################### Inserindo dados antigos #####################

# Lê os dados brutos das buscas e rotas
QueriesIndex <- fread("data/dataset/QueriesIndex.csv", sep = ";", dec = ",", encoding = "UTF-8")
Route_raw <- fread("data/dataset/Routes.csv", sep = ";", dec = ",", encoding = "UTF-8")

# Une os dados das buscas com as rotas
Route <- Route_raw %>% 
  left_join(QueriesIndex, by = "QueryKey") %>%
  # Aproveito para mudar acionar o lubridate em alguns parâmetros
  mutate(NowTime = ymd_hms(NowTime)) %>% 
  mutate(Day_df = lubridate::as_date(NowTime)) %>%
  mutate(Hour_df = lubridate::hour(NowTime)) %>% 
  # Remover dados de dias incompletos
  group_by(Day_df) %>% 
  mutate(flag = ifelse(Hour_df == 20, 1, 0)) %>% 
  mutate(flag = max(flag, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(flag == 1) %>% 
  select(-flag)

rm(Route_raw, QueriesIndex)

# Valor de refência por cidade (tempo de percorrer as rotas de madrugada)
CityRef <- Route %>% 
  # Horários de referência
  dplyr::filter(Hour_df %in% c(0, 4)) %>% 
  # Média dos tempor por cidade e rota
  group_by(CityCode, Directions) %>% 
  summarise(CityRef = mean(RoteTrafficValue, na.rm = TRUE))

################### Criando a Tabela Resumo #####################

# Indicador de trânsito por cidade:
MonitorReport <- Route %>% 
  # Seleção de dados básica
  select(QueryKey, CityCode, Cityname, Directions, NowTime, Day_df, 
         Hour_df, RoteTrafficValue) %>% 
  # Exclui variáveis do período de referência e de extrações erradas
  dplyr::filter((Hour_df %in% c(8, 9, 12, 17, 18, 19))) %>% 
  # Limpa dias de feriado
  # dplyr::filter(!(Day_df %in% Holidays2020)) %>% 
  # Limpa dias de fim de semana
  # mutate(WeekDay = wday(NowTime)) %>% 
  # dplyr::filter(!(WeekDay %in% c(1, 7))) %>%
  # Une os dados de referência
  left_join(CityRef, by = c("CityCode", "Directions")) %>% 
  # Cria o indicador de quarentena
  mutate(TransitIndicator = RoteTrafficValue/CityRef) %>% 
  # Para evitar flutuações, usarei a média aparada.
  group_by(CityCode, Cityname, Day_df) %>% 
  summarise(TransitIndicatorDay = mean(TransitIndicator, 
                                       trim = 0.17, na.rm = TRUE))
rm(Route)
# rm(Holidays2020)

# table(MonitorReport$Day_df)

# Salva a tabela resumo
fwrite(MonitorReport, "data/dataset/TabResumo.csv",
       sep = ";", dec = ",")

# Salva a tabela com os valores de referência das cidades
fwrite(CityRef, "data/dataset/CityRef.csv",
       sep = ";", dec = ",")

# Upload no Google Cloud Storage
gcs_global_bucket("quarantine-monitor-bd-dev")
x <- suppressMessages(gcs_upload(MonitorReport, name = "TabResumo.csv"))
x <- suppressMessages(gcs_upload(CityRef, name = "CityRef.csv"))

rm(MonitorReport, CityRef, x)

# Fim