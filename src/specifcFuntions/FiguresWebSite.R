# Analiza o comportamento do trânsito para avaliar a eficácia da quarentena

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-04-08

library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(stringr)

################ Setup ####################

# Dados do tamanho da imagem
# width=1132 ; height=592
LarguraAltura = 1132/592
TamanhoAltura = 12 # cm
TamanhoLargura = TamanhoAltura * LarguraAltura
rm(LarguraAltura)

# Lê os dados brutos das buscas e rotas
QueriesIndex <- fread("data/dataset/QueriesIndex.csv", sep = ";", dec = ",", encoding = "UTF-8")
Route <- fread("data/dataset/Routes.csv", sep = ";", dec = ",", encoding = "UTF-8")

# Une os das das buscas com as rotas
Route <- Route %>% 
  left_join(QueriesIndex, by = "QueryKey") %>% 
  # Aproveito para mudar acionar o lubridate em alguns parâmetros
  mutate(NowTime = ymd_hms(NowTime)) %>% 
  mutate(Day_df = lubridate::as_date(NowTime)) %>%
  mutate(Hour_df = lubridate::hour(NowTime))

rm(QueriesIndex)

# names(Route)

# Lista de Cidades
# table(Route$Cityname)

# Lista de dias buscados
# table(Route$Day_df)

# Lista de dias horas buscadas
# table(Route$Hour_df)

################ Indicador básico ####################

# Lista dos feriados de 2020.
# Inclui também dias de "enforca" de feriados (segunda e sexta)
Holidays2020 <- c("2020-04-10", "2020-04-20", "2020-04-21", "2020-04-23", "2020-05-01") %>% ymd()

# Valor de refência por cidade (tempo de percorrer as rotas de madrugada)
CityRef <- Route %>% 
  # Horários de referência
  dplyr::filter(Hour_df %in% c(0, 4)) %>% 
  # Média dos tempor por cidade e rota
  group_by(CityCode, Directions) %>% 
  summarise(CityRef = mean(RoteTrafficValue, na.rm = TRUE))


# Seleção de dados básica
MonitorReport <- Route %>% 
  select(QueryKey, CityCode, Cityname, Directions, NowTime, Day_df, Hour_df, RoteTrafficValue) %>% 
  # Exclui variáveis do período de referência e de extrações erradas
  dplyr::filter((Hour_df %in% c(8, 9, 12, 17, 18, 19))) %>% 
  # Limpa dias de feriado
  dplyr::filter(!(Day_df %in% Holidays2020)) %>% 
  # Limpa dias de fim de semana
  mutate(WeekDay = wday(NowTime)) %>% 
  dplyr::filter(!(WeekDay %in% c(1, 7))) %>%
  # Une os dados de referência
  left_join(CityRef, by = c("CityCode", "Directions")) %>% 
  # Cria o indicador de quarentena
  mutate(TransitIndicator = RoteTrafficValue/CityRef) 

MonitorGeneralDayCity <- MonitorReport %>% 
  group_by(Cityname, Day_df) %>% 
  summarise(TransitIndicatorDay = mean(TransitIndicator, trim = 0.17, na.rm = TRUE))

rm(Route, Holidays2020)


################ Indicador de trânsito - média por dia ####################

MonitorGeneralDay <- MonitorGeneralDayCity %>% 
  group_by(Day_df) %>% 
  summarise(TransitIndicatorDay = mean(TransitIndicatorDay, na.rm = TRUE))


# MonitorGeneralDay
# View(MonitorGeneralDay)

# Gráfico com os valores por dia
ggplot(data = MonitorGeneralDay, aes(x = as.character(Day_df), y = TransitIndicatorDay, group=1)) +
  geom_line(size = 1.5, color  = "pink") +
  geom_point(size = 1.5) +
  theme_classic(base_size = 12) +
  ggtitle("Progressão geral do trânsito ") +
  labs(x = "Dia", y = "Fluxo de Trânisto") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print plot
ggsave(filename = "doc/figures/website/ByDay.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


rm(MonitorGeneralDay)

################ Gráfico de multiplas linhas por cidade - por região ####################

# Seleção de cidades
## Grupo 1
### SÃO PAULO
### RIO DE JANEIRO
### BELO HORIZONTE

## Grupo 2
### PORTO ALEGRE
### CURITIBA
### FLORIANÓPOLIS

## Grupo 3
### FORTALEZA
### RECIFE
### SÃO LUÍS

## Grupo 4
### MANAUS
### BELÉM
### mACAPÁ


# Grupo 1: capitais do SUDESTE
citySelection <- c("SÃO PAULO", "RIO DE JANEIRO", "BELO HORIZONTE")
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# Print plot
ggsave(filename = "doc/figures/website/Grupo1.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


# Grupo 2: capitais do SUL
citySelection <- c("PORTO ALEGRE", "CURITIBA", "FLORIANÓPOLIS")
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# Print plot
ggsave(filename = "doc/figures/website/Grupo2.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


# Grupo 3: capitais do NORDESTE
citySelection <- c("FORTALEZA", "RECIFE", "SÃO LUÍS")
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# Print plot
ggsave(filename = "doc/figures/website/Grupo3.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")

rm(citySelection)



# Grupo 4: capitais do NORTE
citySelection <- c("MANAUS", "BELÉM", "PORTO VELHO")
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

# Print plot
ggsave(filename = "doc/figures/website/Grupo4.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")

rm(citySelection)

################ Cidades que mais e menos respeitam a quarentena ####################


# Cidades com menor quarentena
citySelection <- MonitorGeneralDayCity %>% 
  # Seleciona apenas os dados das últimas duas semanas
  dplyr::filter(Day_df >= (as_date(now()) - ddays(14))) %>% 
  group_by(Cityname) %>% 
  summarise(TransitIndicatorDayMean = mean(TransitIndicatorDay, na.rm = TRUE)) %>% 
  arrange(desc(TransitIndicatorDayMean)) %>% 
  slice(1:5) %>% 
  select(Cityname) %>% 
  unlist() %>% as.character()


ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())


ggsave(filename = "doc/figures/website/MaiorTransito.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


# Cidades com maior quarentena
citySelection <- MonitorGeneralDayCity %>% 
  # Seleciona apenas os dados das últimas duas semanas
  dplyr::filter(Day_df >= (as_date(now()) - ddays(14))) %>% 
  group_by(Cityname) %>% 
  summarise(TransitIndicatorDayMean = mean(TransitIndicatorDay, na.rm = TRUE)) %>% 
  arrange(TransitIndicatorDayMean) %>% 
  slice(1:5) %>% 
  select(Cityname) %>% 
  unlist() %>% as.character()

ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

ggsave(filename = "doc/figures/website/MenorTransito.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


rm(citySelection)

################ Um gráfico para cada cidade ####################


CityList <- unique(MonitorReport$Cityname)

for (i in seq_along(CityList)) {
  
  # i <- 2
  
  message("Gráfico de ", CityList[[i]])
  
  CityPlot <- ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname == CityList[[i]]), 
                     aes(x = as.character(Day_df), 
                         y = TransitIndicatorDay, 
                         group = 1)) +
    geom_line(size = 1.5, color  = "pink") +
    geom_point(size = 1.5) +
    theme_classic(base_size = 12) +
    # ggtitle(paste("Indicador de trânsito de", CityList[[i]])) +
    labs(x = "Dia", y = "Fluxo de Trânisto") +
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  

  # Print plot
  ggsave(plot = CityPlot,
         filename = paste0("doc/figures/website/Transito ",  
                           str_pad(i, width = 3, pad = 0), " - ",
                           CityList[[i]], ".png"),
         width = TamanhoLargura, 
         height = TamanhoAltura, 
         units = "cm", 
         device = "png")
  
}
rm(i, CityList, CityPlot)


################ Finaliza Rotina ####################

# Grava a data do último 

writeLines(as.character(max(MonitorReport$Day_df, na.rm = TRUE)), 
           "doc/figures/website/LastUpdate.txt")

# Limpa memória
rm(TamanhoAltura, TamanhoLargura)
rm(CityRef, MonitorReport, MonitorGeneralDayCity)

message("Figures Updated")

# Fim