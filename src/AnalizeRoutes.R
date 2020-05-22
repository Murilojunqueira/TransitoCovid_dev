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
table(Route$Cityname)

# Lista de dias buscados
table(Route$Day_df)

# Lista de dias horas buscadas
table(Route$Hour_df)

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
  dplyr::filter(!(Hour_df %in% c(0, 4, 11, 10, 20, 15, 16, 21))) %>% 
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

rm(Route)


################ Indicador de trânsito - média por dia ####################

MonitorGeneralDay <- MonitorGeneralDayCity %>% 
  group_by(Day_df) %>% 
  summarise(TransitIndicatorDay = mean(TransitIndicatorDayCity, na.rm = TRUE))


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

################ Gráfico de multiplas linhas por cidade - Seleção arbitrária ####################

# Seleção de cidades
## Groupo 1
### SÃO PAULO
### RIO DE JANEIRO
### BELO HORIZONTE

## Grupo 2
### MANAUS
### BELÉM
### SÃO LUIS

## Grupo 3
### FORTALEZA
### RECIFE
### SALVADOR

# Grupo 1: capitais do sudeste
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
  ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print plot
ggsave(filename = "doc/figures/website/Grupo1.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


# Grupo 2: capitais do norte (e do MA)
citySelection <- c("MANAUS", "BELÉM", "SÃO LUÍS")
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print plot
ggsave(filename = "doc/figures/website/Grupo2.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


# Grupo 3: capitais do nordeste
citySelection <- c("FORTALEZA", "RECIFE", "SALVADOR")
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print plot
ggsave(filename = "doc/figures/website/Grupo3.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


rm(MonitorGeneralDayCity, citySelection)


################ Cidades que mais e menos respeitam a quarentena ####################


# Cidades com menor quarentena
citySelection <- MonitorGeneralDayCity %>% 
  group_by(Cityname) %>% 
  summarise(TransitIndicatorDayMean = mean(TransitIndicatorDay, trim = 0.1, na.rm = TRUE)) %>% 
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
  ggtitle("Cidades que menos respeitam a quarentena (entre as 50 maiores)") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))



ggsave(filename = "doc/figures/MaiorTransito.png",
       width = TamanhoLargura, height = TamanhoAltura, units = "cm", device = "png")


# Cidades com maior quarentena

citySelection <- Ind100DayMultCity %>% 
  group_by(Cityname) %>% 
  summarise(TIndicator100Mean = mean(TIndicator100Day, na.rm = TRUE)) %>% 
  arrange(TIndicator100Mean) %>% 
  slice(1:5) %>% 
  select(Cityname) %>% 
  unlist() %>% as.character()


ggplot(data = dplyr::filter(Ind100DayMultCity, Cityname %in% citySelection), 
       aes(x = as.character(Day_df), 
           y = TIndicator100Day, 
           group = Cityname)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  ggtitle("Cidades que mais respeitam a quarentena (entre as 50 maiores)", 
          subtitle  = "2020-04-06 = 100") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(filename = "doc/figures/MenorTransito.png",
       width = TamanhoLargura, height = TamanhoAltura, units = "cm", device = "png")


################ Um gráfico para cada cidade ####################


Ind100DayMultCity <- MonitorReport %>% 
  group_by(CityCode, Directions, Hour_df) %>% 
  mutate(Temp = ifelse(Day_df == as_date("2020-04-06"), TransitIndicator, NA)) %>% 
  mutate(Temp = max(Temp, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(TIndicator100 = TransitIndicator/abs(Temp)) %>% 
  group_by(Cityname, Day_df) %>% 
  summarise(TIndicator100Day = mean(TIndicator100, na.rm = TRUE)*100)



# width=1132 ; height=592
LarguraAltura = 1132/592
TamanhoAltura = 15 # cm
TamanhoLargura = TamanhoAltura * LarguraAltura

CityList <- unique(MonitorReport$Cityname)

for (i in seq_along(CityList)) {
  
  # i <- 2
  
  message("Gráfico de ", CityList[[i]])
  
  CityPlot <- ggplot(data = dplyr::filter(Ind100DayMultCity, Cityname == CityList[[i]]), 
                     aes(x = as.character(Day_df), 
                         y = TIndicator100Day, 
                         group = 1)) +
    geom_line(size = 1.5, colour = "red") +
    # scale_size_manual(values = 3) +
    geom_point(size = 1.5) +
    # Palheta de cores para daltônicos
    scale_colour_colorblind() + 
    ggtitle(paste("Indicador de trânsito de", CityList[[i]]), 
            subtitle  = "2020-04-06 = 100") +
    labs(x = "Dia", y = "Fluxo de trânsito") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 16), 
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # Print plot
  ggsave(plot = CityPlot,
         filename = paste0("doc/figures/ReportsByCity/Transito ",  
                           str_pad(i, width = 3, pad = 0), " - ",
                           CityList[[i]], ".png"),
         width = TamanhoLargura, 
         height = TamanhoAltura, 
         units = "cm", 
         device = "png")
  
}



################ Indicador de trânsito  - Média por dia e cidade ####################

# Seleção de Cidades:
# SÃO PAULO
# RIO DE JANEIRO
# MANAUS
# BELÉM
# FORTALEZA
# RECIFE


# Dados de São Paulo
CitySelection <- "SÃO PAULO"
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname == CitySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = 1)) +
  geom_line(size = 1.5, color  = "pink") +
  geom_point(size = 1.5) +
  theme_classic(base_size = 14) +
  ggtitle(paste0("Progressão geral do trânsito ", " - ", CitySelection)) +
  labs(x = "Dia", y = "Fluxo de Trânisto") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Dados de RIO DE JANEIRO
CitySelection <- "RIO DE JANEIRO"
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname == CitySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = 1)) +
  geom_line(size = 1.5, color  = "pink") +
  geom_point(size = 1.5) +
  theme_classic(base_size = 14) +
  ggtitle(paste0("Progressão geral do trânsito ", " - ", CitySelection)) +
  labs(x = "Dia", y = "Fluxo de Trânisto") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Dados de MANAUS
CitySelection <- "MANAUS"
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname == CitySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = 1)) +
  geom_line(size = 1.5, color  = "pink") +
  geom_point(size = 1.5) +
  theme_classic(base_size = 14) +
  ggtitle(paste0("Progressão geral do trânsito ", " - ", CitySelection)) +
  labs(x = "Dia", y = "Fluxo de Trânisto") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Dados de BELÉM
CitySelection <- "BELÉM"
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname == CitySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = 1)) +
  geom_line(size = 1.5, color  = "pink") +
  geom_point(size = 1.5) +
  theme_classic(base_size = 14) +
  ggtitle(paste0("Progressão geral do trânsito ", " - ", CitySelection)) +
  labs(x = "Dia", y = "Fluxo de Trânisto") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Dados de FORTALEZA
CitySelection <- "FORTALEZA"
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname == CitySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = 1)) +
  geom_line(size = 1.5, color  = "pink") +
  geom_point(size = 1.5) +
  theme_classic(base_size = 14) +
  ggtitle(paste0("Progressão geral do trânsito ", " - ", CitySelection)) +
  labs(x = "Dia", y = "Fluxo de Trânisto") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))



# Dados de RECIFE
CitySelection <- "RECIFE"
ggplot(data = dplyr::filter(MonitorGeneralDayCity, Cityname == CitySelection), 
       aes(x = as.character(Day_df), 
           y = TransitIndicatorDay, 
           group = 1)) +
  geom_line(size = 1.5, color  = "pink") +
  geom_point(size = 1.5) +
  theme_classic(base_size = 14) +
  ggtitle(paste0("Progressão geral do trânsito ", " - ", CitySelection)) +
  labs(x = "Dia", y = "Fluxo de Trânisto") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

rm(MonitorGeneralDayCity, CitySelection)


# Fim