# Analiza o comportamento do trânsito para avaliar a eficácia da quarentena

# A diferença desse script e do script FiguresWebSite.R é que esse
# código não processa os dados de data/dataset, extrai os dados
# diretamente de data/dataset/TabResumo.csv

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Data criação: 2020-04-08

library(googleCloudStorageR)
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(stringr)

################ Setup ####################

# # Para usar dados locais:
# # Dados dos gráficos
# TabResumo <- fread("data/dataset/TabResumo.csv",
#                  sep = ";", dec = ",", encoding = "UTF-8")
#
# # Lista de municípios
# Municipios <- fread("data/dataset/Municipios.csv",
#                     sep = ";", dec = ",")
# 
# # Lista dos feriados.
# # Inclui também dias de "enforca" de feriados (segunda e sexta)
# Holidays <- fread("data/dataset/Holidays.csv",
#                   sep = ";", dec = ",")

# Buscar dados na nuvem

# gcs_list_objects(bucket = bucket, detail = "more")

bucket <- "quarantine-monitor-bd-dev"
gcs_global_bucket(bucket)

# ?gcs_get_object
# gcs_get_global_bucket()
# gcs_list_objects(detail = "more")

f <- function(x) {
  suppressMessages(
    suppressWarnings(
      httr::content(x, encoding = "UTF-8")
    )
  )
}

# Dados dos gráficos
TabResumo <- gcs_get_object("TabResumo.csv", parseFunction = f) %>% 
  select(-X1)

# Lista de municípios
Municipios <- gcs_get_object("Municipios.csv", parseFunction = f) %>% 
  select(-X1)

# Lista dos feriados
Holidays <- gcs_get_object("Holidays.csv", parseFunction = f) %>%
  select(-X1)

# Exclui os feriados e pontos facultativos
TabResumo <- TabResumo %>% 
  mutate(Day_df = ymd(Day_df)) %>% 
  dplyr::filter(!(Day_df %in% dmy(Holidays$Dia))) %>% 
  # Limpa dias de fim de semana
  mutate(WeekDay = wday(Day_df)) %>% 
  dplyr::filter(!(WeekDay %in% c(1, 7))) %>% 
  # Correção dos nomes
  left_join(Municipios, by = c("CityCode" = "Munic_Id")) %>% 
  select(-Cityname) %>% 
  rename(Cityname = Munic_Nome) %>% 
  arrange(desc(Pop2018), Day_df) %>% 
  select(CityCode, Cityname, Day_df, TransitIndicatorDay)

# names(TabResumo)
# 
# # Lista de Cidades
# table(TabResumo$Cityname)
# 
# # Lista de dias buscados
# table(TabResumo$Day_df)

LarguraAltura = 1132/592
TamanhoAltura = 12 # cm
TamanhoLargura = TamanhoAltura * LarguraAltura
rm(LarguraAltura)

# Dias que aparecerão nos gráficos
Days_Plot <- seq(from = min(TabResumo$Day_df), 
                 to = max(TabResumo$Day_df), 
                 length.out = 15) %>% 
  unique() %>% ymd()


rm(Holidays, f, bucket)

################ Indicador de trânsito - média por dia ####################

MonitorGeneralDay <- TabResumo %>% 
  group_by(Day_df) %>% 
  summarise(TransitIndicatorDay = mean(TransitIndicatorDay, na.rm = TRUE))


# MonitorGeneralDay
# View(MonitorGeneralDay)

# Gráfico com os valores por dia
ggplot(data = MonitorGeneralDay, aes(x = Day_df, 
                                     y = TransitIndicatorDay, 
                                     group=1)) +
  geom_line(size = 1.5, color  = "pink") +
  geom_point(size = 1.5) +
  theme_classic(base_size = 12) +
  ggtitle("Progressão geral do trânsito ") +
  labs(x = "Dia", y = "Fluxo de Trânisto") +
  scale_x_continuous(breaks = Days_Plot) +
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
### MACAPÁ


# Grupo 1: capitais do SUDESTE

CityNameSelection <- c("SÃO PAULO", "RIO DE JANEIRO", "BELO HORIZONTE")

citySelection <- Municipios$Munic_Nome %>% 
  magrittr::is_in(CityNameSelection) %>% 
  which() %>% 
  Municipios$Munic_Id[.]


ggplot(data = dplyr::filter(TabResumo, CityCode %in% citySelection), 
       aes(x = Day_df, 
           y = TransitIndicatorDay, 
           group = CityCode)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  # geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  scale_x_continuous(breaks = Days_Plot) +
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
CityNameSelection <- c("PORTO ALEGRE", "CURITIBA", "FLORIANÓPOLIS")

citySelection <- Municipios$Munic_Nome %>% 
  magrittr::is_in(CityNameSelection) %>% 
  which() %>% 
  Municipios$Munic_Id[.]

ggplot(data = dplyr::filter(TabResumo, CityCode %in% citySelection), 
       aes(x = Day_df, 
           y = TransitIndicatorDay, 
           group = CityCode)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  # geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  scale_x_continuous(breaks = Days_Plot) +
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
CityNameSelection <- c("FORTALEZA", "RECIFE", "SÃO LUÍS")

citySelection <- Municipios$Munic_Nome %>% 
  magrittr::is_in(CityNameSelection) %>% 
  which() %>% 
  Municipios$Munic_Id[.]

ggplot(data = dplyr::filter(TabResumo, CityCode %in% citySelection), 
       aes(x = Day_df, 
           y = TransitIndicatorDay, 
           group = CityCode)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  # geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  scale_x_continuous(breaks = Days_Plot) +
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
CityNameSelection <- c("MANAUS", "BELÉM", "PORTO VELHO")

citySelection <- Municipios$Munic_Nome %>% 
  magrittr::is_in(CityNameSelection) %>% 
  which() %>% 
  Municipios$Munic_Id[.]

ggplot(data = dplyr::filter(TabResumo, CityCode %in% citySelection), 
       aes(x = Day_df, 
           y = TransitIndicatorDay, 
           group = CityCode)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  # geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  scale_x_continuous(breaks = Days_Plot) +
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
citySelection <- TabResumo %>% 
  # Seleciona apenas os dados das últimas duas semanas
  dplyr::filter(Day_df >= (as_date(now()) - ddays(14))) %>% 
  group_by(CityCode) %>% 
  summarise(TransitIndicatorDayMean = mean(TransitIndicatorDay, na.rm = TRUE)) %>% 
  arrange(desc(TransitIndicatorDayMean)) %>% 
  slice(1:5) %>% 
  select(CityCode) %>% 
  unlist() %>% as.character()


ggplot(data = dplyr::filter(TabResumo, CityCode %in% citySelection), 
       aes(x = Day_df, 
           y = TransitIndicatorDay, 
           group = CityCode)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  # geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  scale_x_continuous(breaks = Days_Plot) +
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
citySelection <- TabResumo %>% 
  # Seleciona apenas os dados das últimas duas semanas
  dplyr::filter(Day_df >= (as_date(now()) - ddays(14))) %>% 
  group_by(CityCode) %>% 
  summarise(TransitIndicatorDayMean = mean(TransitIndicatorDay, na.rm = TRUE)) %>% 
  arrange(TransitIndicatorDayMean) %>% 
  slice(1:5) %>% 
  select(CityCode) %>% 
  unlist() %>% as.character()

ggplot(data = dplyr::filter(TabResumo, CityCode %in% citySelection), 
       aes(x = Day_df, 
           y = TransitIndicatorDay, 
           group = CityCode)) +
  geom_line(size = 1.5, aes(colour = Cityname)) +
  # scale_size_manual(values = 3) +
  # geom_point(size = 1.5) +
  # Palheta de cores para daltônicos
  scale_colour_colorblind() + 
  # ggtitle("Indicador de trânsito por cidade") +
  labs(x = "Dia", y = "Fluxo de trânsito") +
  theme_classic(base_size = 14) +
  scale_x_continuous(breaks = Days_Plot) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

ggsave(filename = "doc/figures/website/MenorTransito.png",
       width = TamanhoLargura, 
       height = TamanhoAltura, 
       units = "cm", 
       device = "png")


rm(citySelection, CityNameSelection)

################ Um gráfico para cada cidade ####################


CityList <- unique(TabResumo$Cityname)

for (i in seq_along(CityList)) {
  
  # i <- 1
  
  message("Gráfico de ", CityList[[i]])
  
  CityPlot <- ggplot(data = dplyr::filter(TabResumo, 
                                          Cityname == CityList[[i]]), 
                     aes(x = Day_df, 
                         y = TransitIndicatorDay, 
                         group = 1)) +
    geom_line(size = 1.5, color  = "pink") +
    geom_point(size = 1.5) +
    theme_classic(base_size = 12) +
    scale_x_continuous(breaks = Days_Plot) +
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

writeLines(as.character(max(TabResumo$Day_df, na.rm = TRUE)), 
           "doc/figures/website/LastUpdate.txt")

# Limpa memória
rm(TamanhoAltura, TamanhoLargura, Days_Plot)
rm(CityRef, TabResumo, Municipios)
rm(ListaCidades, ListaImagens, LastUpdate)

message("Figures Updated")

# Fim