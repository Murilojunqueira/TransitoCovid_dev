

library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(stringr)

# Indicadores de quarentena
# A: Modelo minutos absolutos – referência
# B: Modelo minutos absolutos/referência
# C: Modelo minutos (absolutos – referência) / (absolutos inicial – referência)
# D: Modelo minutos (absolutos/referência) / (absolutos inicial / referência) 

# Variações:
## 1: Aumento de 1 minuto de trajeto por dia 
## 2: Diminuição de 1 minuto de trajeto por dia 
## 3: Aumento de 1, 3, 5, 10, 20 minuto de trajeto por dia 
## 4: Diminuição de 1, 3, 7, 9 minuto de trajeto por dia 

# Casos:
## I: Cidade com trajeto de 10 minutos.
## II: Cidade com trajeto de 20 minutos.
## III: Cidade com trajeto de 30 minutos.


# Criação de casos e variações

Cidades <- list(Cidade = c("I", "II", "III"),
                TempoBase = c(10, 20, 30)) %>% 
  as_tibble()

Variacoes <- list(Variacao = c(1:4, 
                              -1:-4,
                              c(1, 5, 10, 20), 
                              -c(1, 3, 7, 9))) %>% 
  as_tibble()



# União Cidade e variação. Criação dos indicadores
CidadeVar <- list(Variacao = rep(Variacoes$Variacao, nrow(Cidades)), 
                  Cidade = unlist(map(Cidades$Cidade, rep, times = nrow(Variacoes))),
                  Dia = rep(1:4, 4*nrow(Cidades)), 
                  VarIndex = rep(unlist(map(1:4, rep, times = 4)), 3)) %>% 
  as_tibble() %>% 
  left_join(Cidades, by = "Cidade") %>% 
  select(Cidade, Dia, TempoBase, VarIndex, Variacao) %>%
  mutate(TempoGasto = TempoBase + Variacao) %>% 
  # A: Modelo minutos absolutos – referência
  mutate(IndicadorA = TempoGasto - TempoBase) %>% 
  # B: Modelo minutos absolutos/referência
  mutate(IndicadorB = TempoGasto/TempoBase) %>% 
  # C: Modelo minutos (absolutos – referência) / (absolutos inicial – referência)
  group_by(Cidade, VarIndex) %>% 
  mutate(Temp = ifelse(Dia == 1, IndicadorA, NA)) %>% 
  mutate(Temp = max(Temp, na.rm = TRUE)) %>% 
  mutate(IndicadorC = IndicadorA/abs(Temp)) %>% 
  ungroup() %>% 
  # D: Modelo minutos (absolutos/referência) / (absolutos inicial / referência) 
  group_by(Cidade, VarIndex) %>% 
  mutate(Temp = ifelse(Dia == 1, IndicadorB, NA)) %>% 
  mutate(Temp = max(Temp, na.rm = TRUE)) %>% 
  mutate(IndicadorD = IndicadorB/abs(Temp)) %>% 
  ungroup() %>%  
  select(-Temp) %>% 
  gather(Indicador, IndValor, IndicadorA:IndicadorD) %>% 
  arrange(Indicador, VarIndex, Cidade, Dia) %>%
  dplyr::filter(!(Indicador %in% c("IndicadorA", "IndicadorC")))


table(CidadeVar$Cidade)

table(CidadeVar$VarIndex)

table(CidadeVar$Indicador)

View(CidadeVar)


names(CidadeVar)

# Análise dos indicadores

# Comparando os 4 indicadores na mesma cidade e mesma variação

# width=1132 ; height=592
LarguraAltura = 1132/592
TamanhoAltura = 15 # cm
TamanhoLargura = TamanhoAltura * LarguraAltura

for (i in seq_len(nrow(Cidades))) {
  # i <- 1
  for (j in 1:4) {
    # j <- 3
    Cidades$Cidade[i]
    
    filter(CidadeVar, Cidade == Cidades$Cidade[i] , VarIndex == j) 
    
    ggplot(data = dplyr::filter(CidadeVar, Cidade == Cidades$Cidade[i], VarIndex == j), 
           aes(x = as.character(Dia), 
               y = IndValor, 
               group = Indicador)) +
      geom_line(size = 1, aes(color = Indicador)) +
      geom_point() +
      ggtitle(paste("Cidade", Cidades$Cidade[i], " - Variação", j)) +
      theme_classic(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, size = 16), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Print plot
    ggsave(filename = paste0("doc/figures/TestIndicadors/", 
                             i, j,
                             " Cidade", Cidades$Cidade[i], 
                             " - Indicador", j, ".png"),
           width = TamanhoLargura, height = TamanhoAltura, units = "cm", device = "png")
  }
}


# Comparando o mesmo indicador nas 3 cidades

table(CidadeVar$Indicador)

listIndicador <- c("IndicadorA", "IndicadorB", "IndicadorC", "IndicadorD")

for (i in 1:4) {
  # i <- 4
  for (j in 1:4) {
    # j <- 4
    
    filter(CidadeVar, Indicador == listIndicador[i] , VarIndex == j) 
    
    ggplot(data = dplyr::filter(CidadeVar, Indicador == listIndicador[i], VarIndex == j), 
           aes(x = as.character(Dia), 
               y = IndValor, 
               group = Cidade)) +
      geom_line(size = 1, aes(color = Cidade)) +
      geom_point() +
      ggtitle(paste(listIndicador[i], " - Variação", j)) +
      theme_classic(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, size = 16), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Print plot
    ggsave(filename = paste0("doc/figures/TestIndicadors/", 
                             "Teste2 - ", i, j,
                             " Cidade", Cidades$Cidade[i], 
                             " - Indicador", j, ".png"),
           width = TamanhoLargura, height = TamanhoAltura, units = "cm", device = "png")
  }
}


# Fim

