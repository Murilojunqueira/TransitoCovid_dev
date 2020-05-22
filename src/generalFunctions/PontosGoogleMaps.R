
# Função para buscar uma lista de endereços no Google Maps. Retorna um banco de dados.

# Criado por Murilo Junqueira - m.junqueira@yahoo.com.br

# Criado em: 2017-02-16 (projeto doutorado)

# Modificado em: 2019-06-15

####################### Histórico ##############################################

# Construção original:

# Script para pegar o ponto geográficos dos municípios

# Apesar do IBGE ter as coordenadas municipais, não temos bons pontos, pois
# só podemos usar os polígonos para encontrar o centroide, que muitas vezes fica
# no meio no nada. Usando O google, encontramos o centroide da área urbana, que 
# é uma medida muito melhor.

# 2019-06-15: modificado para ser uma busca geral de pontos no googlemaps

################### Referências ########################

# https://github.com/dkahle/ggmap
# https://lucidmanager.org/geocoding-with-ggmap/
# https://bigdataenthusiast.wordpress.com/tag/google-maps-api-in-r/
# http://rgooglemaps.r-forge.r-project.org/QuickTutorial.html


########################## Debug ##################################

# Adress.List <- Ofertas.BuscaGoogle$BuscaNome[1:10]
# 

################### Função  ########################


# Precisa que o usuário esteja conectado a uma conta Google Cloud e o serviço de Geocoding esteja ativado.
# Para que o serviço esteja ativado, é preciso registrar uma conta de cartão de crédito.

PontosGoogleMaps <- function(Adress.List) {
  
  GeoCodePontos <- data.frame()
  
  for (i in seq_along(Adress.List)) {
    
    # i <- 1
    
    # Imprime
    print(paste("Pesquisando", Adress.List[i],":", i, "de", length(Adress.List)))  
    
    Position <- geocode(Adress.List[i], source = "google", output =  "more")
    
    if(is.na(Position[1,1])) {next()}
    
    # Cria o banco de dados
    if(ncol(GeoCodePontos) == 0) {
      GeoCodePontos <- data.frame(matrix(ncol =  ncol(Position), nrow = length(Adress.List) ) )
      names(GeoCodePontos) <- names(Position)
    }
    
    GeoCodePontos[i,] <- Position
    
    rm(Position)
    gc()
    
  }
  
  return(GeoCodePontos)
  
}
