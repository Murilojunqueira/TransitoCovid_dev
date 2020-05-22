





# Acerta enconding

# geocoded_waypoints_Old <- fread(paste0("data/dataset/geocoded_waypoints.csv"), sep = ";", dec = ",", 
#                                 encoding = "UTF-8")

library(readr)

# ls()
# table_df <- Routes
# ToEnconding = "UTF-8"
# 
# View(table_df)



CorrectEnconding <- function(table_df, ToEnconding = "UTF-8") {
  
  table_Char <- names(table_df)[map_chr(table_df, class) == "character"]
  
  for (i in seq_len(length(table_Char))) {
    # i <- 1
    CurrentEncoding <- guess_encoding(table_df[[table_Char[i]]])[[1,1]]
    table_df[[table_Char[i]]] <- iconv(table_df[[table_Char[i]]], 
                                       from = CurrentEncoding, 
                                       to = ToEnconding)
  }
  return(table_df)
}

# Fim
