
install_or_load_pack <- function(pack){
  
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  
  if (length(create.pkg))
    
    install.packages(create.pkg, dependencies = TRUE)
  
  sapply(pack, require, character.only = TRUE)
  
}

install_or_load_pack(c("readr","hms","XLConnect","moments","ggplot2"))

setwd("C:/Users/LUIS/Desktop/Clases unam")


wb = loadWorkbook("Prices.xlsx")

# E L E G I R ----> sheet = "SP"/ "UST" / "MXN"
activo = "SP" #"UST" "SP" "MXN"
datos = readWorksheet(wb, sheet = activo, header = TRUE) 

activo = "SP" #"UST" "SP" "MXN"
datos = readWorksheet(wb, sheet = activo, header = TRUE) 
