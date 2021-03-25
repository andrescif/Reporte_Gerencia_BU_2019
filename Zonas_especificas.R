library(tidyverse)
Datos_EMEEFUT_17_20$DIRECCIOIN[Datos_EMEEFUT_17_20$ZONA==4]
!is.na(Datos_EMEEFUT_17_20$DIRECCIOIN[Datos_EMEEFUT_17_20$ZONA==4])
table(!is.na(Datos_EMEEFUT_17_20$DIRECCIOIN[Datos_EMEEFUT_17_20$ZONA==4]))
#Obtener los datos de zona 4
Zona_4<- subset.data.frame(Datos_EMEEFUT_17_20,
                  Datos_EMEEFUT_17_20$ZONA==4)
Zona_4$TELEFONO_RESPONSABLE
table(is.na(Zona_4$TELEFONO_RESPONSABLE))
#¿Cuantos son del Campo Marte?
Zona_4_Ciudad <- subset.data.frame(Zona_4,
                  Zona_4$NOMBRE=="CAMPO MARTE ZONA 10" |
                          Zona_4$NOMBRE=="ROOSEVELT ZONA 11" |
                          Zona_4$NOMBRE=="CASTILLO LARA ZONA 7")
#Colocar municipio
Zona_4_Ciudad$MUNICIPIO <- "GUATEMALA"
Zona_4_Ciudad$MUNICIPIO[grep(pattern = "MIXCO",x=Zona_4_Ciudad$DIRECCIOIN)] <- "MIXCO"
Zona_4_Ciudad$MUNICIPIO[grep(pattern = "VILLA NUEVA",Zona_4_Ciudad$DIRECCIOIN)] <- "VILLA NUEVA"
Zona_4_Ciudad$MUNICIPIO[grep(pattern = "MINERVA",Zona_4_Ciudad$DIRECCIOIN)] <- "MIXCO"
Zona_4_Ciudad$MUNICIPIO[grep(pattern = "MONSERRAT",Zona_4_Ciudad$DIRECCIOIN)] <- "MIXCO"
Zona_4_Ciudad$MUNICIPIO[grep(pattern = "NARANJO",Zona_4_Ciudad$DIRECCIOIN)] <- "MIXCO"
Zona_4_Ciudad$MUNICIPIO[grep(pattern = "PETAPA",Zona_4_Ciudad$DIRECCIOIN)] <- "SAN MIGUEL PETAPA"

#Filtrar solo Guatemala
Zona_4_Ciudad <- subset.data.frame(Zona_4_Ciudad,
                                   Zona_4_Ciudad$MUNICIPIO=="GUATEMALA")
write.csv(Zona_4_Ciudad,
          file = "Zona 4 Activos")
