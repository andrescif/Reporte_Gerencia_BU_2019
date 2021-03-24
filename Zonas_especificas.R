library(tidyverse)
Datos_EMEEFUT_17_20$DIRECCIOIN[Datos_EMEEFUT_17_20$ZONA==4]
!is.na(Datos_EMEEFUT_17_20$DIRECCIOIN[Datos_EMEEFUT_17_20$ZONA==4])
table(!is.na(Datos_EMEEFUT_17_20$DIRECCIOIN[Datos_EMEEFUT_17_20$ZONA==4]))
#Obtener los datos de zona 4
Zona_4<- subset.data.frame(Datos_EMEEFUT_17_20,
                  Datos_EMEEFUT_17_20$ZONA==4)

Zona_4$TELEFONO_RESPONSABLE
table(is.na(Zona_4$TELEFONO_RESPONSABLE))

