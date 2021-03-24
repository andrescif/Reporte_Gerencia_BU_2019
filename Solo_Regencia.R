library(tidyverse)

#Filtrar datos
table(Datos_EMEEFUT_17_20$ZONA)

EMEFUT_solo_regencia<- filter(Datos_EMEEFUT_17_20,
       Datos_EMEEFUT_17_20$ZONA==18|
               Datos_EMEEFUT_17_20$ZONA==17|
               Datos_EMEEFUT_17_20$ZONA==24|
               Datos_EMEEFUT_17_20$ZONA==25)

#Solo Regencia son:
263+ 947+108 + 10 

write.csv(EMEFUT_solo_regencia,
          file = "Tabla solo Regencia")
