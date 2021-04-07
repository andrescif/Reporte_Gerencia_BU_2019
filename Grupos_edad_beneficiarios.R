library(tidyverse)
#Agregar estatus de los alumnos para distinguir activos de inactivos
EMEFUT_limpia$ESTATUS <- NA
EMEFUT_limpia$ESTATUS <- Datos_EMEEFUT_17_20$ESTATUS
#Distribuir por grupo de edad
EMEFUT_limpia$GRUPO_EDAD <- NA
EMEFUT_limpia$GRUPO_EDAD[EMEFUT_limpia$EDAD<15] <- "Niños"
EMEFUT_limpia$GRUPO_EDAD[EMEFUT_limpia$EDAD>14 &
                                 EMEFUT_limpia$EDAD<30] <- "Jóvenes"
table(EMEFUT_limpia$GRUPO_EDAD,useNA = "ifany")
table(EMEFUT_limpia$GRUPO_EDAD[EMEFUT_limpia$ESTATUS=="A"])
table(EMEFUT_limpia$GRUPO_EDAD[EMEFUT_limpia$ESTATUS=="B"])
table(EMEFUT_limpia$SEXO[EMEFUT_limpia$ESTATUS=="A"])
table(EMEFUT_limpia$SEXO[EMEFUT_limpia$ESTATUS=="B"])
