library(tidyverse)
#Cargar datos
Datos_BU_2019 <- 
        read.csv("~/R Projects/Reporte_Gerencia_BU_2019/Datos/Datos_BU_2019.csv")
#Explorar los datos
str(Datos_BU_2019)
table(Datos_BU_2019$DESCRIPCION,
      Datos_BU_2019$CURSO)
table(Datos_BU_2019$DESCRIPCION,useNA = "ifany")
table(Datos_BU_2019$ASIGNACION,useNA = "ifany")
table(is.na(Datos_BU_2019$ASIGNACION))
table(is.na(Datos_BU_2019$CUI))
table(Datos_BU_2019$CUI)
#Hace falta transformar las fechas a formato fecha y el cui a string para comparar largo
#CUI
Datos_BU_2019$CUI<- as.character(Datos_BU_2019$CUI)
table(nchar(Datos_BU_2019$CUI))
#FECHA INGRESO
table(Datos_BU_2019$FECHA_INGRESO)
Datos_BU_2019$FECHA_INGRESO_CORREGIDA<- format(as.POSIXct(Datos_BU_2019$FECHA_INGRESO,
        format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
Datos_BU_2019$FECHA_INGRESO_CORREGIDA <- as.Date(Datos_BU_2019$FECHA_INGRESO_CORREGIDA, format="%m/%d/%Y")
#FECHA DE NACIMIENTO
table(Datos_BU_2019$FECHA_NACIMIENTO)
Datos_BU_2019$FECHA_NACIMIENTO_CORREGIDA <- format(as.POSIXct(Datos_BU_2019$FECHA_NACIMIENTO,
                                                              format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
Datos_BU_2019$FECHA_NACIMIENTO_CORREGIDA <- as.Date(Datos_BU_2019$FECHA_NACIMIENTO_CORREGIDA, format="%m/%d/%Y")


