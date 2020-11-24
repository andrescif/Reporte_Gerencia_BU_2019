library(tidyverse)
library(eeptools)
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

#------------------------------------LIMPIAR DATOS-----------------------------------------#
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
#EDAD
Datos_BU_2019$EDAD <- as.integer(Datos_BU_2019$EDAD)
summary(Datos_BU_2019$EDAD)
#DESCRIPCION
table(Datos_BU_2019$DESCRIPCION, useNA = "ifany")
prop.table(table(Datos_BU_2019$DESCRIPCION, useNA = "ifany"))
#CURSO
table(Datos_BU_2019$CURSO, useNA = "ifany")
#Crear grupo de edad
Datos_BU_2019$GRUPO_EDAD <- NA
Datos_BU_2019$GRUPO_EDAD[Datos_BU_2019$EDAD<15] <- "MENOR"
Datos_BU_2019$GRUPO_EDAD[Datos_BU_2019$EDAD>14 &
                                 Datos_BU_2019$EDAD<30] <- "JOVEN"
Datos_BU_2019$GRUPO_EDAD[Datos_BU_2019$EDAD>29 &
                                 Datos_BU_2019$EDAD<60] <- "ADULTO"
Datos_BU_2019$GRUPO_EDAD[Datos_BU_2019$EDAD>59] <- "ADULTO MAYOR"
table(Datos_BU_2019$GRUPO_EDAD,useNA = "ifany")
prop.table(table(Datos_BU_2019$GRUPO_EDAD,useNA = "ifany"))
#Crear categorias de cursos
x<- levels(Datos_BU_2019$CURSO)

write.csv(x,
          file = "Tabla de cursos")
rm(x)

#Agregar la base de datos de EMEFUT
Datos_EMEEFUT_17_20 <- 
        read.csv("~/R Projects/Reporte_Gerencia_BU_2019/Datos/Datos_EMEEFUT_17_20.csv")
table(Datos_EMEEFUT_17_20$ANIO)
Datos_EMEEFUT_17_20$CUI <- as.character(Datos_EMEEFUT_17_20$CUI)
table(nchar(Datos_EMEEFUT_17_20$CUI))

#Agregar los tipos de cursos
Tabla_Tipo_Cursos <- 
        read.csv("~/R Projects/Reporte_Gerencia_BU_2019/Datos/Tabla_Tipo_Cursos.csv")
str(Tabla_Tipo_Cursos)
class(Tabla_Tipo_Cursos$TIPO_CURSO)
Datos_BU_2019$TIPO_CURSO <- NA
Datos_BU_2019$TIPO_CURSO <- 
        Tabla_Tipo_Cursos$TIPO_CURSO[match(Datos_BU_2019$CURSO,Tabla_Tipo_Cursos$CURSO)]
table(Datos_BU_2019$TIPO_CURSO, useNA = "ifany")
table(Tabla_Tipo_Cursos$TIPO_CURSO)
table(nchar(Datos_BU_2019$CUI))
rm(Tabla_Tipo_Cursos)

#Limpiar datos de EMEFUT
class(Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA)
Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA <- NA
Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA<- format(as.POSIXct(Datos_EMEEFUT_17_20$FECHA_NACIMIENTO,
                                                          format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA <- as.Date(Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA, format="%m/%d/%Y")


