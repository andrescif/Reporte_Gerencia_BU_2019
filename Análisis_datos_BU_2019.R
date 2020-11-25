library(tidyverse)
library(eeptools)
R.Version()
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

#Función para calcular edad en función de fecha de nacimeinto
age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age
}
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
#Crear edad corregida
Datos_BU_2019$EDAD_CORREGIDA <- NA
Datos_BU_2019$EDAD_CORREGIDA<- 
  age_years(Datos_BU_2019$FECHA_NACIMIENTO_CORREGIDA,as.Date("2019-12-31"))
summary(Datos_BU_2019$EDAD_CORREGIDA)
summary(Datos_BU_2019$EDAD)
hist(Datos_BU_2019$EDAD)
hist(Datos_BU_2019$EDAD_CORREGIDA)
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

Datos_EMEEFUT_17_20$GRUPO_EDAD <- NA
Datos_EMEEFUT_17_20$GRUPO_EDAD[Datos_EMEEFUT_17_20$EDAD<15] <- "MENOR"
Datos_EMEEFUT_17_20$GRUPO_EDAD[Datos_EMEEFUT_17_20$EDAD>14 &
                           Datos_EMEEFUT_17_20$EDAD<30] <- "JOVEN"
Datos_EMEEFUT_17_20$GRUPO_EDAD[Datos_EMEEFUT_17_20$EDAD>29 &
                           Datos_EMEEFUT_17_20$EDAD<60] <- "ADULTO"
table(Datos_EMEEFUT_17_20$GRUPO_EDAD, useNA = "ifany")
prop.table(table(Datos_EMEEFUT_17_20$GRUPO_EDAD, useNA = "ifany"))
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
Datos_EMEEFUT_17_20$TIPO_CURSO <- "DEPORTIVO"

#Limpiar datos de EMEFUT
class(Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA)
Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA <- NA
Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA<- format(as.POSIXct(Datos_EMEEFUT_17_20$FECHA_NACIMIENTO,
                                                          format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA <- as.Date(Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA, format="%m/%d/%Y")
Datos_EMEEFUT_17_20$EDAD<- NA
Datos_EMEEFUT_17_20$EDAD <- 
  age_years(Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA,as.Date("2019-12-31"))
summary(Datos_EMEEFUT_17_20$EDAD)
hist(Datos_EMEEFUT_17_20$EDAD)
Datos_EMEEFUT_17_20$FECHA_NACIMIENTO_CORREGIDA[is.na(Datos_EMEEFUT_17_20$EDAD)]

#Checamos sexo para limpiar los datos de eso
table(Datos_BU_2019$SEXO, useNA = "ifany")
prop.table(table(Datos_BU_2019$SEXO, useNA = "ifany"))
table(Datos_EMEEFUT_17_20$SEXO, useNA = "ifany")
prop.table(table(Datos_EMEEFUT_17_20$SEXO, useNA = "ifany"))

#Año de ingreso a la base de datos
class(Datos_BU_2019$FECHA_INGRESO_CORREGIDA)
Datos_BU_2019$ANIO <- NA
Datos_BU_2019$ANIO<- format.Date(Datos_BU_2019$FECHA_INGRESO_CORREGIDA,"%Y")
class(Datos_BU_2019$ANIO)
table(Datos_BU_2019$ANIO, useNA = "ifany")
table(Datos_EMEEFUT_17_20$ANIO)
class(Datos_EMEEFUT_17_20$ANIO_CORREGIDO)
Datos_EMEEFUT_17_20$ANIO_CORREGIDO<- substring(Datos_EMEEFUT_17_20$ANIO,3,5)
Datos_EMEEFUT_17_20$ANIO_CORREGIDO<- paste0("2",Datos_EMEEFUT_17_20$ANIO_CORREGIDO)

#Zona de habitación
table(Datos_BU_2019$MUNICIPIO,
      Datos_BU_2019$ZONA)
table(Datos_EMEEFUT_17_20$ZONA,useNA = "ifany")
table(Datos_BU_2019$MUNICIPIO[Datos_BU_2019$ZONA==0])

#Agregar "Descripción" a base EMEFUT
Datos_EMEEFUT_17_20$DESCRIPCION <- NA
Datos_EMEEFUT_17_20$DESCRIPCION <- "DESARROLLO SOCIAL"










