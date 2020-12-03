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
#------------------------------------LIMPIAR DATOS-----------------------------------------
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
#Cambiar los factores
levels(Datos_BU_2019$SEXO)[levels(Datos_BU_2019$SEXO)=="F"] <- "Femenino"
levels(Datos_BU_2019$SEXO)[levels(Datos_BU_2019$SEXO)=="M"] <- "Masculino"
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

#Agregar "Municipio" a base EMEFUT
Datos_EMEEFUT_17_20$MUNICIPIO <- NA

#---------------------CREAR NUEVA BASE DE DATOS-------------------------------------------
BU_FINAL <- Datos_BU_2019 %>%
  select(DESCRIPCION,CUI,SEXO,ZONA,MUNICIPIO,FECHA_NACIMIENTO_CORREGIDA,GRUPO_EDAD,
        TIPO_CURSO,EDAD,ANIO)
EMEFUT_limpia<- Datos_EMEEFUT_17_20 %>%
  select(DESCRIPCION,CUI,SEXO,ZONA,MUNICIPIO,FECHA_NACIMIENTO_CORREGIDA,GRUPO_EDAD,
         TIPO_CURSO,EDAD,ANIO_CORREGIDO)
names(EMEFUT_limpia) <- c("DESCRIPCION","CUI","SEXO","ZONA","MUNICIPIO",
                          "FECHA_NACIMIENTO_CORREGIDA","GRUPO_EDAD","TIPO_CURSO","EDAD",
                          "ANIO")
BU_FINAL<- rbind(BU_FINAL,
      EMEFUT_limpia)
class(BU_2019$GRUPO_EDAD)
levels(BU_2019$GRUPO_EDAD)
BU_2019$GRUPO_EDAD <- as.factor(BU_2019$GRUPO_EDAD)
BU_2019$GRUPO_EDAD <- ordered(BU_2019$GRUPO_EDAD,
                              levels=c("MENOR","JOVEN","ADULTO","ADULTO MAYOR"))
#----------------------ANALISIS DE DATOS 2019 BU------------------------------------------
table(BU_FINAL$ANIO)
BU_2019 <- subset.data.frame(BU_FINAL,
                  BU_FINAL$ANIO==2019)
#La base de datos a utilizar es BU_2019
table(BU_2019$DESCRIPCION)
table(BU_2019$TIPO_CURSO)
prop.table(table(BU_2019$TIPO_CURSO))
#Limpiar NA de todos los valores relevantes
#CUI
table(nchar(BU_2019$CUI), useNA = "ifany")
BU_2019$CHAR_CUI <- nchar(BU_2019$CUI)
BU_2019<- subset.data.frame(BU_2019,
                  BU_2019$CHAR_CUI==13)
#Sexo
table(BU_2019$SEXO, useNA = "ifany")
#Edad
table(BU_2019$EDAD,useNA = "ifany")
BU_2019$FECHA_NACIMIENTO_CORREGIDA[is.na(BU_2019$EDAD)]
BU_2019$DESCRIPCION[is.na(BU_2019$EDAD)]
BU_2019$TIPO_CURSO[is.na(BU_2019$EDAD)]
BU_2019 <- subset.data.frame(BU_2019,
                  !is.na(BU_2019$EDAD))
#Beneficiarios por dirección y por tipo de beneficio
ggplot(BU_2019,
       aes(DESCRIPCION,fill=TIPO_CURSO))+
  geom_bar()+
  theme_bw()

x<- BU_2019 %>%
  group_by(DESCRIPCION,TIPO_CURSO) %>%
  summarise(Total_Beneficiarios=n(),
            Prop_Beneficiarios=Total_Beneficiarios/32609)
write.csv(x,
          file = "Beneficiarios 2019 por direccion y tipo curso")
rm(x)

#Beneficiarios por dirección y por sexo
ggplot(BU_2019,
       aes(DESCRIPCION,fill=SEXO))+
  geom_bar()+
  theme_bw()+
  labs(x="Dirección",
       y="Beneficiarios",
       title = "Beneficiarios por Dirección y Género")

x<- BU_2019 %>%
  group_by(DESCRIPCION,SEXO) %>%
  summarise(Total_beneficiarios=n(),
            Prop_beneficiarios=Total_beneficiarios/32609)
write.csv(x,
          file = "Beneficiarios 2019 por direccion y sexo")
rm(x)

#Beneficiarios por dirección y grupo de edad
table(is.na(BU_2019$GRUPO_EDAD))

x <- BU_2019 %>%
  group_by(DESCRIPCION,GRUPO_EDAD) %>%
  summarise(Total_beneficiarios=n(),
            Prop_beneficiarios=Total_beneficiarios/32609)
write.csv(x,
          file = "Beneficiarios 2019 por dirección y grupo de edad")
rm(x)

ggplot(BU_2019,
       aes(DESCRIPCION,
           fill=GRUPO_EDAD))+
  theme_bw()+
  geom_bar()+
  labs(x="Dirección",
       y="Beneficiarios",
       title = "Beneficiarios por Dirección y Grupo de Edad")+
  guides(fill=guide_legend(title="Grupo de edad"))
#Zona
table(BU_2019$ZONA, useNA = "ifany")
#Corregir zona
BU_2019$ZONA[BU_2019$ZONA==0] <- NA
BU_2019$ZONA[BU_2019$ZONA==22] <- NA
BU_2019$ZONA[BU_2019$ZONA==23] <- NA
BU_2019$ZONA[BU_2019$ZONA==53] <- NA
BU_2019$ZONA[BU_2019$ZONA==73] <- NA
prop.table(table(BU_2019$ZONA, useNA = "ifany"))
#Incluir regiones en la base de datos
BU_2019$REGION <- NA
#Region 1
BU_2019$REGION[BU_2019$ZONA==1] <- 1
BU_2019$REGION[BU_2019$ZONA==4] <- 1
BU_2019$REGION[BU_2019$ZONA==5] <- 1
BU_2019$REGION[BU_2019$ZONA==8] <- 1
BU_2019$REGION[BU_2019$ZONA==9] <- 1
BU_2019$REGION[BU_2019$ZONA==11] <- 1
#Region 2
BU_2019$REGION[BU_2019$ZONA==2] <- 2
BU_2019$REGION[BU_2019$ZONA==3] <- 2
BU_2019$REGION[BU_2019$ZONA==6] <- 2
BU_2019$REGION[BU_2019$ZONA==7] <- 2
BU_2019$REGION[BU_2019$ZONA==10] <- 2
BU_2019$REGION[BU_2019$ZONA==19] <- 2
#Region 3
BU_2019$REGION[BU_2019$ZONA==12] <- 3
BU_2019$REGION[BU_2019$ZONA==13] <- 3
BU_2019$REGION[BU_2019$ZONA==14] <- 3
BU_2019$REGION[BU_2019$ZONA==15] <- 3
BU_2019$REGION[BU_2019$ZONA==16] <- 3
BU_2019$REGION[BU_2019$ZONA==21] <- 3
#Region 4
BU_2019$REGION[BU_2019$ZONA==17] <- 4
BU_2019$REGION[BU_2019$ZONA==18] <- 4
BU_2019$REGION[BU_2019$ZONA==24] <- 4
BU_2019$REGION[BU_2019$ZONA==25] <- 4
#Revision
table(BU_2019$REGION, useNA = "ifany")
BU_2019$ZONA[is.na(BU_2019$REGION)]
table(BU_2019$ZONA, useNA = "ifany")
#Por region y grupo de edad
ggplot(BU_2019,
       aes(REGION,fill=GRUPO_EDAD))+
  theme_bw()+
  geom_bar()+
  labs(y="Beneficiarios",
       x="Región",
       title = "Beneficiarios 2019 por Región y Grupo de Edad")+
  guides(fill=guide_legend(title="Grupo de edad"))
32609-234
x<- subset.data.frame(BU_2019,
                  !is.na(BU_2019$REGION))

x <- x %>%
  group_by(REGION,GRUPO_EDAD) %>%
  summarise(Total_beneficiarios=n(),
            Prop_beneficiarios=Total_beneficiarios/32375)
write.csv(x,
          file = "Beneficiarios 2019 por región y grupo de edad")

#Beneficiarios por región y por sexo
ggplot(BU_2019,
       aes(REGION,fill=SEXO))+
  theme_bw()+
  geom_bar()+
  labs(x="Región",
       y="Beneficiarios",
       title = "Beneficiarios 2019 por Región y Sexo")+
  guides(fill=guide_legend(title="Sexo"))

x<- subset.data.frame(BU_2019,
                      !is.na(BU_2019$REGION))
x<- x %>%
  group_by(REGION,SEXO) %>%
  summarise(Total_beneficiarios=n(),
            Prop_beneficiarios=Total_beneficiarios/32375)
write.csv(x,
          file = "Beneficiarios 2019 por región y sexo")
rm(x)
#Edad de las mujeres atendidas
table(BU_2019$SEXO,useNA = "ifany")
ggplot(Datos_BU_2019,
       aes(EDAD))+
  theme_bw()+
  geom_freqpoly()+
  facet_wrap(~SEXO)
#----------------CORREGIR LA EDAD UTILIZANDO FECHA DE NACIMIENTO--------------------------
class(BU_2019$FECHA_NACIMIENTO_CORREGIDA)
BU_2019$ANIO_NAC <- 
  format(as.Date(BU_2019$FECHA_NACIMIENTO_CORREGIDA, format="%Y-%m/%d"),"%Y")
BU_2019$ANIO_NAC <- as.numeric(BU_2019$ANIO_NAC)
BU_2019$ANIO_ANALISIS <- 2019
class(BU_2019$ANIO_NAC)
class(BU_2019$ANIO_ANALISIS)
BU_2019$EDAD_NUEVA <- (BU_2019$ANIO_ANALISIS - BU_2019$ANIO_NAC)
table(BU_2019$EDAD_NUEVA)
summary(BU_2019$EDAD_NUEVA)
table(BU_2019$FECHA_NACIMIENTO_CORREGIDA[BU_2019$EDAD_NUEVA<1])
table(BU_2019$TIPO_CURSO[BU_2019$EDAD_NUEVA<1])
table(BU_2019$EDAD[BU_2019$EDAD_NUEVA<1])
table(BU_2019$EDAD_NUEVA, useNA = "ifany")
table(is.na(BU_2019$FECHA_NACIMIENTO_CORREGIDA))
summary(BU_2019$EDAD_NUEVA - BU_2019$EDAD)
boxplot(BU_2019$EDAD_NUEVA - BU_2019$EDAD)
summary(BU_2019$EDAD_NUEVA)
table(BU_2019$EDAD_NUEVA,useNA = "ifany")
class(BU_2019$EDAD_NUEVA)
#Eliminar los datos menores de 1 año de edad ya que carecen de sentido
BU_2019$EDAD_NUEVA[BU_2019$EDAD_NUEVA<1] <- NA
#Crear nueva base de datos para generar grupos de edad
Edad_FecNac <- BU_2019 %>%
  filter(!is.na(BU_2019$EDAD_NUEVA))
table(Edad_FecNac$SEXO)
class(Edad_FecNac$SEXO)
Edad_FecNac$SEXO[Edad_FecNac$SEXO=="F"]
#Cambiar los factores
levels(Edad_FecNac$SEXO)[levels(Edad_FecNac$SEXO)=="F"] <- "Femenino"
levels(Edad_FecNac$SEXO)[levels(Edad_FecNac$SEXO)=="M"] <- "Masculino"

ggplot(Edad_FecNac,
       aes(EDAD_NUEVA, colour=SEXO))+
  theme_bw()+
  geom_freqpoly(binwidth = 1)+
  labs(x="Años de edad",
       y="Beneficiarios",
       title = "Beneficiarios de Servicios Sociales 2019 por Edad y Sexo")

ggplot(Edad_FecNac,
       aes(EDAD_NUEVA, colour=DESCRIPCION))+
  theme_bw()+
  geom_freqpoly(binwidth = 1)+
  facet_wrap(~SEXO)+
  labs(x="Años de edad",
       y="Beneficiarios",
       title = "Beneficiarios por Sexo, Dirección de Atención y Edad")+
  guides(colour=guide_legend(title="Dirección que atiende"))





  




