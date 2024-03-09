# Importación de Librerías ------------------------------------------------

library(dplyr)
library(readr)
library(data.table)
library("scales")
library(plyr)
library(lubridate)


# Carga de datos ----------------------------------------------------------

df_covi <- fread('/home/ivanb/Downloads/covilimpio.csv')
#View(df_covi)
#glimpse(df)

#Tomamos los casos de la CDMX
df1_covi <- df_covi %>% filter(CLASIFICACION_FINAL %in% c(1,2) & ENTIDAD_RES==9)
#View(df1_covi)


# 2.2 1) ------------------------------------------------------------------


par(mar=c(6,5,3,1), cex.axis=0.7)
hist(df1_covi$FECHA_SINTOMAS, "months", freq = TRUE,
     ylab="",
     main="",
     las=3,
     col="turquoise3",
     xlab="")

title(ylab="Frecuencia", line=3)
title(xlab="Fecha", line=4.5)
title(main="Casos COIVID en CDMX", line=1)




# 2) ----------------------------------------------------------------------
#Primero nos quedamos con todos los pacientes de tipo 1
df_hosp <- df1_covi %>% filter(TIPO_PACIENTE == 1)

#Ahora le sumamos 15 días a la fecha de inicio de síntomas
df_hosp$HOSP_MES <- df_hosp$FECHA_SINTOMAS +15

#View(df_hosp)

#Graficamos
par(mar=c(6,5,3,1), cex.axis=0.7)
hist(df_hosp$HOSP_MES, "months", freq = TRUE,
     ylab="",
     main="",
     las=3,
     col="coral2",
     xlab="")

title(ylab="Frecuencia", line=3)
title(xlab="Fecha", line=4.5)
title(main="Hospitalizaciones COIVID en CDMX", line=1)


# 3) ----------------------------------------------------------------------
#Cambiamos el formato de las fechas
df_hosp$HOSP_MES_MA <- as.Date(df_hosp$HOSP_MES, format = "%d/%m/%Y")
df_hosp$HOSP_MES_MA <- format(df_hosp$HOSP_MES, format = "%Y/%m")

df1_covi$FECHA_SINTOMAS_FORMAT <- as.Date(df1_covi$FECHA_SINTOMAS, format = "%d/%m/%Y")
df1_covi$FECHA_SINTOMAS_FORMAT <- format(df1_covi$FECHA_SINTOMAS, format = "%Y/%m")

View(df1_covi)

#Contamos los casos por mes
HOSP_MES_MA <- count(df_hosp,'HOSP_MES_MA') 
FECHA_SINTOMAS_FORMAT <- count(df1_covi,'FECHA_SINTOMAS_FORMAT') 


#Eliminamos el último registro
HOSP_MES_MA <- HOSP_MES_MA[-c(25), ] 

#Creamos un data frame con los porcentajes
HOSP_MES_MA$Porcentaje <-  as.integer(HOSP_MES_MA$freq) / as.integer(FECHA_SINTOMAS_FORMAT$freq)


HOSP_MES_MA
HOSP_MES_MA

#FECHA_SINTOMAS_FORMAT
#HOSP_MES_MA

#Graficamos

mts <- ts(HOSP_MES_MA$Porcentaje, start = decimal_date(ymd("2020-03-01")),
          frequency = 12)


par(mar=c(5,5,3,1), cex.axis=0.7)
plot(mts,
     ylab="",
     main="",
   #  las=3,
     col="coral2",
     xlab="")


title(ylab="Porcentaje", line=3)
title(xlab="Fecha", line=3)
title(main="Porcentaje de Hospitalizaciones por Mes", line=1)


# 4) ----------------------------------------------------------------------
#Filtramos solo las defunciones
df_def<- df1_covi %>% filter(FECHA_DEF != '9999-99-99')
#dim(df_def)

#df_def$FECHA_DEF <- as.Date(df_def$FECHA_DEF, format="%d/%m/%Y")

df_def$DEF_MES_MA <- as.Date(df_def$FECHA_DEF, format =  "%Y-%m-%d")
df_def$DEF_MES_MA <- format(df_def$DEF_MES_MA, format = "%Y/%m")




#Contamos
DEF_MES_MA <- count(df_def,'DEF_MES_MA') 
FECHA_SINTOMAS_FORMAT

DEF_MES_MA$Porcentaje <-  as.integer(DEF_MES_MA$freq) / as.integer(FECHA_SINTOMAS_FORMAT$freq)


mts <- ts(DEF_MES_MA$Porcentaje, start = decimal_date(ymd("2020-03-01")),
          frequency = 12)

#Graficamos
par(mar=c(5,5,3,1), cex.axis=0.7)
plot(mts,
     ylab="",
     main="",
     #  las=3,
     col="coral2",
     xlab="")


title(ylab="Porcentaje", line=3)
title(xlab="Fecha", line=3)
title(main="Defunciones por COVID en CDMX", line=1)


# 2.3 1) ------------------------------------------------------------------

df_hosp <- df1_covi %>% filter(TIPO_PACIENTE == 1)
df_hosp$HOSP_MES <- df_hosp$FECHA_SINTOMAS +15

df_hosp$HOSP_MES_MA <- as.Date(df_hosp$HOSP_MES, format = "%d/%m/%Y")
df_hosp$HOSP_MES_MA <- format(df_hosp$HOSP_MES, format = "%Y/%m")

df1_covi$FECHA_SINTOMAS_FORMAT <- as.Date(df1_covi$FECHA_SINTOMAS, format = "%d/%m/%Y")
df1_covi$FECHA_SINTOMAS_FORMAT <- format(df1_covi$FECHA_SINTOMAS, format = "%Y/%m")



###########################################################################
#Menores a 20
###########################################################################
df_20_hosp <- df_hosp %>% filter(EDAD < 20)
df_20_sint <- df1_covi %>% filter(EDAD < 20)

HOSP_20 <- count(df_20_hosp,'HOSP_MES_MA') 
SINT_20 <- count(df_20_sint,'FECHA_SINTOMAS_FORMAT') 

HOSP_20
SINT_20 



#Eliminamos el último registro
HOSP_20 <- HOSP_20[-c(25), ] 

#Creamos un data frame con los porcentajes
HOSP_20$Porcentaje <-  as.integer(HOSP_20$freq) / as.integer(SINT_20$freq)

mts_20 <- ts(HOSP_20$Porcentaje, start = decimal_date(ymd("2020-03-01")),
          frequency = 12)


###########################################################################
#De 20 a 40
###########################################################################

df_40_hosp <- df_hosp %>% filter(EDAD >= 20 & EDAD < 40)
df_40_sint <- df1_covi %>% filter(EDAD >= 20 & EDAD < 40)

HOSP_40 <- count(df_40_hosp,'HOSP_MES_MA') 
SINT_40 <- count(df_40_sint,'FECHA_SINTOMAS_FORMAT') 

HOSP_40
SINT_40 



#Eliminamos el último registro
HOSP_40 <- HOSP_40[-c(25), ] 

#Creamos un data frame con los porcentajes
HOSP_40$Porcentaje <-  as.integer(HOSP_40$freq) / as.integer(SINT_40$freq)

mts_40 <- ts(HOSP_40$Porcentaje, start = decimal_date(ymd("2020-03-01")),
             frequency = 12)


###########################################################################
#De 40 a 60
###########################################################################

df_60_hosp <- df_hosp %>% filter(EDAD >= 40 & EDAD < 60)
df_60_sint <- df1_covi %>% filter(EDAD >= 40 & EDAD < 60)

HOSP_60 <- count(df_60_hosp,'HOSP_MES_MA') 
SINT_60 <- count(df_60_sint,'FECHA_SINTOMAS_FORMAT') 

HOSP_60
SINT_60 



#Eliminamos el último registro
HOSP_60 <- HOSP_60[-c(25), ] 

#Creamos un data frame con los porcentajes
HOSP_60$Porcentaje <-  as.integer(HOSP_60$freq) / as.integer(SINT_60$freq)

mts_60 <- ts(HOSP_60$Porcentaje, start = decimal_date(ymd("2020-03-01")),
             frequency = 12)

###########################################################################
#De 60 y mas
###########################################################################


df_80_hosp <- df_hosp %>% filter(EDAD >= 60)
df_80_sint <- df1_covi %>% filter(EDAD >= 60)

HOSP_80 <- count(df_80_hosp,'HOSP_MES_MA') 
SINT_80 <- count(df_80_sint,'FECHA_SINTOMAS_FORMAT') 

HOSP_80
SINT_80 



#Eliminamos el último registro
HOSP_80 <- HOSP_80[-c(25), ] 

#Creamos un data frame con los porcentajes
HOSP_80$Porcentaje <-  as.integer(HOSP_80$freq) / as.integer(SINT_80$freq)

mts_80 <- ts(HOSP_80$Porcentaje, start = decimal_date(ymd("2020-03-01")),
             frequency = 12)


#############################################################################
#Graficamos
#############################################################################
par(mar=c(5,5,3,3), cex.axis=0.7)
par(mfrow=c(2,2))  
plot(mts_20,
     ylab="",
     main="",
     #  las=3,
     col="mediumorchid1",
     xlab="")


title(ylab="Porcentaje", line=3)
title(xlab="Fecha", line=3)
title(main="Hospitalizaciones de menores de 20", line=1)

plot(mts_40,
     ylab="",
     main="",
     #  las=3,
     col="mediumorchid1",
     xlab="")


title(ylab="Porcentaje", line=3)
title(xlab="Fecha", line=3)
title(main="Hospitalizaciones de menores de 40 y mayores de 20", line=1)

plot(mts_60,
     ylab="",
     main="",
     #  las=3,
     col="mediumorchid1",
     xlab="")


title(ylab="Porcentaje", line=3)
title(xlab="Fecha", line=3)
title(main="Hospitalizaciones de menores de 60 y mayores de 40", line=1)

plot(mts_80,
     ylab="",
     main="",
     #  las=3,
     col="mediumorchid1",
     xlab="")


title(ylab="Porcentaje", line=3)
title(xlab="Fecha", line=3)
title(main="Hospitalizaciones mayores de 80", line=1)




























