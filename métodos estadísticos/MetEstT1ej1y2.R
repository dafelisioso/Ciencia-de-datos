

# Importación de Librerías ------------------------------------------------

library(dplyr)
library(readr)
library(data.table)
library("scales")



# Carga de datos ----------------------------------------------------------

df <- fread('/home/ivanb/Downloads/iter_00_cpv2020_csv/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv')
#View(df)
#glimpse(df)


# Limpieza de Datos -------------------------------------------------------

#Vamos a tomar los totales de los resultados por municipio, los cuales 
# corresponden a Mun=0 y cuando Nom_Ent!=0 

df1 <- df %>% filter(LOC==0 & MUN != 0)


# 1.1 ---------------------------------------------------------------------

#1.1 a)

df1 %>%  glimpse()

# R:  Rows: 2,469 x Columns: 232 

tamano_df1 <- format(object.size(df1), units="Mb")
print(tamano_df1)

# R: 22.5 Mb

#1.1 b)

#Checamos el primer registro de POBTOT 
df[1,POBTOT]

#R: 126,014,024  https://www.inegi.org.mx/contenidos/saladeprensa/boletines/2021/EstSociodemo/ResultCenso2020_Nal.pdf


#1.1 c)
# R: Ya que hay municipios con 1 o 2 habitantes y en la descripción de todos los habitantes, todos los campos son nulos y la suma de estos no dan el total de habitantes, esto causaría una pérdida de datos sobre los habitantes totalees.



# 1.2  --------------------------------------------------------------------

#1.2 a)

# Total por entidad federativa
Ent_fed <- df %>% filter(MUN == 0 & LOC == 0 & ENTIDAD != 0)

df_ent_fed <- data.frame(
  Entidad = Ent_fed$NOM_ENT,
  P_12YMas_Act = as.integer(Ent_fed$PEA), 
  P12YMAS = as.integer(Ent_fed$P_12YMAS),
  Porcentaje = (as.integer(Ent_fed$PEA))*100/as.integer(Ent_fed$P_12YMAS)
)
 
#View(df_ent_fed)

par(mar=c(4,10,3,2), cex.axis=0.65)
entidad_plot <- barplot(height = df_ent_fed$Porcentaje, 
                        xlim=c(0,70),
                        ylab="",
                        main="",
                        names = df_ent_fed$Entidad, 
                        las=1, horiz=T, col="#69b3a2",
                        xlab=""
                        )

#text(df_ent_fed$Porcentaje, entidad_plot, pos = 0.5)
title(ylab="Entidades", line=8)
title(xlab="Porcentaje", line=2.5)
title(main="Población económicamente activa", line=1)

df_ent_fed$Porcentaje



# Total por delegación CDMX
Del_CDMX <- df %>% filter(MUN != 0 & LOC == 0 & ENTIDAD == 9)
View(Del_CDMX)

df_del <- data.frame(
  Entidad = Del_CDMX$NOM_MUN,
  P_12YMas_Act = as.integer(Del_CDMX$PEA), 
  P12YMAS = as.integer(Del_CDMX$P_12YMAS),
  Porcentaje = (as.integer(Del_CDMX$PEA))*100/as.integer(Del_CDMX$P_12YMAS)
)

par(mar=c(4,8,3,2), cex.axis=0.5)
del_plot <- barplot(height = df_del$Porcentaje, 
                        xlim=c(0,70),
                        ylab="",
                        main="",
                        names = df_del$Entidad, 
                        las=1, horiz=T, col="violetred1",
                        xlab=""
)

title(ylab="Delegaciones", line=6)
title(xlab="Porcentaje", line=2.5)
title(main="Población económicamente activa CDMX", line=1)




# 1.3 ---------------------------------------------------------------------

avg_P5_HLI <- df %>% filter(MUN != 0 & LOC == 0)
View(df_avg_P5_HLI)

df_avg_P5_HLI <- data.frame(
  Municipio = avg_P5_HLI$NOM_MUN,
  P5_HLI  = as.integer(avg_P5_HLI$P5_HLI), 
  P_5YMAS = as.integer(avg_P5_HLI$P_5YMAS),
  Porcentaje_P5_HLI  = (as.integer(avg_P5_HLI$P5_HLI))*100/as.integer(avg_P5_HLI$P_5YMAS)
)


df_avg_P15YM_AN <- data.frame(
  Municipio = avg_P5_HLI$NOM_MUN,
  P15YM_AN   = as.integer(avg_P5_HLI$P15YM_AN), 
  P_15YMAS = as.integer(avg_P5_HLI$P_15YMAS),
  Porcentaje_P15YM_AN = (as.integer(avg_P5_HLI$P15YM_AN))*100/as.integer(avg_P5_HLI$P_15YMAS)
)

View(df_avg_P15YM_AN)

View(df_avg_P5_HLI)

data_merge <- merge(df_avg_P15YM_AN, df_avg_P5_HLI) 

View(data_merge)

data_merge_rand <- data_merge[sample(nrow(data_merge), 10), ]
#View(data_merge_rand)


 
#Scatter Plot
par(mar=c(5,5,4,3), cex.axis=1)

plot(x = df_avg_P5_HLI$Porcentaje, 
     y = df_avg_P15YM_AN$Porcentaje, 
     col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.5),
     #col= data_merge_rand$Municipio,
     ylab="",
     main="",
     xlab=""
     )

title(ylab="Porcentaje de población de 15 años y más analfabeta", line=2,cex.lab=0.8)
title(xlab="Porcentaje de población de 5 años y más que habla alguna lengua indígena", line=2, cex.lab=0.8)
title(main="Analfabetismo y lenguas indígenas", line=1)
text(data_merge_rand$Porcentaje_P5_HLI+3, data_merge_rand$Porcentaje_P15YM_AN, labels= data_merge_rand$Municipio,cex= 0.65, pos=3)
points(x=data_merge_rand$Porcentaje_P5_HLI, y=data_merge_rand$Porcentaje_P15YM_AN, col='red' )

















