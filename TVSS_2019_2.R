#Importando cada uno de los archivos desde el origen

#Se deben importar cada uno de los datos exportados desde PISIS

#Luego de importar cada uno de los archivos se consolida en uno solo
TVSS_2019_2 <- rbind(ConsultaData1, ConsultaData2, ConsultaData3, ConsultaData4, ConsultaData5, ConsultaData6, ConsultaData7,ConsultaData8)
View(TVSS_2019_2)

#Se exporta el archivo en .txt
write.table(TVSS_2019_2, file="TVSS_2019_2.txt", sep=";")

#Importando datos en caso de requerirlos
library(readr)
TVSS_2019_2 <- read.csv("TVSS_2019_2.txt")

#Valor total de las transferencias realizadas en el periodo 2019-12-31
sum(TVSS_2019_2$MontoEjecutadoTV, na.rm=TRUE)

str(TVSS_2019_2)
summary(TVSS_2019_2)

#Convirtiendo los tipos de variables
TVSS_2019_2$FechaTV <- as.Date(TVSS_2019_2$FechaTV)
TVSS_2019_2$MontoEjecutadoTV <- as.numeric(TVSS_2019_2$MontoEjecutadoTV)

#Gráfico monto ejecutado por fecha
plot(TVSS_2019_2$FechaTV, TVSS_2019_2$MontoEjecutadoTV)


install.packages("dplyr")
library("dplyr")

#Tabla cantidad de transferencias, receptores y valor del monto por entidad
Entidades <- TVSS_2019_2 %>% group_by(FuenteNombreEntidadKReporta) %>% 
  summarise(Transferencias = n(), Receptores = n_distinct(NumIDReceptor), 
            Monto = sum(MontoEjecutadoTV, na.rm = TRUE))

#Tabla cantidad de transferencias, reporotadores y monto por receptor
Receptores <- TVSS_2019_2 %>% group_by(NumIDReceptor) %>% 
  summarise(Transferencias = n(), Reportador = min(FuenteNroIdReportador),
            CantidadReportadores = n_distinct(FuenteNroIdReportador),
          Monto = sum(MontoEjecutadoTV, na.rm = TRUE))

#Casos específicos
filter(TVSS_2019_2, NombreReceptor == "CUENTA DE ALTO COSTO")
filter(TVSS_2019_2, NombreReceptor == "NUBIA STELLA OTERO VIANA")
Receptor_18342 <- filter(TVSS_2019_2, NumIDReceptor == 18342)
Receptor_38425 <- filter(TVSS_2019_2, NumIDReceptor == 38425)

PersonasNaturales <- filter(TVSS_2019_2, TipoIDReceptor == "CC")

ReceptoresNaturales <- PersonasNaturales %>% group_by(NumIDReceptor) %>% 
  summarise(Transferencias = n(), Reportador = n_distinct(FuenteNroIdReportador), 
            TipoReceptor  = n_distinct(TipoReceptorTV), Entidad = n_distinct(FuenteNroIdReportador),
            Monto = sum(MontoEjecutadoTV, na.rm = TRUE))

min(PersonasNaturales$NumIDReceptor, na.rm = TRUE)
max(PersonasNaturales$NumIDReceptor, na.rm = TRUE)
mean(PersonasNaturales$NumIDReceptor, na.rm = TRUE)

summary(PersonasNaturales)

install.packages("modeest")
library("modeest")

frecuenciasTipoTransferencias <- table(TVSS_2019_2$TipoTransferenciaValor)
prop.table(frecuenciasTipoTransferencias)

plot(x = PersonasNaturales$TipoTransferenciaValor,  y = PersonasNaturales$NumIDReceptor)

save(TVSS_2019_2, file="TVSS_2019_2")

Receptores_menores4digitos<- TVSS_2019_2 %>% group_by(NumIDReceptor) %>% 
  summarise(Transferencias = n(), Reportador = min(FuenteNroIdReportador), 
            Monto = sum(MontoEjecutadoTV, na.rm = TRUE))

Receptores_Duda <- filter(Receptores, Receptores$CantidadReportadores == 1, 
                          Receptores$Monto > 0)

write.csv(Entidades, file="Entidades.csv")
