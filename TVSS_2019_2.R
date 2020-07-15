#Importando cada uno de los archivos desde el origen

library(readr)
ConsultaData1 <- read_delim("ConsultaData1.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

ConsultaData2 <- read_delim("ConsultaData2.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

ConsultaData3 <- read_delim("ConsultaData3.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

ConsultaData4 <- read_delim("ConsultaData4.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

ConsultaData5 <- read_delim("ConsultaData5.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

ConsultaData6 <- read_delim("ConsultaData6.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

ConsultaData7 <- read_delim("ConsultaData7.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

ConsultaData8 <- read_delim("ConsultaData8.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

ConsultaData9 <- read_delim("ConsultaData9.csv", 
                            ";", escape_double = FALSE, col_types = cols(MontoEjecutadoTV = col_double()), 
                            trim_ws = TRUE)

sum(ConsultaData1$MontoEjecutadoTV, na.rm = TRUE)
ConsultaData1$MontoEjecutadoTV <- as.numeric(ConsultaData1$MontoEjecutadoTV)

#Luego de importar cada uno de los archivos se consolida en uno solo
TVSS_2019_2 <- rbind(ConsultaData1, ConsultaData2, ConsultaData3, ConsultaData4, ConsultaData5, ConsultaData6, 
                     ConsultaData7,ConsultaData8, ConsultaData9)
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

#Tabla cantidad de transferencias, RECEPTORES y valor del monto por entidad
Entidades <- TVSS_2019_2 %>% group_by(FuenteNombreEntidadKReporta) %>% 
  summarise(Transferencias = n(), Receptores = n_distinct(NumIDReceptor), 
            Monto = sum(MontoEjecutadoTV, na.rm = TRUE))

#Tabla cantidad de transferencias, REPORTADORES y monto por receptor
Receptores <- TVSS_2019_2 %>% group_by(NumIDReceptor) %>% 
  summarise(Transferencias = n(), Reportador = min(FuenteNroIdReportador),
            CantidadReportadores = n_distinct(FuenteNroIdReportador),
          Monto = sum(MontoEjecutadoTV, na.rm = TRUE))

#Tabla cantidad de muestras médicas, receptores y reportadores
MuestrasMedicas <- filter(TVSS_2019_2, TipoTransferenciaValor == 10)
MuestrasMedicas <- MuestrasMedicas %>% group_by(NroIdMuestraMedica) %>% 
  summarise(Transferencias = n(), Reportador = min(FuenteNroIdReportador),
            CantidadReportadores = n_distinct(FuenteNroIdReportador),
            Cantidad = sum(NumMuestrasMedicasEntregadas, na.rm = TRUE))

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

Receptores_menores4digitos<- TVSS_2019_2 %>% group_by(NumIDReceptor) %>% 
  summarise(Transferencias = n(), Reportador = min(FuenteNroIdReportador), 
            Monto = sum(MontoEjecutadoTV, na.rm = TRUE))

Receptores_Duda <- filter(Receptores, Receptores$CantidadReportadores == 1, 
                          Receptores$Monto > 0)

write.csv(Entidades, file="Entidades.csv")

#Pendiente ver relación entre muestras médicas, cantidad de monto de TV y prescripciones