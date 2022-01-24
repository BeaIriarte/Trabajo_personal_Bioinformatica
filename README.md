# Trabajo_personal_Bioinformatica

library(nycflights13)

install.packages("nycflights13")

vuelos <- nycflights13::flights

vuelos

### borrar los NA de todas las filas

delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

vuelos_NA <- delete.na(vuelos)

## Ejercicio 1

?flights

tarde <- vuelos_NA[which(vuelos_NA$arr_delay > 60),]
dim(tarde)

##Ejercicio 2

dest_NY <- vuelos_NA[which(vuelos_NA$dest == "SFO"),]
dim(dest_NY)

dest_NY2 <- vuelos_NA[which(vuelos_NA$dest == "OAK"),]
dim(dest_NY2)



## Ejercicio 3

carrier_UA <- vuelos_NA[which(vuelos_NA$carrier == "UA"),]
carrier_UA

dim(carrier_UA)

carrier_AA <- vuelos_NA[which(vuelos_NA$carrier == "AA"),]
dim(carrier_AA)

## Ejercicio 4

abril <- vuelos_NA[which(vuelos_NA$month == 4),]
mayo <- vuelos_NA[which(vuelos_NA$month == 5),]
junio <- vuelos_NA[which(vuelos_NA$month == 6),]

dim(abril)
dim(mayo)
dim(junio)

sum(dim(abril), dim(mayo), dim(junio))


##Ejercicio 5

tarde_pronto <- tarde[which(tarde$dep_delay < 60),]
dim(tarde_pronto)               

## Ejercicio 6

tarde_pronto1 <- tarde[which(tarde$dep_delay < 30),]
dim(tarde_pronto1)

filter(tarde, dep_delay <60)

## Ejercicio 7

install.packages("tidyverse")
library(tidyverse)


nocturnos <- vuelos_NA[which(vuelos_NA$hour >= 0 & vuelos_NA$hour <= 7),]
dim(nocturnos)
       
### Ejercicio 8

desconocidos <- filter(vuelos, is.na(dep_time))
desconocidos

## Ejercicio 9

?is.na



## Ejercicio 10

ordenados <- arrange(vuelos_NA, desc(dep_delay))
ordenados

### ordenados de mayor tardanza en salir a más rápido.

filter(vuelos_NA, dep_delay < 0)

### aquellos que salen antes de tiempo

min(vuelos_NA$air_time)
#### Vuelo más rápido tarda 20 minutos.

min(vuelos_NA$dep_delay)
### Vuelo que antes sale

tail(ordenados)
#### Los seis que salen antes de lo previsto

## Ejercicio 11

vuelos_cortos <- vuelos_NA[order(vuelos_NA$air_time),]

head(vuelos_cortos)
head(vuelos_cortos$air_time)

## Ejercicio 12. Cuales tienen los trayectos más largos

vuelos_largos <- arrange(vuelos_NA, desc(air_time))

largos <- vuelos_NA[order(vuelos_NA$air_time, decreasing = TRUE),]
### Otra forma de ordenar el tiempo de vuelo de mayor a menor.

head(largos$air_time)
head(vuelos_largos)

## Ejercicio 13. Trayectos más cortos

head(vuelos_cortos)

## Ejercicio 14. 

vuelos_NA$HoraSalida <- c(vuelos_NA$hour*60+vuelos_NA$minute)
vuelos_NA$HoraSalida

vuelos_NA$HoraSalidaRetraso <- c(vuelos_NA$hour*60+vuelos_NA$minute+vuelos_NA$dep_delay)
vuelos_NA$HoraSalidaRetraso

## Ejercicio 15. dep_time, sched_dep_time, dep_delay

Dep_time es la hora real a la que el avión parte.
Sched_dep_time es la hora a la que estaba planeado partir.
Dep_delay son los minutos que se demora en salir el avion en función de la hora prevista. Por lo tanto, es la resta entre la hora de salida y la planeada


## Ejercicio 16.




## Ejercicio 19. ¿Qué compañía aérea sufre los peores retrasos?

peores_retrasos <- vuelos_NA[order(vuelos_NA$dep_delay, decreasing = TRUE),]
head(peores_retrasos)

aerolineas_peores <- head(peores_retrasos$carrier)
aerolineas_peores

## Ejercicio 20. Queremos saber qué hora del día nos conviene volar si queremos evitar los retrasos en la salida.

horassalida_peores <- head(peores_retrasos$hour)
## Las peores horas para salir son las 9, 16, 18, 19

menores_retrasos <- vuelos_NA[order(vuelos_NA$dep_delay),]
horassalida_mejores <- head(menores_retrasos$hour)
horassalida_mejores
## Las mejores horas para salir son las 7, 14, 17, 20, 21


## Ejercicio 21. Queremos saber qué día de la semana nos conviene volar si queremos evitar los retrasos en la salida.

mejores_dias <- head(menores_retrasos$day)
mejores_dias

## Ejercicio 22. Para cada destino, calcula el total de minutos de retraso acumulado.


## Ejercicio 23. Para cada uno de ellos, calcula la proporción del total de retraso para dicho destino.


## Ejercicio 24. Es hora de aplicar todo lo que hemos aprendido para visualizar mejor los tiempos de salida para vuelos cancelados vs los no cancelados. Recuerda bien qué tipo de dato tenemos en cada caso. ¿Qué deduces acerca de los retrasos según la hora del día a la que está programada el vuelo de salida?


## Ejercicio 25. Subir la carpeta a github y facilitar la url.


## Ejercicio 26. Al finalizar el documento agrega el comando sessionInfo()
