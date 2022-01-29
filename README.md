# Trabajo_personal_Bioinformatica

library(nycflights13)
library(tidyverse)

install.packages("nycflights13")

vuelos <- nycflights13::flights

vuelos

### borrar los NA de todas las filas

delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

vuelos_NA <- delete.na(vuelos)

## Ejercicio 1. Encuentra todos los vuelos que llegaron más de una hora tarde de lo previsto

?flights

tarde <- vuelos_NA[which(vuelos_NA$arr_delay > 60),]
dim(tarde)

##Ejercicio 2. Encuentra todos los vuelos que volaron hacia San Francisco (aeropuertos SFO y OAK)

dest_NY <- vuelos_NA[which(vuelos_NA$dest == "SFO"),]
dim(dest_NY)

dest_NY2 <- vuelos_NA[which(vuelos_NA$dest == "OAK"),]
dim(dest_NY2)

filter(vuelos_NA, dest == "OAK" | dest == "SFO")
Sanfrancisco <- 13173/309


## Ejercicio 3. Encuentra todos los vuelos operados por United American (UA) o por American Airlines (AA)

carrier_UA <- vuelos_NA[which(vuelos_NA$carrier == "UA"),]
carrier_UA

dim(carrier_UA)

carrier_AA <- vuelos_NA[which(vuelos_NA$carrier == "AA"),]
dim(carrier_AA)

filter(vuelos_NA, carrier == "AA" | carrier == "UA")
## Ejercicio 4. Encuentra todos los vuelos que salieron los meses de primavera (Abril, Mayo y Junio)

abril <- vuelos_NA[which(vuelos_NA$month == 4),]
mayo <- vuelos_NA[which(vuelos_NA$month == 5),]
junio <- vuelos_NA[which(vuelos_NA$month == 6),]

dim(abril)
dim(mayo)
dim(junio)

sum(dim(abril), dim(mayo), dim(junio))

abril_mayo_junio <- filter(vuelos_NA, month == "4" | month == "5" | month == "6")
dim(abril_mayo_junio)

##Ejercicio 5. Encuentra todos los vuelos que llegaron más de una hora tarde pero salieron con menos de una hora de retraso.

tarde <- vuelos_NA[which(vuelos_NA$arr_delay > 60),]

tarde_pronto60 <- tarde[which(tarde$dep_delay < 60),]
dim(tarde_pronto60)               

filter(vuelos_NA, arr_delay > 60 & dep_delay <60)

## Ejercicio 6. Encuentra todos los vuelos que salieron con más de una hora de retraso pero consiguieron llegar con menos de 30 minutos de retraso (el avión aceleró en el aire)

tarde <- vuelos_NA[which(vuelos_NA$arr_delay > 60),]
tarde_pronto30 <- tarde[which(tarde$dep_delay < 30),]
dim(tarde_pronto30)

tp <- filter(tarde, dep_delay < 30)
dim(tp)

## Ejercicio 7. Encuentra todos los vuelos que salen entre medianoche y las 7 de la mañana (vuelos nocturnos).

nocturnos <- vuelos_NA[which(vuelos_NA$hour >= 0 & vuelos_NA$hour <= 7),]
dim(nocturnos)
       
### Ejercicio 8. ¿Cuántos vuelos tienen un valor desconocido de dep_time?

desconocidos <- filter(vuelos, is.na(dep_time))
dim(desconocidos) 

## Ejercicio 9. ¿Qué variables del dataset contienen valores desconocidos?

?is.na
summary(vuelos)
## Las variables que contienen valores desconocidos son: dep_time (8255), dep_delay (8255), arr_time (8713) y arr_delay (9430).

## Ejercicio 10. Ordena los vuelos de flights para encontrar los vuelos más retrasados en la salida. ¿Qué vuelos fueron los que salieron los primeros antes de lo previsto?

Vuelos_mas_retrasados_salida <- arrange(vuelos_NA, desc(dep_delay))

### ordenados de mayor tardanza en salir a más rápido.

tail(Vuelos_mas_retrasados_salida)
#### Los seis que salen antes de lo previsto

## Ejercicio 11. Ordena los vuelos de flights para encontrar los vuelos más rápidos. Usa el concepto de rapidez que consideres.

mas_rapidos <- arrange(vuelos_NA, arr_delay)

### aquellos que llegan antes de tiempo van a ser considerados los más rápidos.

head(mas_rapidos)
## Seis vuelos más rápidos que llegan con más antelación.


head(arrange(vuelos_NA, desc(distance / air_time)))
## Otro concepto para buscar aquellos vuelos más rápidos es realizar la proporción entre la distancia y el tiempo en el aire.

## Ejercicio 12. ¿Qué vuelos tienen los trayectos más largos?

vuelos_largos <- arrange(vuelos_NA, desc(air_time))

largos <- vuelos_NA[order(vuelos_NA$air_time, decreasing = TRUE),]
### Otra forma de ordenar el tiempo de vuelo de mayor a menor.

head(largos$air_time)
head(vuelos_largos)

## También se pueden ver los vuelos más largos en función de la distancia:

vuelos_largos2 <- arrange(vuelos_NA, desc(distance))
head(vuelos_largos2)

## Ejercicio 13. ¿Qué vuelos tienen los trayectos más cortos?

vuelos_cortos <- vuelos_NA[order(vuelos_NA$air_time),]

head(vuelos_cortos)
head(vuelos_cortos$air_time)

vuelos_cortos2 <- arrange(vuelos_NA, distance)
head(vuelos_cortos2)

## Ejercicio 14. El dataset de vuelos tiene dos variables, dep_time y sched_dep_time muy útiles pero difíciles de usar por cómo vienen dadas al no ser variables continuas. Fíjate que cuando pone 559, se refiere a que el vuelo salió a las 5:59... Convierte este dato en otro más útil que represente el número de minutos que pasan desde media noche.


vuelos$horasalida<-(vuelos$dep_time %/% 100 * 60) + (vuelos$dep_time %% 100)
vuelos$horasalretraso<-(vuelos$sched_dep_time %/% 100 * 60) + (vuelos$sched_dep_time %% 100)

## Ejercicio 15. dep_time, sched_dep_time, dep_delay

Dep_time es la hora real a la que el avión parte.
Sched_dep_time es la hora a la que estaba planeado partir.
Dep_delay son los minutos que se demora en salir el avion en función de la hora prevista. Por lo tanto, es la resta entre la hora de salida y la planeada

flights_times <- mutate(vuelos,
  dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
  sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
    sched_dep_time %% 100) %% 1440
)

time2mins <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}

flights_times <- mutate(vuelos,
  dep_time_mins = time2mins(dep_time),
  sched_dep_time_mins = time2mins(sched_dep_time)
)

time2mins(vuelos$dep_time)


## Ejercicio 16.Investiga si existe algún patrón del número de vuelos que se cancelan cada día.

desconocidos <- filter(vuelos, is.na(dep_time))

desc_carrier <- table(desconocidos$carrier)
barplot(desc_carrier, main = "Cancelaciones según la Compañía Aérea", col = 2, legend.text = T)
## Parece que la compañía EV es aquella con mayor número de cancelaciones.

desc_dias <- table(desconocidos$day)
barplot(desc_dias, main = "Cancelaciones según el día del mes", col = 3)
## Parece que el día del mes con mayores cancelaciones es el 8.

desc_destinos <- table(desconocidos$dest)
barplot(desc_destinos, main = "Cancelaciones según el destino", col = 4)
## Parece que el destino con mayores cancelaciones es "ORF"

chisq.test(desconocidos$carrier, desconocidos$dest)


cancelled_per_day <- 
  vuelos %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled_num = sum(cancelled),
    flights_num = n(),
  )
  
  ggplot(cancelled_per_day) +
  geom_point(aes(x = flights_num, y = cancelled_num)) 


## Ejercicio 17. Investiga si la proporción de vuelos cancelados está relacionada con el retraso promedio por día en los vuelos.

cancelled_and_delays <- 
  
  vuelos %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled_prop = mean(cancelled),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup()
  
## Se intuye que existe una relación creciente entre el retraso medio de salida y el retraso medio de llegada y la proporción de vuelos cancelados.

proptard_canc <- 
  vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(cancelados_prop = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()

ggplot(proptard_canc) +
  geom_point(aes(x = med_dep_delay, y = cancelados_prop, col=cancelados_prop))
  
  ggplot(proptard_canc) +
  geom_point(aes(x = med_arr_delay, y = cancelados_prop, col=cancelados_prop))
  
## Ejercicio 18. Investiga si la proporción de vuelos cancelados está relacionada con el retraso promedio por aeropuerto en los vuelos.

proptard_canc_aer <- 
  vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(cancelados_prop = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()

ggplot(proptard_canc_aer) +
  geom_point(aes(x = med_dep_delay, y = cancelados_prop, col=cancelados_prop))
  
  ggplot(proptard_canc_aer) +
  geom_point(aes(x = med_arr_delay, y = cancelados_prop, col=cancelados_prop))

## Ejercicio 19. ¿Qué compañía aérea sufre los peores retrasos?

vuelos %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))

## Ejercicio 20. Queremos saber qué hora del día nos conviene volar si queremos evitar los retrasos en la salida.

vuelos %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
  
# Conviene volar a las 5,6,7,8 o 9 de la mañana.

## Ejercicio 21. Queremos saber qué día de la semana nos conviene volar si queremos evitar los retrasos en la salida.
library(lubridate)
  
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- vuelos %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))
  
  
# Conviene salir los días 4,6,15 o 29 de cada mes.
  
  
## Ejercicio 22. Para cada destino, calcula el total de minutos de retraso acumulado.

vuelos %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  
select(dest, month, day, dep_time, carrier, flight,
         arr_delay, arr_delay_prop) %>%
arrange(dest, desc(arr_delay_prop))


## Ejercicio 23. Para cada uno de ellos, calcula la proporción del total de retraso para dicho destino.

vuelos %>%
  filter(arr_delay > 0) %>%
  group_by(dest, origin, carrier, flight) %>%
  summarise(arr_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_prop = arr_delay / sum(arr_delay)
  ) %>%
  arrange(dest, desc(arr_delay_prop)) %>%
  select(carrier, flight, origin, dest, arr_delay_prop)


## Ejercicio 24. Es hora de aplicar todo lo que hemos aprendido para visualizar mejor los tiempos de salida para vuelos cancelados vs los no cancelados. Recuerda bien qué tipo de dato tenemos en cada caso. ¿Qué deduces acerca de los retrasos según la hora del día a la que está programada el vuelo de salida?

flights_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()

### Se puede deducir que a lo largo del transcurso del día, los vuelos cada vez se atrasan más en su salida. Esto parece que de madrugada se vuelve a normalizar y empieza otra vez a elevarse a partir de las 5 de la mañana.

## Ejercicio 25. Subir la carpeta a github y facilitar la url.


## Ejercicio 26. Al finalizar el documento agrega el comando sessionInfo()

sessionInfo()
