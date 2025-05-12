#Limpieza de datos
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

# Carga del dataset
df <- read.csv('hotel_bookings.csv', header = TRUE, sep = ',', dec = '.', stringsAsFactors = FALSE)
View(df) # Visualización de la base de datos

# Librerias
library(tidyverse)
library(VIM)
library(ggplot2)
library(dplyr)
library(patchwork)  # Asegúrate de tenerlo instalado con install.packages("patchwork")
library(dplyr)
library(cowplot)
library(lubridate)

# Visualización general del dataset
head(df)          # Primeras filas
str(df)           # Estructura de las variables
dim(df)           # Dimensiones del dataframe
names(df)         # Nombres de las columnas
summary(df)       # Estadísticas descriptivas

# Conversión de variables categóricas a tipo factor
df$hotel <- as.factor(df$hotel)
df$arrival_date_month <- as.factor(df$arrival_date_month)
df$meal <- as.factor(df$meal)
df$market_segment <- as.factor(df$market_segment)
df$distribution_channel <- as.factor(df$distribution_channel)
df$reserved_room_type <- as.factor(df$reserved_room_type)
df$assigned_room_type <- as.factor(df$assigned_room_type)
df$deposit_type <- as.factor(df$deposit_type)
df$customer_type <- as.factor(df$customer_type)
df$reservation_status <- as.factor(df$reservation_status)
df$required_car_parking_spaces <- as.integer(as.character(df$required_car_parking_spaces))
df$country <- as.factor(df$country)
df$agent <- as.factor(df$agent)
df$company <- as.factor(df$company)
df$is_canceled <- as.factor(df$is_canceled)
df$is_repeated_guest <- as.factor(df$is_repeated_guest)


# Conversión de la fecha a tipo Date
df$reservation_status_date <- as.Date(df$reservation_status_date)

#Unimos las fechas
# Aseguremonos que la fecha sea un numero para poder unirlo
df$arrival_date_day_of_month <- as.numeric(df$arrival_date_day_of_month)
df$arrival_date_year <- as.numeric(df$arrival_date_year)
# Convertir el nombre del mes a número
df$arrival_date_month <- match(df$arrival_date_month, month.name)
# Combinar las columnas de día, mes y año para formar una fecha completa
df$arrival_date <- paste(df$arrival_date_year, df$arrival_date_month, df$arrival_date_day_of_month, sep = "-")
# Convertir el texto a una fecha (tipo Date)
df$arrival_date <- as.Date(df$arrival_date, format = "%Y-%m-%d")
#Eliminamos las columnas arrive inecesarias
df <- subset(df, select = -c(arrival_date_day_of_month))
df <- subset(df, select = -c(arrival_date_year))
df <- subset(df, select = -c(arrival_date_month))
# Verificar fechas inválidas (que dieron NA)
df_with_invalid_dates <- df %>% filter(is.na(arrival_date))
print(df_with_invalid_dates)

#Juntamos el stays_in_weekend_nights y stays_in_week_nights para que sea numero de noches que se quedaron
df$total_nights <- df$stays_in_weekend_nights + df$stays_in_week_nights
df <- subset(df, select = -c(stays_in_weekend_nights))
df <- subset(df, select = -c(stays_in_week_nights))

# Verificación de la estructura después de las conversiones
str(df)

# Eliminación de duplicados
sum(duplicated(df))
df <- df[!duplicated(df), ]


# Comprobación de los resúmenes de las variables categóricas principales
lapply(df[c("reservation_status", "is_canceled", "meal", "is_repeated_guest", 
            "reserved_room_type", "assigned_room_type", "deposit_type", 
            "customer_type", "market_segment", "distribution_channel")], summary)

# Comprobación de la variable de fecha
str(df$reservation_status_date)

# Resumen estadístico básico
summary(df)
str(df)

# Visualizar valores faltantes
aggr(df, numbers = T, sortVar = T)

#Eliminar datos sin valor
data <- read.csv('hotel_bookings.csv', na.strings="",sep=';')
View(data)
aggr(df,numbers=T,sortVar=T)


# Comprobar valores faltantes en todo el conjunto de datos
colSums(is.na(df))

#Porcentaje de valor NA con todos los registros de la columna
colSums(is.na(df)) / nrow(df) * 100

# 1. Calcular el promedio de niños cuando hay exactamente 2 o 3 adultos (ignorando NA)
# 2. Reemplazar valores NA en 'children' cuando hay 2 o 3 adultos con el promedio calculado
mean_children_2_adults <- mean(df$children[df$adults == 2], na.rm = TRUE)
df$children[is.na(df$children) & df$adults == 2] <- mean_children_2_adults
mean_children_3_adults <- mean(df$children[df$adults == 3], na.rm = TRUE)
df$children[is.na(df$children) & df$adults == 3] <- mean_children_3_adults

# Verificar los cambios
summary(df$children)


#Corrección de errores
# Reemplazar por NA a "adultos" 0 porque es muy raro que vayan niños por varios dias sin adultos o bebes sin adultos.
df$adults[df$adults == "0"] <- NA

# Reemplazar "Undefined" por NA en la columna 'meal'
df$meal[df$meal == "Undefined"] <- NA

# Reemplazar "Undefined" por NA en la columna 'distribution_channel'
df$distribution_channel[df$distribution_channel == "Undefined"] <- NA

# Reemplazar "NULL" (como texto) por NA en las columnas 'company' y 'agent'
df$company[df$company == "NULL"] <- NA
df$agent[df$agent == "NULL"] <- NA

# Reemplazar "Undefined" por NA en el market segment
df$market_segment[df$market_segment == "Undefined"] <- NA

#Reemplazar los NULL o "Undefined" por NA en los paises
df$country[df$country == "Undefined"] <- NA
df$country[df$country == "NULL"] <- NA

#Vamos a ver estadisticas para arreglar si vemos algunos datos incoherentes
summary(df) #Notamos que el minimo valor en adr es negativo y eso es incoherente por lo que
   #le pondremos NULL para luego darle otro valor, los datos atipicos vamos a modificarlo
   #luego
df$adr[df$adr <0] <- NA

#Vamos a verificar de nuevo
summary(df)

#Vamos a verificar cuantos valores NA hay
# Comprobar valores faltantes en todo el conjunto de datos
colSums(is.na(df))

#Porcentaje de valor NA con todos los registros de la columna
colSums(is.na(df)) / nrow(df) * 100

#Ahora vamos a arreglar esos valores NA, antes de eliminar company y agent por ser una 
#cantidad grande de porcentaje comparado con el total de datos, vamos a reemplazar los otros NA
# Ver distribución de valores
table(df$meal)
prop.table(table(df$meal)) * 100  # Porcentajes

table(df$distribution_channel)
prop.table(table(df$distribution_channel)) * 100



# Grafico 1, Proporción de meals por hotel
p1 <- ggplot(df, aes(x = hotel, fill = meal)) +
  geom_bar(position = "fill") +
  labs(title = "Por tipo de hotel",
       y = "Proporcion",
       x = "Hotel") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Grafico 2, Proporción de meals por segmento de mercado
p2 <- ggplot(df, aes(x = market_segment, fill = meal)) +
  geom_bar(position = "fill") +
  labs(title = "Por segmento de mercado",
       y = "Proporcion",
       x = "Segmento de mercado") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combinacion
p1 + p2 +
  plot_annotation(
    title = "Distribucion de tipos de 'meal' según caracteristicas del cliente",
    subtitle = "Proporciones relativas por tipo de hotel y segmento de mercado"
  )

# Encontramos la moda de 'meal' (valor más frecuente), ya que hasta con los graficos
#Notamos que el BB es mayoritario y no se nota un patron que no sea el BB mayor
moda_meal <- names(sort(table(df$meal), decreasing = TRUE))[1]
print(moda_meal)  # Esto te va a dar el valor que más aparece

# Reemplazar NAs en 'meal' con la moda (BB)
df$meal[is.na(df$meal)] <- moda_meal
summary(df$meal) #Ahora notamos que ya no hay NA en meal ni undefined


summary(df$distribution_channel) #Hay valores NA en distribution_channel
summary(df$market_segment)#Tambien en  market segment y estos se relacionan
#por lo que trataremos de encontrar un patron con graficos para reemplazar valores NA

# p4: Proporción de distribution_channel por segmento de mercado
p4 <- ggplot(df, aes(x = market_segment, fill = distribution_channel)) +
  geom_bar(position = "fill") +
  labs(title = "Por segmento de mercado",
       y = "Proporción",
       x = "Segmento de mercado") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Paso 1: Convertimos a character para trabajar sin errores
df$distribution_channel <- as.character(df$distribution_channel)

# Paso 3: Calcular la moda de 'distribution_channel' cuando tanto 'distribution_channel' como 'market_segment' son NA
# Calculamos la moda general para distribution_channel
mode_dist_channel <- names(sort(table(df$distribution_channel), decreasing = TRUE))[1]

# Paso 4: Rellenar NA en distribution_channel según el market_segment (o con la moda si ambos son NA)
df$distribution_channel <- ifelse(is.na(df$distribution_channel) & is.na(df$market_segment), mode_dist_channel,
                                  ifelse(is.na(df$distribution_channel) & df$market_segment %in% c("Corporate", "Aviation"), "Corporate",
                                         ifelse(is.na(df$distribution_channel) & df$market_segment %in% c("Direct", "Complementary"), "Direct",
                                                ifelse(is.na(df$distribution_channel) & df$market_segment %in% c("Groups", "Online TA"), "TA/TO",
                                                       df$distribution_channel))))

# Paso 5: Convertimos de vuelta a factor
df$distribution_channel <- as.factor(df$distribution_channel)


# Forzar los valores correctamente
df$distribution_channel <- factor(df$distribution_channel,
                                  levels = c("Corporate", "Direct", "GDS", "TA/TO"))

# Paso 1: Convertimos a character para trabajar sin errores
df$market_segment <- as.character(df$market_segment)

# Paso 3: Calcular la moda de 'market_segment' según la combinación de valores de 'distribution_channel'
# Calculamos la moda general para market_segment
mode_market_segment_corporate <- "Corporate"  # Como sabemos que "Corporate" predomina en "Corporate" y "Aviation"
mode_market_segment_direct <- "Direct"        # "Direct" predomina en "Direct" y "Complementary"
mode_market_segment_ta_to <- "TA/TO"          # "TA/TO" predomina en "Groups" y "Online TA"

# Paso 4: Rellenar los NA en market_segment según distribution_channel
df$market_segment <- ifelse(is.na(df$market_segment) & df$distribution_channel == "Corporate", mode_market_segment_corporate,
                            ifelse(is.na(df$market_segment) & df$distribution_channel == "Direct", mode_market_segment_direct,
                                   ifelse(is.na(df$market_segment) & df$distribution_channel == "TA/TO", mode_market_segment_ta_to,
                                          df$market_segment)))

# Paso 5: Convertimos de vuelta a factor
df$market_segment <- as.factor(df$market_segment)

#Ahora vamos a ver una estadistica para ver lo que nos falta
summary(df)

#Para lo de country no podemos encontrar un patron por ahora por lo que 
   #como es un factor el valor NA será reemplazado por la mayor cantidad de ciudadanos
    #que van al hotel segun el data set, voy a hacer una estadistica de country
summary(df$country)  #El mayor valor es PRT, por lo que reemplazaremos con ese
df$country[is.na(df$country)] <- "PRT"

#Ahora vamos a reemplazar los valores NA de aduts
# Calcular la media redondeada de adults, excluyendo NAs
mean_adults <- round(mean(df$adults, na.rm = TRUE))

# Imputar valores NA en adults con la media
df$adults[is.na(df$adults)] <- mean_adults

# Aplicamos la condición: si no hay niños ni bebés y la reserva fue cancelada, poner adults en 0
df <- df %>%
  mutate(adults = ifelse(children == 0 & babies == 0 & is_canceled == 1, 0, adults))


#Por ultimo le darenos un valor medio al adr para que no afecte a la dataset
df$adr[is.na(df$adr)] <- round(mean(df$adr, na.rm = TRUE))

#Vamos a suponer que las fechas son correctas porque es dificil averiguar si alguno es correcto igual que el lead time
#Arreglamos los errores de reservas como incoherencias
df <- df %>%
  mutate(
    expected_nights = as.numeric(reservation_status_date - arrival_date),
    status_check = case_when(
      reservation_status == "Check-Out" ~ expected_nights == total_nights,
      reservation_status == "No-Show" ~ reservation_status_date == arrival_date,
      reservation_status == "Canceled" ~ expected_nights <= 0,
      TRUE ~ NA
    )
  ) %>%
  # Arreglar casos inconsistentes
  mutate(
    reservation_status = case_when(
      # Si es No-Show, pero la fecha no coincide y arrival es mayor, y sí se canceló → cambiar a "Canceled"
      reservation_status == "No-Show" &
        status_check == FALSE &
        arrival_date > reservation_status_date &
        is_canceled == 1 ~ "Canceled",
      
      TRUE ~ reservation_status
    ),
    # Ajustar total_nights si es Check-Out inconsistente y no fue cancelado
    total_nights = case_when(
      reservation_status == "Check-Out" &
        status_check == FALSE &
        is_canceled == 0 ~ expected_nights,
      
      TRUE ~ total_nights
    )
  )
#Eliminamos las columnas que habiamos creado
df <- subset(df, select = -c(expected_nights))
df <- subset(df, select = -c(status_check))



# Ver el porcentaje de valores faltantes por columna
colSums(is.na(df)) / nrow(df) * 100

#Eliminamos 
df <- subset(df, select = -c(arrival_date_year))
df <- subset(df, select = -c(arrival_date_month))
df <- subset(df, select = -c(arrival_date_day_of_month))


head(df) #Vemos los datos nuevamente para ya poder revisar los datos atipicos
summary(df$lead_time)

#Nos aseguramos que las variables categóricas sean factores para evitar problemas
df$hotel <- as.factor(df$hotel)
df$is_canceled <- as.factor(df$is_canceled)
df$meal <- as.factor(df$meal)
df$country <- as.factor(df$country)
df$market_segment <- as.factor(df$market_segment)
df$distribution_channel <- as.factor(df$distribution_channel)
df$is_repeated_guest <- as.factor(df$is_repeated_guest)
df$reservation_status <- as.factor(df$reservation_status)
df$deposit_type <- as.factor(df$deposit_type)
df$customer_type <- as.factor(df$customer_type)
df$reserved_room_type <- as.factor(df$reserved_room_type)
df$assigned_room_type <- as.factor(df$assigned_room_type)
#Convertir la fecha correctamente
df$reservation_status_date <- as.Date(df$reservation_status_date, format="%Y-%m-%d")
df$arrival_date <- as.Date(df$arrival_date, format="%Y-%B-%d")



#Vamos a crear primero para los valores numericos graficas de histograma y boxplots para 
#poder identificar los valores atipicos
summary(df) #Visualizamos las estadisticas para ver que todo este bien
p5 <- ggplot(df, aes(x = lead_time)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$lead_time, na.rm = TRUE), 
                                         sd = sd(df$lead_time, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: lead_time", subtitle = "Con curva normal")


b5 <- ggplot(df, aes(x = lead_time)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot: lead_time") +
  theme_classic()


d1 <- (p5 | b5) 
  plot_annotation(
    title = "Análisis de outliers en del lead time",
    subtitle = "Distribución y valores atípicos de lead_time",
    caption = "Estado: con sueñoo, quiero dormir"
  ) #Para el lead time si vamos a reemplazar los valores atipicos porque esos dias de reserva son demasiado largos
  
  
  
#Visualización de datos atipicos
#lead_time
outliers<-boxplot(df$lead_time,plot=FALSE)$out
outliers

# Calcular el IQR para la variable 'lead_time'
Q1 <- quantile(df$lead_time, 0.25)
Q3 <- quantile(df$lead_time, 0.75)
IQR <- Q3 - Q1
# Limites inferior y superior
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Identificar datos atípicos
outliers <- df$lead_time[df$lead_time < lower_bound | df$lead_time > upper_bound
]
outliers



#Segunda parte de  adults, childre

p9 <- ggplot(df, aes(x = adults)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "salmon", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$adults, na.rm = TRUE), 
                                         sd = sd(df$adults, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: adults", subtitle = "Con curva normal")

p10 <- ggplot(df, aes(x = children)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "turquoise3", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$children, na.rm = TRUE), 
                                         sd = sd(df$children, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: children", subtitle = "Con curva normal")

b9 <- ggplot(df, aes(x = adults)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Boxplot: adults") +
  theme_classic()

b10 <- ggplot(df, aes(x = children)) +
  geom_boxplot(fill = "turquoise3") +
  labs(title = "Boxplot: children") +
  theme_classic()
d2 <- 
  (p9 | b9) /
  (p10 | b10) +
  plot_annotation(
    title = "Análisis de outliers (Parte 2)",
    subtitle = "Distribución y valores atípicos de stays_in_week_nights, adults y children",
    
  ) #Notamos que hay muchos adultos en "0" pero muchos se deben porque cancelaron por lo que
   #adultos lo dejamos como esta pero los niños vemos un valor de casi 10 niños por lo que sí es 
   #es un valor atipico y lo vamos a reemplazar  por lo que no afectará para después analizar porqyue
   #ese casi 10 sí podria afectarnos si buscamos maximo


#Visualización de datos atipicos
#adults
outliers<-boxplot(df$adults,plot=FALSE)$out
outliers
#children
outliers<-boxplot(df$children,plot=FALSE)$out
outliers


#Parte 3 babies, previous_cancellations y previous_bookings_not_canceled.
p11 <- ggplot(df, aes(x = babies)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "plum", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$babies, na.rm = TRUE),
                                         sd = sd(df$babies, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: babies", subtitle = "Distribución + curva normal")

p12 <- ggplot(df, aes(x = previous_cancellations)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "khaki3", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$previous_cancellations, na.rm = TRUE),
                                         sd = sd(df$previous_cancellations, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: previous_cancellations", subtitle = "Distribución + curva normal")

p13 <- ggplot(df, aes(x = previous_bookings_not_canceled)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightskyblue4", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$previous_bookings_not_canceled, na.rm = TRUE),
                                         sd = sd(df$previous_bookings_not_canceled, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: previous_bookings_not_canceled", subtitle = "Distribución + curva normal")

b11 <- ggplot(df, aes(x = babies)) +
  geom_boxplot(fill = "plum") +
  labs(title = "Boxplot: babies") +
  theme_classic()

b12 <- ggplot(df, aes(x = previous_cancellations)) +
  geom_boxplot(fill = "khaki3") +
  labs(title = "Boxplot: previous_cancellations") +
  theme_classic()

b13 <- ggplot(df, aes(x = previous_bookings_not_canceled)) +
  geom_boxplot(fill = "lightskyblue4") +
  labs(title = "Boxplot: previous_bookings_not_canceled") +
  theme_classic()
d3 <- (p11 | b11) /
  (p12 | b12) /
  (p13 | b13) +
  plot_annotation(
    title = "Análisis de outliers (Parte 3)",
    subtitle = "Distribuciones: bebés, cancelaciones y reservas previas no canceladas",
  ) #Viendo el grafico vamos a modificar algu8nos valores atipcios en babies porque es muy atipico que
    #alguien vaya con casi 10 bebes, lo de  previous_cancelllations y previous_bookings_not_canceled son numeros
    #posibles y comunes por lo que lo dejaremos tal cual

#Visualización de datos atipicos
#babies
outliers<-boxplot(df$babies,plot=FALSE)$out
outliers
#previous_cancellations
outliers<-boxplot(df$previous_cancellations,plot=FALSE)$out
outliers
#previous_bookings_not_canceled
outliers<-boxplot(df$previous_bookings_not_canceled,plot=FALSE)$out
outliers


#Pate 4, booking_changes, days_in_waiting_list y adr
p14 <- ggplot(df, aes(x = booking_changes)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "lightgreen", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$booking_changes, na.rm = TRUE),
                                         sd = sd(df$booking_changes, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: booking_changes", subtitle = "Distribución + curva normal")

p15 <- ggplot(df, aes(x = days_in_waiting_list)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "salmon", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$days_in_waiting_list, na.rm = TRUE),
                                         sd = sd(df$days_in_waiting_list, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: days_in_waiting_list", subtitle = "Distribución + curva normal")

p16 <- ggplot(df, aes(x = adr)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$adr, na.rm = TRUE),
                                         sd = sd(df$adr, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: adr", subtitle = "Distribución + curva normal")
b14 <- ggplot(df, aes(x = booking_changes)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot: booking_changes") +
  theme_classic()

b15 <- ggplot(df, aes(x = days_in_waiting_list)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Boxplot: days_in_waiting_list") +
  theme_classic()

b16 <- ggplot(df, aes(x = adr)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot: adr") +
  theme_classic()
d4 <- (p14 | b14) /
  (p15 | b15) /
  (p16 | b16) +
  plot_annotation(
    title = "Análisis de outliers (Parte 4)",
    subtitle = "booking_changes, days_in_waiting_list y adr"
  ) #Aqui notamos que sí puede haber como 30 cambios en la reserva pero es un valor muy atipico
    #y eso afectaria en nuestras estadisticas por lo que podemos modificar los valores atipcios
    #En days in waiting list lo normal es hasta 180 días pero después ya es muy raro, por lo que
    #Tambien podemos modificar lkos valores atipicos
    #Y en adr notamos en el gragico un punto muy al extremo derecho por lo que eso sí es probablemente un error por lo que lo reemplazaremos


#Visualización de datos atipicos
#booking_changes
outliers<-boxplot(df$booking_changes,plot=FALSE)$out
outliers
#days_in_waiting_list
outliers<-boxplot(df$days_in_waiting_list,plot=FALSE)$out
outliers
#adr
outliers<-boxplot(df$adr,plot=FALSE)$out
outliers

#Parte 5, required_car_parking_spaces y total_of_special_requests
p17 <- ggplot(df, aes(x = required_car_parking_spaces)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "orchid", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$required_car_parking_spaces, na.rm = TRUE),
                                         sd = sd(df$required_car_parking_spaces, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: required_car_parking_spaces", y= 'conteos', subtitle = "Distribución + curva normal")

p18 <- ggplot(df, aes(x = total_of_special_requests)) +
  geom_histogram(aes(y = ..density..), bins = 6, fill = "orange", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$total_of_special_requests, na.rm = TRUE),
                                         sd = sd(df$total_of_special_requests, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: total_of_special_requests", subtitle = "Distribución + curva normal")

p8 <- ggplot(df, aes(x = total_nights)) +
  geom_histogram(aes(y = ..density..), bins = 6, fill = "blue", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(df$total_nights, na.rm = TRUE),
                                         sd = sd(df$total_nights, na.rm = TRUE)), color = "red") +
  labs(title = "Histograma: total_nights", subtitle = "Distribución + curva normal")


b17 <- ggplot(df, aes(x = required_car_parking_spaces)) +
  geom_boxplot(fill = "orchid") +
  labs(title = "Boxplot: required_car_parking_spaces") +
  theme_classic()

b18 <- ggplot(df, aes(x = total_of_special_requests)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot: total_of_special_requests") +
  theme_classic()

b8 <- ggplot(df, aes(x = total_nights)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Boxplot: total_nights") +
  theme_classic()

d5 <- (p17 | b17) /
  (p18 | b18) /
  (p8| b8) +
  plot_annotation(
    title = "Análisis de outliers (Parte 5)",
    subtitle = "required_car_parking_spaces, total_of_special_requests y total_nights"

  ) #notamos segun el grafico que hay algunos puntos muy atipicos en required_car_parking_spaces
    #por lo que lo vamos a reemplazar y en total_of_special_requests no porque la diferencia no es mucha
    #en total_nights sí notamos algunos puntos muy a la derecha por lo que vamos a reemplzar sus valores
    #atipicos

#Visualización de datos atipicos
#required_car_parking_spaces
outliers<-boxplot(df$required_car_parking_spaces,plot=FALSE)$out
outliers
#total_of_special_requests
outliers<-boxplot(df$total_of_special_requests,plot=FALSE)$out
outliers
#total_nights
outliers<-boxplot(df$total_nights,plot=FALSE)$out
outliers

#Ahora vamos a graficar los factores pero con barras simples
# Gráfico para 'hotel'
p_hotel <- ggplot(df, aes(x = hotel)) +
  geom_bar(fill = "lightskyblue3") +
  labs(title = "Frecuencia: Hotel", x = "Tipo de hotel", y = "Frecuencia") +
  theme_minimal()

# Gráfico para 'is_canceled'
p_cancel <- ggplot(df, aes(x = factor(is_canceled))) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Frecuencia: Cancelaciones", x = "¿Cancelado?", y = "Frecuencia") +
  theme_minimal()

# Gráfico para 'meal'
p_meal <- ggplot(df, aes(x = meal)) +
  geom_bar(fill = "mediumseagreen") +
  labs(title = "Frecuencia: Meal", x = "Tipo de comida", y = "Frecuencia") +
  theme_minimal()

# Combinar los 3 en d6
d6 <- (p_hotel | p_cancel | p_meal) +
  plot_annotation(
    title = "Distribución de Variables Categóricas",
    subtitle = "Hotel, Cancelaciones y Tipos de Comida",

  )

# Mostrar d6
d6 #Notamos que el unico valor atipico es el FB en meal pero solo son 4 categoricos


# Gráfico para 'country'
p_country <- ggplot(df, aes(x = country)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frecuencia: País", x = "País", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Oculta las etiquetas si hay muchas

# Gráfico para 'market_segment'
p_market <- ggplot(df, aes(x = market_segment)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Frecuencia: Segmento de Mercado", x = "Segmento", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico para 'distribution_channel'
p_channel <- ggplot(df, aes(x = distribution_channel)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Frecuencia: Canal de Distribución", x = "Canal", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico para 'is_repeated_guest'
p_repeat <- ggplot(df, aes(x = factor(is_repeated_guest))) +
  geom_bar(fill = "purple") +
  labs(title = "Frecuencia: ¿Huésped Repetido?", x = "Repetido", y = "Frecuencia") +
  theme_minimal()

# Combinar los 4 en d7
d7 <- (p_country | p_market) / (p_channel | p_repeat) +
  plot_annotation(
    title = "Distribución de Variables Categóricas",
    subtitle = "País, Segmento de Mercado, Canal y Huéspedes Repetidos"
  )

# Mostrar d7
d7  # Aqui notamos que segun el grafico en pais hay demasiado ruido por lo que sí reemplazaremos los
    #valores atipicos o lo reagruparemos, igual en canal de distribución ya que no afectaria mucho
     #reemplazarlo en la estadictica y en segmento de mercadi los cvalores muy pequeños tambien serán atipicos
     #En Frecuencia no es necesario porque solo son dos valores


#reserved_room_type, assigned_room_type, deposit_type, customer_type

# Gráfico para 'reserved_room_type'
p_reserved <- ggplot(df, aes(x = reserved_room_type)) +
  geom_bar(fill = "tomato") +
  labs(title = "Frecuencia: Tipo de Habitación Reservada", x = "Reservado", y = "Frecuencia") +
  theme_minimal()

# Gráfico para 'assigned_room_type'
p_assigned <- ggplot(df, aes(x = assigned_room_type)) +
  geom_bar(fill = "dodgerblue") +
  labs(title = "Frecuencia: Tipo de Habitación Asignada", x = "Asignado", y = "Frecuencia") +
  theme_minimal()

# Gráfico para 'deposit_type'
p_deposit <- ggplot(df, aes(x = deposit_type)) +
  geom_bar(fill = "orchid") +
  labs(title = "Frecuencia: Tipo de Depósito", x = "Depósito", y = "Frecuencia") +
  theme_minimal()

# Gráfico para 'customer_type'
p_customer <- ggplot(df, aes(x = customer_type)) +
  geom_bar(fill = "seagreen") +
  labs(title = "Frecuencia: Tipo de Cliente", x = "Cliente", y = "Frecuencia") +
  theme_minimal()

# Combinar los 4 en d8
d8 <- (p_reserved | p_assigned) / (p_deposit | p_customer) +
  plot_annotation(
    title = "Distribución de Variables Categóricas",
    subtitle = "Habitaciones, Depósitos y Tipos de Cliente"
  )

# Mostrar d8
d8  #En tipo de habitación reservada vamos a poner los datos menores en una sola categoria para eliminar el ruido
   #En habitacion asignado también vamos a hacer lo mismo que habitaacion reservada
    #En tipo de deposito no es necesario porque solo son 3 categorias
   #En cliente tampoco lo veo necesario porque solo son 4 categorias



# Gráfico 1: Estado de reserva
p_status <- ggplot(df, aes(x = reservation_status)) +
  geom_bar(fill = "plum") +
  labs(title = "Frecuencia: Estado de Reserva", x = "Estado", y = "Cantidad") +
  theme_minimal()

# Gráfico 2: Fechas de cambio de estado
p_status_date <- df %>%
  count(reservation_status_date) %>%
  ggplot(aes(x = reservation_status_date, y = n)) +
  geom_line(color = "tomato") +
  labs(title = "Cambios de Estado a lo Largo del Tiempo", x = "Fecha", y = "Cantidad de Cambios") +
  theme_minimal()

# Gráfico 3: Fechas de llegada
p_arrival <- df %>%
  count(arrival_date) %>%
  ggplot(aes(x = arrival_date, y = n)) +
  geom_line(color = "darkgreen") +
  labs(title = "Llegadas por Día", x = "Fecha de Llegada", y = "Cantidad de Reservas") +
  theme_minimal()

# Combinar los tres gráficos
d9 <- (p_status | p_status_date | p_arrival) +
  plot_annotation(
    title = "Distribución Temporal y Estados de Reserva",
    subtitle = "Estado, Fecha de cambio y Fecha de llegada"
  )

# Mostrar todo junto
d9 #Aqui no veo valores atipcios que sea necesario reemplazar

#Eliminación de atipicos
df_clean<-df
num_cols <- sapply(df_clean, is.numeric)


# Lista de variables numéricas
#Vamos a comenzar con lead_time
d1
# Definir límites del 1% y 99%
lower_bound <- quantile(df$lead_time, 0.00)
upper_bound <- quantile(df$lead_time, 0.99)

# Calcular media del lead_time (solo dentro del rango "normal")
media <- mean(df$lead_time[df$lead_time >= lower_bound & df$lead_time <= upper_bound], na.rm = TRUE)

# Reemplazar outliers por la media
df$lead_time_clean <- ifelse(df$lead_time < lower_bound | df$lead_time > upper_bound,
                             media,
                             df$lead_time)
# Visualizar antes y después
par(mfrow = c(1, 2))
boxplot(df$lead_time, main = "con outliers", col = 3)
boxplot(df$lead_time_clean, main = "outliers -> media", col = 2)

#Ahora vamos por children
d2
# Definir límites del 1% y 99%
upper_bound <- quantile(df$children, 0.9999, na.rm = TRUE)

# Calcular la media solo de los valores dentro del rango aceptable
media <- mean(df$children[df$children <= upper_bound], na.rm = TRUE)

# Reemplazar solo los valores mayores al límite por la media
df$children_clean <- ifelse(df$children > upper_bound, media, df$children)

# Visualizar antes y después
par(mfrow = c(1, 2))
boxplot(df$children, main = "con outliers", col = 3)
boxplot(df$children_clean, main = "outliers -> media", col = 2)

# Ver resumen
summary(df$children)
summary(df$children_clean)



#Ahora vamos con days in waiting list
# Definir límites del 1% y 99%
# Definir el límite superior (percentil 99)
upper_bound <- quantile(df$days_in_waiting_list, 0.999, na.rm = TRUE)

# Calcular la mediana de los valores dentro del rango aceptable
mediana <- median(df$days_in_waiting_list[df$days_in_waiting_list <= upper_bound], na.rm = TRUE)

# Reemplazar valores mayores al límite por la mediana
df$days_in_waiting_list_clean <- ifelse(df$days_in_waiting_list > upper_bound, mediana, df$days_in_waiting_list)

# Visualizar antes y después
par(mfrow = c(1, 2))
boxplot(df$days_in_waiting_list, main = "con outliers", col = 3)
boxplot(df$days_in_waiting_list_clean, main = "outliers -> mediana", col = 2)

# Ver resumen
summary(df$days_in_waiting_list)
summary(df$days_in_waiting_list_clean)


#Ahora vamos con ADR
# Definir el límite superior (percentil 99)
upper_bound <- quantile(df$adr, 0.99999, na.rm = TRUE)

# Calcular la mediana de los valores dentro del rango aceptable
mediana <- median(df$adr[df$adr <= upper_bound], na.rm = TRUE)

# Reemplazar valores mayores al límite por la mediana
df$adr_clean <- ifelse(df$adr > upper_bound, mediana, df$adr)

# Visualizar antes y después
par(mfrow = c(1, 2))
boxplot(df$adr, main = "con outliers", col = 3)
boxplot(df$adr_clean, main = "outliers -> mediana", col = 2)

# Ver resumen
summary(df$adr)
summary(df$adr_clean)


#Ahora vamos por booking changes
summary(df$booking_changes)

# Definir el límite superior (percentil 99)
upper_bound <- quantile(df$booking_changes, 0.9999, na.rm = TRUE)

# Calcular la media de los valores dentro del rango aceptable
media <- mean(df$booking_changes[df$booking_changes <= upper_bound], na.rm = TRUE)

# Reemplazar los valores mayores al límite por la media
df$booking_changes_clean <- ifelse(df$booking_changes > upper_bound, media, df$booking_changes)

# Visualizar antes y después
par(mfrow = c(1, 2))
boxplot(df$booking_changes, main = "con outliers", col = 3)
boxplot(df$booking_changes_clean, main = "outliers -> media", col = 2)

# Ver resumen
summary(df$booking_changes)
summary(df$booking_changes_clean)


#Ahora con required_car_parking_spaces
summary(df$booking_changes)
# Definir el límite superior (percentil 99)
upper_bound <- quantile(df$required_car_parking_spaces, 0.9999, na.rm = TRUE)

# Calcular la media de los valores dentro del rango aceptable
media <- mean(df$required_car_parking_spaces[df$required_car_parking_spaces <= upper_bound], na.rm = TRUE)

# Reemplazar los valores mayores al límite por la media
df$required_car_parking_spaces_clean <- ifelse(df$required_car_parking_spaces > upper_bound, 
                                               media, 
                                               df$required_car_parking_spaces)

# Visualizar antes y después
par(mfrow = c(1, 2))
boxplot(df$required_car_parking_spaces, main = "con outliers", col = 3)
boxplot(df$required_car_parking_spaces_clean, main = "outliers -> media", col = 2)

# Ver resumen
summary(df$required_car_parking_spaces)
summary(df$required_car_parking_spaces_clean)


#Ahora con los categoricos
#
# Definir el umbral (porcentaje)
umbral <- 0.02  # Esto significa el 1%

# Calcular la frecuencia de cada categoría
categoria_count <- table(df$reserved_room_type)

# Calcular el porcentaje de cada categoría
categoria_percent <- prop.table(categoria_count)

# Crear un vector lógico donde las categorías que tengan un porcentaje menor al umbral se agruparán en "Other"
df$reserved_room_type_clean <- df$reserved_room_type

# Añadir el nivel "Other" a los factores de la columna
levels(df$reserved_room_type_clean) <- c(levels(df$reserved_room_type_clean), "Other")

# Reemplazar las categorías menos frecuentes por "Other"
df$reserved_room_type_clean[df$reserved_room_type %in% names(categoria_percent[categoria_percent < umbral])] <- "Other"

# Ver el resumen de la columna original y la nueva
summary(df$reserved_room_type)
summary(df$reserved_room_type_clean)

# Ver la distribución de categorías
table(df$reserved_room_type_clean)
# Gráfico para la columna modificada (con "Other")
p_reserved_clean <- ggplot(df, aes(x = reserved_room_type_clean)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Frecuencia: Tipo de Habitación Reservada - Modificado (con 'Other')", 
       x = "Reservado", y = "Frecuencia") +
  theme_minimal()

# Usamos gridExtra para mostrar ambos gráficos en una sola fila
library(gridExtra)
grid.arrange(p_reserved, p_reserved_clean, ncol = 2)

#Ahora vamos con assigned_room_type
# Definir el umbral (porcentaje)
umbral <- 0.02  # Esto significa el 2%

# Calcular la frecuencia de cada categoría en assigned_room_type
categoria_count_assigned <- table(df$assigned_room_type)

# Calcular el porcentaje de cada categoría en assigned_room_type
categoria_percent_assigned <- prop.table(categoria_count_assigned)

# Crear un vector lógico donde las categorías que tengan un porcentaje menor al umbral se agruparán en "Other"
df$assigned_room_type_clean <- df$assigned_room_type

# Añadir el nivel "Other" a los factores de la columna
levels(df$assigned_room_type_clean) <- c(levels(df$assigned_room_type_clean), "Other")

# Reemplazar las categorías menos frecuentes por "Other"
df$assigned_room_type_clean[df$assigned_room_type %in% names(categoria_percent_assigned[categoria_percent_assigned < umbral])] <- "Other"

# Ver el resumen de la columna original y la nueva
summary(df$assigned_room_type)
summary(df$assigned_room_type_clean)

# Ver la distribución de categorías
table(df$assigned_room_type_clean)

# Gráfico para la columna original (assigned_room_type)
p_assigned_original <- ggplot(df, aes(x = assigned_room_type)) +
  geom_bar(fill = "tomato") +
  labs(title = "Frecuencia: Tipo de Habitación Asignada - Original", 
       x = "Asignado", y = "Frecuencia") +
  theme_minimal()

# Gráfico para la columna modificada (con "Other")
p_assigned_clean <- ggplot(df, aes(x = assigned_room_type_clean)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Frecuencia: Tipo de Habitación Asignada - Modificado (con 'Other')", 
       x = "Asignado", y = "Frecuencia") +
  theme_minimal()

# Usamos gridExtra para mostrar ambos gráficos en una sola fila
grid.arrange(p_assigned_original, p_assigned_clean, ncol = 2)

#Ahora vamos con country
# Definir el umbral (porcentaje)
umbral <- 0.02  # Esto significa el 2%

# Calcular la frecuencia de cada categoría en country
categoria_count_country <- table(df$country)

# Calcular el porcentaje de cada categoría en country
categoria_percent_country <- prop.table(categoria_count_country)

# Crear un vector lógico donde las categorías que tengan un porcentaje menor al umbral se agruparán en "Other"
df$country_clean <- df$country

# Añadir el nivel "Other" a los factores de la columna
levels(df$country_clean) <- c(levels(df$country_clean), "Other")

# Reemplazar las categorías menos frecuentes por "Other"
df$country_clean[df$country %in% names(categoria_percent_country[categoria_percent_country < umbral])] <- "Other"

# Ver el resumen de la columna original y la nueva
summary(df$country)
summary(df$country_clean)

# Ver la distribución de categorías
table(df$country_clean)

# Gráfico para la columna original (country)
p_country_original <- ggplot(df, aes(x = country)) +
  geom_bar(fill = "tomato") +
  labs(title = "Frecuencia: País - Original", 
       x = "País", y = "Frecuencia") +
  theme_minimal()

# Gráfico para la columna modificada (con "Other")
p_country_clean <- ggplot(df, aes(x = country_clean)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Frecuencia: País - Modificado (con 'Other')", 
       x = "País", y = "Frecuencia") +
  theme_minimal()

# Usamos gridExtra para mostrar ambos gráficos en una sola fila
grid.arrange(p_country_original, p_country_clean, ncol = 2)



#Ahora por ultimo market_segment
# Definir el umbral (porcentaje)
umbral <- 0.02  # Esto significa el 2%

# Calcular la frecuencia de cada categoría en market_segment
categoria_count_segment <- table(df$market_segment)

# Calcular el porcentaje de cada categoría en market_segment
categoria_percent_segment <- prop.table(categoria_count_segment)

# Crear un vector lógico donde las categorías que tengan un porcentaje menor al umbral se agruparán en "Other"
df$market_segment_clean <- df$market_segment

# Añadir el nivel "Other" a los factores de la columna
levels(df$market_segment_clean) <- c(levels(df$market_segment_clean), "Other")

# Reemplazar las categorías menos frecuentes por "Other"
df$market_segment_clean[df$market_segment %in% names(categoria_percent_segment[categoria_percent_segment < umbral])] <- "Other"

# Ver el resumen de la columna original y la nueva
summary(df$market_segment)
summary(df$market_segment_clean)

# Ver la distribución de categorías
table(df$market_segment_clean)

# Gráfico para la columna original (market_segment)
p_segment_original <- ggplot(df, aes(x = market_segment)) +
  geom_bar(fill = "tomato") +
  labs(title = "Frecuencia: Segmento de Mercado - Original", 
       x = "Segmento de Mercado", y = "Frecuencia") +
  theme_minimal()

# Gráfico para la columna modificada (con "Other")
p_segment_clean <- ggplot(df, aes(x = market_segment_clean)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Frecuencia: Segmento de Mercado - Modificado (con 'Other')", 
       x = "Segmento de Mercado", y = "Frecuencia") +
  theme_minimal()

# Usamos gridExtra para mostrar ambos gráficos en una sola fila
grid.arrange(p_segment_original, p_segment_clean, ncol = 2)

