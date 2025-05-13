# 1ACC0216-TB1-2025-1
# Contenido

- [🎯 Objetivo del trabajo](#-objetivo-del-trabajo)
- [👥 Integrantes del grupo](#-integrantes-del-grupo)
- [📊 Descripción del dataset](#-descripción-del-dataset)
- [Ver informe completo en PDF](./upc-4-tb1.pdf)
- [✅ Conclusiones](#-conclusiones)
- [🔐 Licencia](#-licencia)

## 🎯 Objetivo del trabajo
Realizar un análisis exploratorio de datos (EDA) utilizando RStudio, con el fin de identificar patrones, realizar limpieza de datos y generar visualizaciones a partir del conjunto de datos modificado de reservas hoteleras.
Se debe limpiar la data para responder las sibguientes preguntas:
¿Cuántas reservas se realizan por tipo de hotel? ¿Qué tipo de hotel prefiere la gente?
▪ ¿Está aumentando la demanda con el tiempo?
▪ ¿Cuáles son las temporadas de reservas (alta, media, baja)?
▪ ¿Cuál es la duración promedio de las estancias por tipo de hotel?
▪ ¿Cuántas reservas incluyen niños y/o bebés?
▪ ¿Es importante contar con espacios de estacionamiento?
▪ ¿En qué meses del año se producen más cancelaciones de reservas?
▪ Plantear una pregunta del equipo


## 👥 Integrantes del grupo
Cahuana López, Leicy Cristell (U20231E777) 
Huamán Cortez, Anabella Karina (U202216171) 
Mercado De La Rosa, Luis Marcelo (U20211B656) 
Montenegro López, Valentina Étoile (U202312021) 

## 📊 Descripción del dataset
Este conjunto de datos contiene información de reservas realizadas en dos tipos de hoteles ubicados en Portugal: un hotel de ciudad y un resort. Los datos fueron proporcionados originalmente a través de la plataforma Kaggle y luego modificados por los docentes del curso para incluir ruido (datos faltantes y outliers) con fines académicos.

Se incluyen tres versiones:
- `hotel_bookings.csv`: versión original proporcionada por la docente.
- `hotel_bookings_original.csv`: versión limpia con los valores atipicos.
- `hotel_bookings_limpio.csv`: versión preprocesada con tratamiento de NA y valores atípicos.
Tenemos dos versiones finales de hotel_bookings porque para algunos casos nos sirviria poder analizar con los datos atipicos y otros sin los datos atipicos.

## ✅ Conclusiones
1. **Preferencia por el City Hotel:** La mayoría de los clientes prefiere el *City Hotel*, que concentra una mayor cantidad de reservas frente al *Resort Hotel*. Esto puede deberse a su ubicación, accesibilidad o servicios ofrecidos.
2. **Tendencia de crecimiento:** Entre 2015 y 2017 se observó un aumento sostenido en las reservas, alcanzando su punto máximo en 2016. Aunque 2017 tuvo una ligera baja, los niveles se mantuvieron por encima de 2015, evidenciando una tendencia general positiva.
3. **Temporadas de demanda:** Agosto es el mes con mayor cantidad de reservas, consolidándose como temporada alta. Por el contrario, enero y diciembre presentan una baja actividad, siendo considerados temporada baja.
4. **Duración de las estancias:** La mayoría de los huéspedes se alojan entre una y dos noches, especialmente durante la semana, lo cual podría estar relacionado con viajes cortos o estadías laborales.
5. **Composición de los grupos:** La mayoría de las reservas no incluye niños ni bebés, lo que sugiere un perfil mayoritariamente adulto. Aun así, se recomienda ofrecer algunos servicios para familias.
6. **Uso de estacionamiento:** La mayoría de las reservas no requiere estacionamiento, lo que podría indicar que muchos huéspedes no usan vehículo propio o que los hoteles se encuentran en zonas céntricas con buena conectividad.
7. **Cancelaciones:** Agosto también fue el mes con mayor número de cancelaciones, posiblemente por sobreofertas, cambios de planes o estacionalidad. A lo largo del año, el *City Hotel* mantiene un volumen constante y superior de reservas respecto al *Resort Hotel*.

## 🔐 Licencia
Este proyecto está licenciado bajo la **Licencia UPC**, lo que no permite su uso, copia, modificación, distribución y comercialización, con o sin modificacionees.

### 📄 UPC Licensea para uso exclusivo en el curso 1ACC0216 - Fundamentos de Data Science.
