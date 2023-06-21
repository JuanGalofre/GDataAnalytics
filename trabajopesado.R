library("lubridate")
#Cargar todos los nombres de los archivos a una sola lista.
filenames<-list.files(path="./GDA_Study_case",pattern="csv", full.names=TRUE)
print(filenames)
for (i in filenames){
  #Genero el nombre de la base de datos, basado en el año y en el mes. Servirá
  #para después ponerlo con assign(variable_name,value) y que no se cambie cuando
  #se estudie otra base de datos
  nombre_datos<-(substring(i,18,23))
  
  
  #Se lee el csv
  dataBase<- read.csv(i)
  
  
  #Se remueven duplicados, si es que hay
  dataBase<-distinct(dataBase)
  
  
  #Se analizan nulls. Se encuentra que hay lat y lon que no tienen datos.Se
  #encuentra que hay nulls en end_lat.Se esperaría que como no hay lat, no
  #hay lng.
  apply(is.na(dataBase), 2, which)
  
  
  #Filtrar para ver que se puede hacer frente a estos NAs.Si esta el nombre
  #de la estación, se pueden encontrar las coordenadas y poneras en end_lat
  #na_s<-filter(dataBase,is.na(end_lat)|is.na(end_lng))
  
  
  #Verificar los numbres de las estaciones que no tienen coordenadas.Se encuentra
  #tambien sin nombre. 
  #na_s$end_station_name
  
  
  #Se flitra la base de datos para omitir estos valores.
  dataBase<-filter(dataBase,end_station_name!="" & (!is.na(end_lat)&!is.na(end_lng)))
  
  
  #Como se vió que habian valores "" en end_station_name, se buscan valores
  #asi en start_station_name
  #nameless_coordinates<-filter(dataBase,start_station_name=="")
  
  
  
  #Una vez recuperados los datos que se pudieron recuperar frente al incio de 
  #las estaciones, entonces se procede a filtrar la base de datos para que algún
  #dato que no haya podido ser recuperado se filtre.
  
  dataBase<-filter(dataBase,start_station_name!=""&!is.na(start_station_name)) %>% 
    filter(end_station_name!=""&!is.na(end_station_name))
  
  
  #Se organiza la base de datos.
  arrange(dataBase,member_casual)
  
  
  #Se agrega la distancia del recorrido
  dataBase<- rowwise(dataBase)%>% 
    mutate(
      distance=distm(c(start_lng, start_lat), c(end_lng, end_lat), fun = distHaversine)[1,]
    )
  
  
  #Se agrega la duración del recorrido
  dataBase_end_times<- (hour(dataBase$ended_at)*60)+minute(dataBase$ended_at)+(second(dataBase$ended_at)/60)
  dataBase_start_times<- (hour(dataBase$started_at)*60)+minute(dataBase$started_at)+(second(dataBase$started_at)/60)
  dataBase_minutes <-abs(dataBase_end_times-dataBase_start_times)
  dataBase["Tiempo"]<-dataBase_minutes
  
  
  #Se filtran los viajes que no tienen sentido por la duración del viaje
  dataBase<-filter(dataBase, Tiempo>1,Tiempo<500, !is.na(Tiempo))
  
  
  #Analisis de Estaciones frecuentadas
  mensual_start_estaciones_frecuentes<-group_by(dataBase,start_station_name,member_casual)%>%
    count(start_station_name)
  mensual_end_estaciones_frecuentes<-group_by(dataBase,end_station_name,member_casual)%>%
    count(end_station_name) 
  mensual_estaciones_frecuentes<-rbind(mensual_start_estaciones_frecuentes,mensual_end_estaciones_frecuentes)
  mensual_estaciones_frecuentes[is.na(mensual_estaciones_frecuentes)] <-""
  mensual_estaciones_frecuentes<-unite(mensual_estaciones_frecuentes,col="stations",c("start_station_name","end_station_name"),sep="",remove=TRUE)
  mensual_estaciones_frecuentes<-group_by(mensual_estaciones_frecuentes,stations,member_casual) %>%  
    summarise(total_n=sum(n))
  nombre_lista<-paste0("estaciones_",nombre_datos)
  assign(nombre_lista,mensual_estaciones_frecuentes)

  #Analisis de tipos de bicicletas usadas
  mensual_common_bike_types <-group_by(dataBase,rideable_type,member_casual) %>%  
    count(rideable_type)
  nombre_lista<-paste0("bike_types_",nombre_datos)
  assign(nombre_lista,mensual_common_bike_types)
  
  #Analisis de tiempo por tipo de cicla y miembro
  mensual_bikes_time <-group_by(dataBase,rideable_type,member_casual)%>%  
    count(Tiempo)
  nombre_lista<-paste0("bike_time_",nombre_datos)
  assign(nombre_lista,mensual_bikes_time)
  
  
  N_viajes<-nrow(dataBase)
  nombre_lista<-paste0("Numero_viajes",nombre_datos)
  assign(nombre_lista,N_viajes)
}





#Como se encontraron estos valores, pero tienen coordenadas, se va a intentar
#ver si las coordenadas que tienen corresponden a alguna de la estación que
#se encuentra en la base de datos. Si no, se eliminará, ya que el servicio va
#de estación a estación.
#Cuando se hace un for despues de una base de datos, lo que R toma es que se
#va a iterar sobre las columnas, no sobre las filas. Por lo que se tiene que 
#añadir el seq_le. Ahora el i representara el numero de la fila.

for (i in seq_len(nrow(nameless_coordinates))) {
  id <- nameless_coordinates[i, "ride_id"]
  lng <- nameless_coordinates[i, "start_lng"]
  lat <- nameless_coordinates[i, "start_lat"]
  
  for (j in seq_len(nrow(locations_database))) {
    if (between(lng, locations_database[j, "Max_lng"], locations_database[j, "Min_lng"]) &&
        between(lat, locations_database[j, "Max_lat"], locations_database[j, "Min_lat"])) {
      rows_to_update <- which(dataBase$ride_id == id)
      dataBase$start_station_name[rows_to_update] <- locations_database[j, "Nombre"]
    }
  }
}
