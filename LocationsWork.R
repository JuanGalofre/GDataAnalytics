#Para filtrar todos los nombres de las estacionesde partida,primero se seleccionan para
#que no quede ninguna como NA
dataBase<-filter(dataBase,start_station_name!=""&!is.na(start_station_name))

#Se organizan según el nombre de las estaciones de partida
arrange(dataBase,start_station_name)

#Se obtienen los nombres unicos de las estaciones de partida
start_names_list<-unique(dataBase$start_station_name)


#Primer paso pero ahora para los nombres de las estaciones de llegada
dataBase<-filter(dataBase,end_station_name!=""&!is.na(end_station_name))

arrange(dataBase,end_station_name)

end_names_list<-unique(dataBase$end_station_name)

#Se agregan las dos listas de nombres por si alguna estación de partida no llegase
#a ser estación de llegada
names_list<-c(start_names_list,end_names_list)

#Se omiten los duplicados
names_list<-unique(names_list)


#Base de datos para filtrada para no tener NAs o "" a la hora de buscar las
#locaciones de los nombres de las estaciones.
dataBase_Filtered<-filter(dataBase,start_station_name!=""&!is.na(start_station_name)) %>% 
  filter(end_station_name!=""&!is.na(end_station_name))


#Se crea el data frame.
locations_database<-data.frame(Nombre=c(NA),
                               Max_lng=c(NA),
                               Min_lng=c(NA),
                               Max_lat=c(NA),
                               Min_lat=c(NA)
                               )

#Se crea el ciclo en el cual se recorrera la lista de nombres, mientras se anota
#la maxima y la minima longitud como la latitud. Para despues comparar con las
#coordenadas que tenemos solas y ver si se pueden ubicar en algunas de las estaciones

for (i in names_list){
    set_of_values <- filter(dataBase_Filtered, start_station_name == i)
    nueva_linea <- data.frame(
      Nombre = i,
      Max_lng = max(set_of_values$start_lng),
      Min_lng = min(set_of_values$start_lng),
      Max_lat = max(set_of_values$start_lat),
      Min_lat = min(set_of_values$start_lat)
    )
    #Este if es por si la estación que estamos buscando no esta en la estación
    #de entrada, entonces la buscamos en la estaciones finales.
    if (nueva_linea$Max_lng==-Inf){
      set_of_values <- filter(dataBase_Filtered, end_station_name == i)
      nueva_linea <- data.frame(
        Nombre = i,
        Max_lng = max(set_of_values$end_lng),
        Min_lng = min(set_of_values$end_lng),
        Max_lat = max(set_of_values$end_lat),
        Min_lat = min(set_of_values$end_lat)
      ) 
    }
  locations_database=rbind(locations_database,nueva_linea)
}

