#Ahora los tiempos de desplazamiento de los usarios en los tipos de bicicletas. Quiero hacer un 
#promedio anual y una grafica de tiempo de los usuarios en las dif ciclas alrededor del año.



#Hago una lista con las varaibles que estan en el entorno para acceder a lo que
#saque con el codigo anterior
lista_trabajo<-ls(pattern="bike_time")
lista_trabajo<- lista_trabajo[1:13]

#Creo un data frame para almacenar todos los miembros
member_bike_times<-data.frame(rideable_type=c(NA),
                              member_casual=c(NA),
                              n=c(NA),
                              Tiempo=c(NA)
)


#Data frame para almacenar a todos los casuales
casual_bike_times<-data.frame(rideable_type=c(NA),
                              member_casual=c(NA),
                              n=c(NA),
                              Tiempo=c(NA)
)
mixed_bike_times_year<-data.frame()
#For para recorrer los archivos de tiempos  de bicicletas, mientras se almacenan en
#los data frames
indicador_mes <-1
for (i in lista_trabajo){
  #obtener el data frame a partir del nombre
  dataFrame <- get(i)
  #separar entre miembro y casual
  member_month <- dataFrame[dataFrame$member_casual == "member",]
  member_bike_times<-rbind(member_bike_times,member_month)
  casual_month<- dataFrame[dataFrame$member_casual == "casual",]
  casual_bike_times<-rbind(casual_bike_times,casual_month)
  
  
  #Ver como se desempeñan las bicicletas a traves del año 
  mixed_bike_times_year_pre<-merge(member_month,casual_month, all=TRUE)
  mixed_bike_times_year_pre["Mes"]<-indicador_mes
  mixed_bike_times_year<-rbind(mixed_bike_times_year,mixed_bike_times_year_pre)
  indicador_mes <- indicador_mes +1
}
#Se filtra para quitar NAs, se agrupa y se suma las cantidades
member_bike_times <-filter(member_bike_times,!is.na(rideable_type))
member_bike_times <-group_by(member_bike_times,rideable_type) %>%  
  summarise(Avg_time=mean(Tiempo, na.rm=TRUE)) 

casual_bike_times <-filter(casual_bike_times,!is.na(rideable_type))
casual_bike_times <-group_by(casual_bike_times,rideable_type) %>%  
  summarise(Avg_time=mean(Tiempo, na.rm=TRUE)) 

#-----------------------------------------------------------------------
#Graphs
#Comparación de elección de tipo de bicicletas entre miembros y casuales.
casual_bike_times_mod <-casual_bike_times
casual_bike_times_mod$rideable_type <-c("Classic","Docked","Electric")
casual_bike_times_mod["TipoCliente"] <-c("Casual","Casual","Casual")


member_bike_times_mod <-member_bike_times
member_bike_times_mod$rideable_type <-c("Classic","Electric")
member_bike_times_mod["TipoCliente"] <-c("Member","Member")

casual_and_member_bike_times<-merge(casual_bike_times_mod,member_bike_times_mod, all=TRUE)
#Graficar la preferencia por tipo de cliente a lo largo del año
filter(casual_and_member_bike_times,)
ggplot(casual_and_member_bike_times,aes(x=rideable_type,y=Avg_time, fill=TipoCliente))+
  geom_bar(stat="identity",position = "dodge")+
  ylab("Riding time")+
  xlab("Bike type")+
  ggtitle("Bikes riding time",subtitle = "April 2022 to April 2023" )


#Histograma del tiempo de viaje en general
ggplot(mixed_bike_times_year,aes(x=Tiempo))+
  geom_histogram(binwidth=2) +
  ggtitle("Bike rides across the year", subtitle="April 2022 to April 2023")+
  xlim(1,200)



#Histograma del tiempo de viaje para miembros
only_member_bike_times_graph<-filter(mixed_bike_times_year,member_casual=="member")
ggplot(only_member_bike_times_graph,aes(x=Tiempo))+
  geom_histogram(binwidth=2) +
  xlim(1,200)+
  ggtitle("Bike times for members", subtitle="April 2022 to April 2023")

ggplot(only_member_bike_times_graph,aes(x=Tiempo))+
  geom_boxplot()

#Histograma del tiempo de viaje para casuales
only_casual_bike_times_graph<-filter(mixed_bike_times_year,member_casual=="casual")
ggplot(only_casual_bike_times_graph,aes(x=Tiempo))+
  geom_histogram(binwidth=2) +
  xlim(1,200)+
  ggtitle("Bike times for casuals", subtitle="April 2022 to April 2023")

