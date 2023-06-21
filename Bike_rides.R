#Empezamos los resumenes del año con tipos de bicicletas. Quiero hacer un 
#promedio anual y una grafica de utilización de las ciclas a traves del año.
#En el promedio quiero utilizar un pie_chart para comparar el uso de que tipo
#de bicicletas por casuales y miembros.


#Hago una lista con las varaibles que estan en el entorno para acceder a lo que
#saque con el codigo anterior
lista_trabajo<-ls(pattern="bike_types")
lista_trabajo<- lista_trabajo[1:13]

#Creo un data frame para almacenar todos los miembros
member_bike_types<-data.frame(rideable_type=c(NA),
                   member_casual=c(NA),
                   n=c(NA)
                   )


#Data frame para almacenar a todos los casuales
casual_bike_types<-data.frame(rideable_type=c(NA),
                   member_casual=c(NA),
                   n=c(NA)
)
mixed_bike_types_year<-data.frame()
#For para recorrer los archivos de tipos de bicicletas, mientras se almacenan en
#los data frames
indicador_mes <-1
for (i in lista_trabajo){
  #obtener el data frame a partir del nombre
  dataFrame <- get(i)
  #separar entre miembro y casual
  member_month <- dataFrame[dataFrame$member_casual == "member",]
  member_bike_types<-rbind(member_bike_types,member_month)
  casual_month<- dataFrame[dataFrame$member_casual == "casual",]
  casual_bike_types<-rbind(casual_bike_types,casual_month)
  
  
  #Ver como se desempeñan las bicicletas a traves del año 
  mixed_bike_types_year_pre<-merge(member_month,casual_month, all=TRUE)
  mixed_bike_types_year_pre["Mes"]<-c(indicador_mes,indicador_mes,indicador_mes,indicador_mes,indicador_mes)
  mixed_bike_types_year<-rbind(mixed_bike_types_year,mixed_bike_types_year_pre)
  indicador_mes <- indicador_mes +1
}
#Se filtra para quitar NAs, se agrupa y se suma las cantidades
member_bike_types <-filter(member_bike_types,!is.na(rideable_type))
member_bike_types <-group_by(member_bike_types,rideable_type) %>%  
  summarise(Total=sum(n, na.rm=TRUE)) 

casual_bike_types <-filter(casual_bike_types,!is.na(rideable_type))
casual_bike_types <-group_by(casual_bike_types,rideable_type) %>%  
  summarise(Total=sum(n, na.rm=TRUE)) 

#-----------------------------------------------------------------------
#Graphs
#Comparación de elección de tipo de bicicletas entre miembros y casuales.
casual_bike_types_mod <-casual_bike_types
casual_bike_types_mod$rideable_type <-c("Classic","Docked","Electric")
casual_bike_types_mod["TipoCliente"] <-c("Casual","Casual","Casual")


member_bike_types_mod <-member_bike_types
member_bike_types_mod$rideable_type <-c("Classic","Electric")
member_bike_types_mod["TipoCliente"] <-c("Member","Member")

casual_and_member_bike_types<-merge(casual_bike_types_mod,member_bike_types_mod, all=TRUE)
#Graficar la preferencia por tipo de cliente a lo largo del año
ggplot(casual_and_member_bike_types,aes(x=rideable_type,y=Total, fill=TipoCliente))+
  geom_bar(stat="identity",position = "dodge")+
  ylab("Rides")+
  xlab("Bike type")+
  ggtitle("Bikes preference",subtitle = "April 2022 to April 2023" )
#Graficar el numero de viajes en el año por tipo de bicicleta
ggplot(mixed_bike_types_year,aes(x=Mes,y=n,fill=rideable_type))+
  geom_bar(stat="identity") +
  ggtitle("Bike rides across the year", subtitle="April 2022 to April 2023")
#Graficar el numero de viajes en el año por tipo de bicicleta para miembros
only_member_bike_types_graph<-filter(mixed_bike_types_year,member_casual=="member")
ggplot(only_member_bike_types_graph,aes(x=Mes,y=n,fill=rideable_type))+
  geom_bar(stat="identity") +
  ggtitle("Bike rides for memebers across the year", subtitle="April 2022 to April 2023")
#Graficar el numero de viajes en el año por tipo de bicicleta para casuales
only_casual_bike_types_graph<-filter(mixed_bike_types_year,member_casual=="casual")
ggplot(only_casual_bike_types_graph,aes(x=Mes,y=n,fill=rideable_type))+
  geom_bar(stat="identity") +
  ggtitle("Bike rides for casuals across the year", subtitle="April 2022 to April 2023")

