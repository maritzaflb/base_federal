#Defunciones en Jalisco
#9999-99-99



library(dplyr)
library(data.table)
library("tidyr")
library(ggplot2)
library(scales) 
library(stringr)


#importo el archivo
#importo los datos como un dataset
datos<-as.data.frame(read.csv('200916COVID19MEXICO.csv',header=T, sep=','))



#Voy a extraer todos los positivos

#fecha actual Sys.Date()

activos<-datos %>% dplyr::filter(RESULTADO=='1' & FECHA_DEF=='9999-99-99' & ENTIDAD_RES=='14' & TIPO_PACIENTE=='1')




	fechas_sin1<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+1)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin1$FECHA_SINTOMAS))
	array1<-as.data.frame(FECHA_SINTOMAS)

	fechas_sin2<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+2)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin2$FECHA_SINTOMAS))
	array2<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin3<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+3)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin3$FECHA_SINTOMAS))
	array3<-as.data.frame(FECHA_SINTOMAS)

	fechas_sin4<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+4)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin4$FECHA_SINTOMAS))
	array4<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin5<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+5)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin5$FECHA_SINTOMAS))
	array5<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin6<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+6)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin6$FECHA_SINTOMAS))
	array6<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin7<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+7)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin7$FECHA_SINTOMAS))
	array7<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin8<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+8)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin8$FECHA_SINTOMAS))
	array8<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin9<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+9)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin9$FECHA_SINTOMAS))
	array9<-as.data.frame(FECHA_SINTOMAS)



	fechas_sin10<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+10)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin10$FECHA_SINTOMAS))
	array10<-as.data.frame(FECHA_SINTOMAS)



	fechas_sin11<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+11)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin11$FECHA_SINTOMAS))
	array11<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin12<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+12)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin12$FECHA_SINTOMAS))
	array12<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin13<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+13)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin13$FECHA_SINTOMAS))
	array13<-as.data.frame(FECHA_SINTOMAS)


	fechas_sin14<-activos %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+14)
	FECHA_SINTOMAS<-as.Date(as.character(fechas_sin14$FECHA_SINTOMAS))
	array14<-as.data.frame(FECHA_SINTOMAS)





seleccion<-activos %>% select(FECHA_SINTOMAS)
sel<-seleccion

 es<-plyr::rbind.fill(array1,array2,array3,array4,array5,array6,array7,array8,array9,array10,array11,array12,array13,array14)
fech<-plyr::rbind.fill(seleccion,es)
fechas<-fech%>%  
dplyr::group_by(FECHA_SINTOMAS)%>%
dplyr::summarise(datos=n()) 

fechas<-fechas[1:(dim(fechas)[1]-14),]
win.graph()

ggplot(data= fechas,aes(x =as.Date(FECHA_SINTOMAS) , y =(datos))) +
 geom_point(color = 'blue')+ xlab('Fecha de síntomas')+ geom_smooth(color = 'black',level = 0.95,span=0.4,method="loess",fill='#87CEEB')+xlab('Fecha de síntomas')+
  ylab('Número de casos') +  labs(title = "Casos confirmados activos ambulatorios por covid-19 en el estado de Jalisco 02/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+

geom_vline(xintercept =as.Date("2020-07-13"),col='red')+
geom_vline(xintercept =as.Date("2020-06-01"),col='red')+

theme_bw()

