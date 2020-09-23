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
datos<-as.data.frame(read.csv('200921COVID19MEXICO.csv',header=T, sep=','))


#CASO 1:Los que no fallecieron con periodo de 14 días
activos<-datos %>% dplyr::filter(RESULTADO=='1' & FECHA_DEF=='9999-99-99' & ENTIDAD_RES=='14')
activos_v<-activos %>% select(FECHA_SINTOMAS)
hol<-NULL
#le voy sumando de uno en uno los 1 días a cada fecha
for(i in 1:13)
{	
	hola<-paste0("fechas_sin",i)
	hola<-activos_v %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+i)
	FECHA_SINTOMAS<-as.Date(as.character(hola$fecha))
	hol<-rbind(hol,as.data.frame(FECHA_SINTOMAS))
}

#uno los datasets que van saliendo
activo<-plyr::rbind.fill(activos_v,hol)


#CASO 2: Para las personas que fallecieron que tuvieron un periodo activo

#de 14 días
activos2<-datos %>% dplyr::filter(RESULTADO=='1' & FECHA_DEF!='9999-99-99' & ENTIDAD_RES=='14')
defunciones<-activos2 %>% mutate(def=as.Date(FECHA_DEF),diferencia=abs(as.Date(FECHA_SINTOMAS)-as.Date(FECHA_DEF)))
defunciones<-defunciones%>%select(FECHA_SINTOMAS,diferencia,def)
def<-defunciones %>% dplyr::filter(diferencia>=14) %>%select(FECHA_SINTOMAS)
hol2<-hol1<-NULL
#le sumo de uno en uno los 13 días
for(i in 1:13)
		{	
			hola<-paste0("fechas_sin",i)
			hola<-def %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+i)
			FECHA_SINTOMAS<-as.Date(as.character(hola$fecha))
			hol2<-rbind(hol2,as.data.frame(FECHA_SINTOMAS))
		}

#uno los dataframe que van saliendo
active<-plyr::rbind.fill(activo,def,hol2)


hol1<-NULL
#Para la gente que fallecio en el primer día de síntomas
def_2<-defunciones %>% dplyr::filter(diferencia==0)
def_f<-def_2 %>%select(FECHA_SINTOMAS)
	
	hola<-paste0("fechas_sin",i)
	hola<-def_f %>% mutate(fecha=as.Date(FECHA_SINTOMAS))
	FECHA_SINTOMAS<-as.Date(as.character(hola$fecha))
	hol1<-rbind(hol1,as.data.frame(FECHA_SINTOMAS))

#CASO 3: Para la gente que fallecio de 1 a 13 días después de inicio de síntomas
arr<-c(1,2,3,4,5,6,7,8,9,10,11,12,13)
for(i in 1:13)
{
def_2<-defunciones %>% dplyr::filter(diferencia==arr[i])
def_f<-def_2 %>%select(FECHA_SINTOMAS)
	for(j in 1:arr[i])
	{
		#Para cada caso dependiendo de los días transcurridos le sumo la cantidad de días
		hola<-paste0("fechas_sin",j)
		hola<-def_f %>% mutate(fecha=as.Date(FECHA_SINTOMAS)+j)
		FECHA_SINTOMAS<-as.Date(as.character(hola$fecha))
		hol1<-rbind(hol1,as.data.frame(FECHA_SINTOMAS))
	}
}

#saco los días originales para el caso anterior, es decir el día 1
def_f<-defunciones %>% dplyr::filter(diferencia<14) %>%select(FECHA_SINTOMAS)
#agrego ese día al dataframe
hol1<-plyr::rbind.fill(def_f,hol1)


#junto el de los días de los que fallecieron en un tiempo menor que 13 y el de los anteriores
active<-plyr::rbind.fill(active,hol1)



#Ordeno y cuento por fecha
fechas<-active%>%  
dplyr::group_by(FECHA_SINTOMAS)%>%
dplyr::summarise(datos=n()) 

#le quito los 14 días de las dos semanas que aún no son claras en los resultados
fechas<-fechas[1:(dim(fechas)[1]-14),]


#grafica, yo use el geom_smooth para mostrar como va la tendencia
win.graph()
ggplot(data= fechas,aes(x =as.Date(FECHA_SINTOMAS) , y =(datos))) +
 geom_point(color = 'blue')+ geom_line(color='blue')+xlab('Fecha de sintomas')+ geom_smooth(color = 'black',fill=NA)+xlab('Fecha de sintomas')+
  ylab('Número de casos') +  labs(title = "Casos confirmados activos por covid-19 en el estado de Jalisco 05/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
geom_vline(xintercept =as.Date("2020-07-20"),col='red')+
geom_vline(xintercept =as.Date("2020-06-8"),col='red')+
theme_bw()
#+geom_line(color='blue')
