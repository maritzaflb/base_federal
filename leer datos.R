
library(dplyr)
library(data.table)
library("dplyr")
library("tidyr")
library(ggplot2)
library(scales) 
library(stringr)

library(fTrading)
#importo el archivo
#importo los datos como un dataset
datos<-as.data.frame(read.csv('200916COVID19MEXICO.csv',header=T, sep=','))



##EN todo el pais

#ordenar los datos por fecha

#fechas<-datos %>% arrange(FECHA_INGRESO)


#Voy a extraer todos los positivos

positivos<-datos %>% dplyr::filter(RESULTADO=='1')


#ORDENADOS POR FECHA
deteccion_s<-positivos%>%  
dplyr::group_by(FECHA_INGRESO) %>%  
dplyr::summarise(casos_s=n()) 


s_s<-as.data.frame(deteccion_s)
s_s<-s_s[1:(dim(s_s)[1]-14),]

dia<-as.Date(as.character(s_s$FECHA_INGRESO))

#En México
win.graph()
ggplot(data= s_s,aes(x =as.Date(FECHA_INGRESO) , y =as.numeric(casos_s))) +
 geom_point(color = 'blue')+ geom_smooth(color='black',level = 0.95,span=0.2,method="loess",fill='#87CEEB')+ xlab('Fecha de ingreso')+
  ylab('Número de casos') +  labs(title = "Confirmados por covid-19 en México al 02/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
theme_bw()

#


##En jalisco


fechas<-datos %>% arrange(FECHA_INGRESO)


#Voy a extraer todos los positivos

positivos<-datos %>% dplyr::filter(RESULTADO=='1' & ENTIDAD_RES=='14')

#ORDENADOS POR FECHA
deteccion_s<-positivos%>%  
dplyr::group_by(FECHA_INGRESO) %>%  
dplyr::summarise(casos_s=n()) 


s_s<-as.data.frame(deteccion_s)

s_s<-s_s[1:(dim(s_s)[1]-14),]


dia<-as.Date(as.character(s_s$FECHA_INGRESO))


win.graph()
ggplot(data= s_s,aes(x =dia , y =as.numeric(casos_s))) +
 geom_point(color = 'blue')+ geom_smooth(color = 'black',level = 0.95,span=0.3,method="loess",fill='#87CEEB')  + xlab('Fecha de ingreso')+
  ylab('Número de casos') +  labs(title = "Confirmados (hospitalizados y ambulatorios) por covid-19 en el estado de Jalisco utilizando la técnica PCR al 02/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
geom_vline(xintercept=as.Date("2020/06/12"),color='red')+
theme_bw()





	