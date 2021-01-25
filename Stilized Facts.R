
install_or_load_pack <- function(pack){
  
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  
  if (length(create.pkg))
    
    install.packages(create.pkg, dependencies = TRUE)
  
  sapply(pack, require, character.only = TRUE)
  
}

install_or_load_pack(c("readr","hms","XLConnect","moments","ggplot2"))

setwd("C:/Users/LUIS/Desktop/Clases unam")


wb = loadWorkbook("Prices.xlsx")

# E L E G I R ----> sheet = "SP"/ "UST" / "MXN"


activo = "MXN" #"UST" "SP" "MXN"
datos = readWorksheet(wb, sheet = activo, header = TRUE) 

if (activo=="MXN"){
  etiqueta = c("Peso mexicano","Pesos por dólar","Porcentaje","Rendimientos logarítmicos")
}
if(activo=="UST"){
  etiqueta = c("Bono de 10 años del Tesoro de Estados Unidos","Tasa","Porcentaje","Rendimientos simples")
}
if(activo=="SP"){
  etiqueta = c("Índice S&P","Índice","Porcentaje","Rendimientos logarítmicos")
}

datos$Date<-as.Date(datos$Date,format='%m/%d/%Y') 
par(cex=0.7,mfrow=c(2,1)) 

par(cex.axis = 0.6, cex.lab = .8, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
plot(x=datos$Date,y=datos$Close,col="red",type="l",ylab=etiqueta[2],xlab= "",lwd = 1, font = 1, font.lab = 1,main = etiqueta[1])


#Calcula rendimientos logaritmicos y grafica
if(activo == "UST"){
  Rdatos<-diff(datos$Close,lag = 1)
} else {
  Rdatos<-diff(log(datos$Close),lag = 1)*100
}

df<-data.frame(x=datos$Date[2:nrow(datos)],y=Rdatos)
par(cex.axis = 0.6, cex.lab = .8, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
plot(x=datos$Date[2:nrow(datos)],y=Rdatos,col="blue",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = etiqueta[4],cex.main=1)

A<-matrix(data = NA,ncol =4,nrow = 1 )
A[1,1]<-round(mean(Rdatos),5)
A[1,2]<-round(sd(Rdatos),5)
A[1,3]<-round(skewness(Rdatos),5)
A[1,4]<-round(kurtosis(Rdatos)+3,5)
A

par(mfrow=c(1,1)) 
Ker<-density(Rdatos)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(Rdatos, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",lwd=2,xlim =c(-3,3),
     ylim = c(0,0.9),xlab ="Rendimientos logarítmicos",ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(Rdatos),sd=sd(Rdatos)), from=-10, to=10, add=TRUE, col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')


par(mfrow=c(1,1))
qqnorm(Rdatos,ylim = c(-7,7),main=activo)
qqline(Rdatos,col = "firebrick",lwd=2,lty=2)


if ( activo == "UST"){
  par(mfrow=c(2,1))
  acf(Rdatos, lwd=8, lend=1, col="coral2",main="Rendimientos simples")
  acf(abs(Rdatos), lwd=8, lend=1, col="coral2",main="Valor absoluto de los rendimientos simples")
} else {
  par(mfrow=c(2,1))
  acf(Rdatos, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos")
  acf(abs(Rdatos), lwd=8, lend=1, col="coral2",main="Valor absoluto de los rendimientos logarítmicos")
}

par(mfrow=c(2,2))
#Calcula rendimientos logaritmicos y grafica
if ( activo == "UST"){
  Rdatos<-diff(datos$Close,lag = 1)
  plot(x=datos$Date[2:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos simples diarios ",cex.main=1)
  
  Rdatos<-diff(datos$Close,lag = 5)
  plot(x=datos$Date[6:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos simples semanales",cex.main=1)
  
  Rdatos<-diff(datos$Close,lag = 21)
  plot(x=datos$Date[22:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos simples mensuales",cex.main=1)
  
  Rdatos<-diff(datos$Close,lag = 360)
  plot(x=datos$Date[361:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos simples anuales",cex.main=1)
} else {
  Rdatos<-diff(log(datos$Close),lag = 1)*100
  plot(x=datos$Date[2:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos logarítmicos diarios ",cex.main=1)
  
  Rdatos<-diff(log(datos$Close),lag = 5)*100
  plot(x=datos$Date[6:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos logarítmicos semanales",cex.main=1)
  
  Rdatos<-diff(log(datos$Close),lag = 21)*100
  plot(x=datos$Date[22:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos logarítmicos mensuales",cex.main=1)
  
  Rdatos<-diff(log(datos$Close),lag = 360)*100
  plot(x=datos$Date[361:nrow(datos)],y=Rdatos,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos logarítmicos anuales",cex.main=1)
  
}
