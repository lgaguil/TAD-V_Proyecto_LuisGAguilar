Info_Autos= as.data.frame(MOCK_DATA_Autos)


View(Info_Autos)


#Diferencia en el precio de BMW a traves de los a�os

plot(Info_Autos[Info_Autos$Marca =='BMW',]$Precio
     ,Info_Autos[Info_Autos$Marca =='BMW',]$A�o,
     xlab = "Monto",ylab = "A�o",
     main='Distribucion de precio seg�n a�o',
     pch=16,
     col="dark green",
     cex=2)




#Marcas Vendidas por a�o


fx_Muestra_marcas = function(A�o){
  plot(as.factor(Info_Autos[Info_Autos$A�o==A�o,]$Marca),
       main='Marcas vendidas por a�o',
       col="gray")
  
}

fx_Muestra_marcas('2018')
  

  

#Ventas Marcas por a�o por pa�s



View(filter(Info_Autos, Marca=="Ford" & Pa�s%in%c("Brazil") ))

fx_MarcaPais = function(Marca2,Pais2){
  ggplot((filter(Info_Autos, Marca==Marca2 & Pa�s%in%Pais2 )), aes(x=A�o , y=Pa�s)) + geom_jitter()
  
}

fx_MarcaPais("Toyota",c("United States","France"))



#Agencias que ha vendido m�s atraves de los a�os




conteo=table(Info_Autos$Agencia)

barplot(conteo,main = "Hist�rico Ventas por agencia",
        ylab="Cantidad")






#Diferencia en Metodo de compra a traves de los a�os seg�n agencia




fx_MetodoCompra = function(Marca2,MetComp,Agencia2){
  ggplot((filter(Info_Autos, Marca==Marca2 & MetodoCompra%in%MetComp & Agencia%in%Agencia2 )), 
         aes(x=MetodoCompra , y=A�oVenta)) + geom_jitter(aes(colour=Agencia))
  
}

fx_MetodoCompra("Ford",c("Leasing","Contado","Prestamo"),c("Zoozzy","Viva"))

