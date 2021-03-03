Info_Autos= as.data.frame(MOCK_DATA_Autos)


View(Info_Autos)


#Diferencia en el precio de BMW a traves de los años

plot(Info_Autos[Info_Autos$Marca =='BMW',]$Precio
     ,Info_Autos[Info_Autos$Marca =='BMW',]$Año,
     xlab = "Monto",ylab = "Año",
     main='Distribucion de precio según año',
     pch=16,
     col="dark green",
     cex=2)




#Marcas Vendidas por año


fx_Muestra_marcas = function(Año){
  plot(as.factor(Info_Autos[Info_Autos$Año==Año,]$Marca),
       main='Marcas vendidas por año',
       col="gray")
  
}

fx_Muestra_marcas('2018')
  

  

#Ventas Marcas por año por país



View(filter(Info_Autos, Marca=="Ford" & País%in%c("Brazil") ))

fx_MarcaPais = function(Marca2,Pais2){
  ggplot((filter(Info_Autos, Marca==Marca2 & País%in%Pais2 )), aes(x=Año , y=País)) + geom_point()
  
}

fx_MarcaPais("Toyota",c("Brazil","Japan"))



#Agencias que ha vendido más atraves de los años




conteo=table(Info_Autos$Agencia)

barplot(conteo,main = "Histórico Ventas por agencia",
        ylab="Cantidad")






#Diferencia en Metodo de compra a traves de los años según agencia




fx_MetodoCompra = function(Marca2,MetComp){
  ggplot((filter(Info_Autos, Marca==Marca2 & MetodoCompra%in%MetComp )), 
         aes(x=MetodoCompra , y=AñoVenta)) + geom_jitter(aes(colour=Agencia))
  
}

fx_MetodoCompra("Ford",c("Leasing","Contado","Prestamo"))

