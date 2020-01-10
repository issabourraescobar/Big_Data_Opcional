#Ejercicio1#

Datos_Nomofobia <- read_excel("Datos_Nomofobia.xlsx")

summary(Datos_Nomofobia)

hist(Datos_Nomofobia$Nomofobia)
hist(Datos_Nomofobia$Ansiedad)
hist(Datos_Nomofobia$Estrés)
hist(Datos_Nomofobia$Compulsividad)
hist(Datos_Nomofobia$`Habilidades blandas`)
hist(Datos_Nomofobia$`Resolución de conflictos`)
hist(Datos_Nomofobia$`Tiempo de uso del celular`)
hist(Datos_Nomofobia$Edad)


#Ejercicio2#

library(moments)
skewness(Datos_Nomofobia[-8])
kurtosis(Datos_Nomofobia[-8])
par(mfcol=c(2,4))
qqnorm(Datos_Nomofobia$Nomofobia)
qqnorm(Datos_Nomofobia$Ansiedad)
qqnorm(Datos_Nomofobia$Estrés)
qqnorm(Datos_Nomofobia$Compulsividad)
qqnorm(Datos_Nomofobia$`Habilidades blandas`)
qqnorm(Datos_Nomofobia$`Resolución de conflictos`)
qqnorm(Datos_Nomofobia$Edad)

#Ejercicio3#

RegresionCompleta<-lm(Datos_Nomofobia$Nomofobia~Datos_Nomofobia$Ansiedad+Datos_Nomofobia$Compulsividad+Datos_Nomofobia$Estrés+Datos_Nomofobia$`Habilidades blandas`+Datos_Nomofobia$`Resolución de conflictos`+Datos_Nomofobia$`Tiempo de uso del celular`+Datos_Nomofobia$Género+Datos_Nomofobia$Edad)
RegresionCompleta


#Ejercicio5#

ANOVA1<-aov(Datos_Nomofobia$Nomofobia~Datos_Nomofobia$Ansiedad+Datos_Nomofobia$Compulsividad+Datos_Nomofobia$Estrés+Datos_Nomofobia$`Habilidades blandas`+Datos_Nomofobia$`Resolución de conflictos`+Datos_Nomofobia$`Tiempo de uso del celular`+Datos_Nomofobia$Género+Datos_Nomofobia$Edad)
summary(ANOVA1)

#Ejercicio6#

hist(Datos_Nomofobia$Nomofobia)
boxplot(Datos_Nomofobia$Ansiedad)
barplot(Datos_Nomofobia$Estrés)

#Ejercicio7#

par(mfcol=c(2,4))
boxplot(Datos_Nomofobia$Nomofobia,main= "Nomofobia")
boxplot(Datos_Nomofobia$Ansiedad,main= "Ansiedad")
boxplot(Datos_Nomofobia$Estrés,main= "Estres")
boxplot(Datos_Nomofobia$Compulsividad,main= "Compulsividad")
boxplot(Datos_Nomofobia$`Habilidades blandas`,main= "Habilidades Blandas")
boxplot(Datos_Nomofobia$`Resolución de conflictos`,main= "Resolucion de Conflictos")
boxplot(Datos_Nomofobia$Edad,main= "Edad")
boxplot(Datos_Nomofobia$Nomofobia~Datos_Nomofobia$Género,main= "Genero")


#Ejercicio8#

DatosFinal<-Datos_Nomofobia[c(-5,-11,-106),]
DatosFinal_Sin_Negativosa<-DatosFinal[c(-46,-41,-126,-31),]
