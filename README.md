# Big_Data_Opcional

**Pregunta 1**

Para la realizaci�n de una estad�stica descriptiva, se trabajar� con el comando summary(Datos_Nomofobia), arrojando la media, mediana, moda y percentiles de las
variables a analizar.


 **Nomofobia         Estr�s              Ansiedad            Compulsividad**  
 Min.   : 1.00       Min.   : 5.00       Min.   : 4.00       Min.   : 1.00  
 1st Qu.:25.00       1st Qu.:32.00       1st Qu.:26.00       1st Qu.:36.00  
 Median :50.00       Median :49.00       Median :51.00       Median :50.00  
 Mean   :50.88       Mean   :51.46       Mean   :52.15       Mean   :52.87  
 3rd Qu.:75.00       3rd Qu.:72.00       3rd Qu.:77.00       3rd Qu.:74.00  
 Max.   :99.00       Max.   :97.00       Max.   :99.00       Max.   :99.00  
 

**Habilidades blandas     Resoluci�n de conflictos     Tiempo de uso del celular**
   Min.   :  1.00         Min.   :-10.00               Min.   :-15.00           
   1st Qu.: 28.00         1st Qu.: 20.00               1st Qu.: 34.00           
   Median : 54.00         Median : 52.00               Median : 57.00           
   Mean   : 57.71         Mean   : 49.61               Mean   : 54.83           
   3rd Qu.: 77.00         3rd Qu.: 74.00               3rd Qu.: 74.00           
   Max.   :350.00         Max.   : 99.00               Max.   : 99.00           


 **G�nero               Edad **     
 Length:129             Min.   :20.00  
 Class :character       1st Qu.:22.00  
 Mode  :character       Median :25.00  
                        Mean   :24.59  
                        3rd Qu.:27.00  
                        Max.   :30.00  

La variable "Genero" NO es posible realizarle dicha estad�stica porque pertenece a una variable cualitativa.



**Pregunta 2** 

Para el analisis de Normalidad, Kurtosis y Asimetria se utilizan los comandos Kurtosis(Data[-8]) y Skewness(Data[-8]) ademas de graficos de histogramas. 
Ademas como se solicita analisis de normalidad, se utilizar� el comando qqnorm para visualizar los graficos de normalidad de las variables, donde la curva si se comporta de forma diagonal, 
se cumplir�a con la normalidad. Si bien la mayor�a de las variables cumplen con dicho requisito, esto no ocurre con la variable "Habilidades Blandas".

#Skewness#

  Nomofobia                    Estr�s                  Ansiedad 
  0.13605192                0.06769615                0.09125894 

 Compulsividad           Habilidades blandas    Resoluci�n de conflictos 
  0.06296501                3.22958177                0.02052786 

Tiempo de uso del celular        Edad 
  -0.15754888                0.23635694 

Es sabido para que el comportamiento de los errores sea normal, este debe tener su simetr�a cercana a 0, en este caso la variable "Habilidades Blandas" no cumple con el requisito.


#Kurtosis#

 Nomofobia                    Estr�s                  Ansiedad 
 1.804174                    1.963184                 1.774285 
            
Compulsividad       Habilidades blandas       Resoluci�n de conflictos 
 2.028986                23.118473                    1.763247 

Tiempo de uso del celular          Edad 
 2.099065                        1.951769 

Por ultimo la kurtosis deberia dar como valor no mayor a 3 para que sea normal, se confirma que la variable "Habilidades Blandas" no cumple con normalidad.


**Pregunta 3** 

La regresi�n a utilizar es la siguiente: 
RegresionCompleta<-lm(Datos_Nomofobia$Nomofobia~Datos_Nomofobia$Ansiedad+Datos_Nomofobia$Compulsividad+Datos_Nomofobia$Estr�s+Datos_Nomofobia$`Habilidades blandas`+Datos_Nomofobia$`Resoluci�n de conflictos`+Datos_Nomofobia$`Tiempo de uso del celular`+Datos_Nomofobia$G�nero+Datos_Nomofobia$Edad)


**Pregunta4** 

                            Coeficientes
Constante                :   -0.30657 
Estr�s                   :    0.17206
Ansiedad                 :    0.01860  
Compulsividad            :    0.24256
Habilidades Blandas      :    0.03125  
Resoluci�n de Conflictos :   -0.03900  
Tiempo de uso del Celular:    0.57070
Edad                     :   -0.14257 
Genero (mujer)           :    1.82019  

Al analizar los coeficiente, se llega a la conclusi�n que: mientras mayor sea el estr�s, mayor es la posibilidad de sufrir de nomofobia, lo mismo ocurre con
la ansiedad, compulsividad, habilidades blandas, tiempo de uso del celular y genero si esta pertenece al genero femenino, en el caso de las habilidades blandas
uno esperar�a que su signo fuera negativo, tal como ocurre con resoluci�n de conflicto y edad, en donde, mas alto sea su coeficiente menor es la probabilidad
de sufrir de nomofobia.


**Pregunta 5**

Analisis Anova 

                                             Df Sum Sq Mean Sq F value   Pr(>F)    
Datos_Nomofobia$Ansiedad                      1   1813    1813   3.894 0.050748 .  
Datos_Nomofobia$Compulsividad                 1   9368    9368  20.128 1.67e-05 ***
Datos_Nomofobia$Estr�s                        1   6699    6699  14.393 0.000234 ***
Datos_Nomofobia$`Habilidades blandas`         1    421     421   0.905 0.343354    
Datos_Nomofobia$`Resoluci�n de conflictos`    1      1       1   0.003 0.956902    
Datos_Nomofobia$`Tiempo de uso del celular`   1  23935   23935  51.426 6.62e-11 ***
Datos_Nomofobia$G�nero                        1    106     106   0.227 0.634649    
Datos_Nomofobia$Edad                          1     21      21   0.045 0.831736    
Residuals                                   120  55851     465                     
---
Signif. codes:  0 �***� 0.001 �**� 0.01 �*� 0.05 �.� 0.1 � � 1

Analizando el valor p de cada variable, refleja que "Habilidades Blandas", "Resoluci�n de conflictos", "Genero" y "Edad" no son significativos, al no ser menor a 0,05.


**Pregunta 6**

hist(Datos_Nomofobia$Nomofobia): al ejecutar el comando, el resultado que se obtiene es que su comportamiento no tender�a a normal.
boxplot(Datos_Nomofobia$Ansiedad: viendo el boxplot en una primera instanc�a tenderia a normal, una media de 50, percentil 75 de 75 y percentil 25 de 25, tambien los limites superior e inferior son el 100 y el 0 respectivamente.
barplot(Datos_Nomofobia$Estr�s): tomamndo la variable estr�s, posee un comportamiento uniforme.


**Pregunta 7**

Identificar Outliers.

Como se ha mencionado en preguntas anteriores la unica variable que no posee una comportamiento normal es "Habilidades Blandas".


**Pregunta 8**

Los outliers deben ser eliminados ya que los valores deben comprender entre el 1 y 100, si los valores estan entre los 200 y 350 son considerados altos.
Por no existir una precisi�n en las respuestas de las observacion es recomendable eliminar los outliers que son considerados altos. 
 