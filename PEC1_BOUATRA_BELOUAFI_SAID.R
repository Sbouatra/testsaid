##Sección 1. Importación, exportación y unión de datos

## a) Importa los datos del fichero stressEchoMale.csv que corresponde a los hombres y guárdalos en un data frame llamado stressEchoDataMale.
stressEchoMaleData<- read.csv("/Users/saidbouatra/Statistical software/1718S1_SAD_PEC1/DATOS/stressEchoMale.csv",header = TRUE,dec = ",")

## b) Especifica cómo cambiarías el directorio donde guardar los ficheros de datos y de código que generas con R-Studio.
getwd() ## primero localizamos donde esta nuestro directorio de trabajo con la funcion getwd()
setwd("/Users/saidbouatra/Statistical software")## despues definimos donde queremos que se guarde nuestro directorio de trabajo con setwd().

## c) Importa los datos del fichero stressEchoFem.dta y guárdalos en un data frame llamado stressEchoFemaleData.
install.packages("foreing")## no me deja instalar el paquete "foreign" ,(package ‘foreing’ is not available (for R version 3.4.2)).
## para importar ficheros en formato ".dta" con el paquete "foreign" , ejecutamos el siguiente codigo .read.dta("la ruta del archivo")
## por lo que importo el fichero en formato".csv" para poder trabajar con los datos.
stressEchoFemaleData<- read.csv("/Users/saidbouatra/Statistical software/1718S1_SAD_PEC1/DATOS/stressEchoFem.csv",header = TRUE,dec = ",")


## d) Crea un data frame llamado stressEchoDataSet que contenga los datos de stressEchoMaleData y stressEchoFemaleData. ¿Cuáles son los nombres de los campos que contiene la tabla resultante?
stressEchoDataSet<-merge(stressEchoMaleData,stressEchoFemaleData,all = TRUE)
attach(stressEchoDataSet)

## e) Muestra la información del data frame stressEchoDataSet. De forma predeterminada, se muestran un número determinado de filas.
print(stressEchoDataSet) ## se muestran solamente 90 filas , y se omitieron 468 filas 
## ¿cuál sería la instrucción que permite mostrar toda la información de la tabla.
## para modificar el max de filas que se muestran , invocamos a la funcion "options"  para modificar el max de filas .i.e : 1000 filas.
options(max.print = 10000)
## f) Muestra la estructura interna del data frame stressEchoDataSet, es decir, el resumen datos y variables, junto con el formato de cada una de ellas.
names(stressEchoDataSet)
class(stressEchoDataSet)
head(stressEchoDataSet)
tail(stressEchoFemaleData)
names(stressEchoDataSet)
summary(stressEchoDataSet)

## g) Exporta el data frame stressEchoDataSet en un fichero en formato texto. Explica cómo se realizaría a nivel de código y cómo se haría desde R-Commander.
## utilizamos la funcion write.table ( "nombre de data frame", "la ubicacion en el disco duro donde queremos exportar el archivo.txt" , separacion entre los valores de las variables es "white space")
write.table(stressEchoDataSet,"/Users/saidbouatra/Statistical software/1718S1_SAD_PEC1/DATOS/stressEchoMale.txt",sep = "")

## h) Exporta el data frame stressEchoDataSet en un formato apto para trabajar con STATA (http://www.stata.com)
library(foreign)
write.dta(stressEchoDataSet,"/Users/saidbouatra/Statistical software/1718S1_SAD_PEC1/stressEchoDataSet.dta")

## Sección 2. Estructura de datos

## Ejercicio 2: Halla el número de pacientes que han realizado el estudio en stressEchoDataSet.
n_pacients<-nrow(stressEchoDataSet)
print(n_pacients)

## Ejercicio 3: Muestra en una tabla, la distribución por la variable age del data frame stressEchoDataSet y la ordenación de estas edades en modo ascendente.
as.data.frame(table(stressEchoDataSet$age))
stressEchoDataSet[order(age),]
## Ejercicio 4: Muestra los datos de los pacientes a los que se les ha asignado una dosis de dobutamina de 40.
stressEchoDataSet[dose==40,]
## Ejercicio 5: Define un vector que guarde los datos del primer paciente y indica la sintaxis para mostrar esta información por pantalla.
primer_paciente<-stressEchoDataSet[1,1:11]
primer_paciente
## Ejercicio 6: Define un vector que guarde las dosis otorgadas a cada paciente y especifica cual es el valor mínimo y máximo de dosis asignadas. ¿A qué id de paciente corresponde la menor dosis asignada? ¿Corresponde a un hombre o a una mujer? Muestra toda la información de este paciente.
dosis_vector<-data.frame (idPatient,dose)
dosis_vector
with(dosis_vector,min(dose))
with(dosis_vector,max(dose))
## Ejercicio 7: Crea un vector que incluya los id de los pacientes que tengan menos de 50 años, ¿cuántos pacientes hay?
under_50<-stressEchoDataSet[age<50,1]  ## under_50 es un vector con los id de los pacientes menores de 50 años.
length(under_50)  ## hay 48 pacientes que tienen menos de 50 años.

## Ejercicio 8: Lee la secuencia del grupo de proteínas del fichero secProteinGroup extraído de http://www.bioinformatics.org/sms2/group_protein.html y muestra dicha información. 
## ¿Cuántos caracteres aparecen? ¿Cuántas veces aparece cada carácter? 
##¿Cuántas veces se repite el patrón “EE”?Nota.- Para importar secProteinGroup es necesario leerlo utilizando la función readChar().

secProteinGroup<-readChar("/Users/saidbouatra/Statistical software/1718S1_SAD_PEC1/DATOS/secProteinGroup",file.info("/Users/saidbouatra/Statistical software/1718S1_SAD_PEC1/DATOS/secProteinGroup")$size)
gsub("[\r\n]","","/Users/saidbouatra/Statistical software/1718S1_SAD_PEC1/DATOS/secProteinGroup")

install.packages("readr") ## esta es otra forma de lees el archivo.
library("readr")
secProteinGroup<-read_file("/Users/saidbouatra/Statistical software/1718S1_SAD_PEC1/DATOS/secProteinGroup")

nchar(secProteinGroup) ## son 450 caracteres ( incluido el "\n").podemos eliminar este caracter utilizando la funcion "strsplit()".
nchar(strsplit(secProteinGroup,"\n")) ## entonces serian 449 caracters.
table(strsplit(secProteinGroup,split=character(0))) ## la frecuencia de aparicion de cada caracter.
as.vector(unlist (lapply(gregexpr("EE",secProteinGroup),length))) ## El patron "EE" se repite 4 veces.

## Sección 3. Estadística descriptiva y gráficos
## a) Realiza un resumen de los parámetros estadísticos básicos de stressEchoDataSet y comenta los resultados obtenidos.
summary(stressEchoDataSet)## el estudio se realizo en 220 hombres y 338 mujers de edades comprendidas entre 26 y 93.
                        ## la dosis min es 10 (desconozco las unidades), l la maxima es 40 . la dosis media es de 33.75.


## b)Realiza un resumen de los parámetros estadísticos básicos, un boxplot, un histograma y un diagrama de densidad de la variable age y comenta los resultados.
summary(stressEchoDataSet$age)
boxplot(stressEchoDataSet$age)
hist(stressEchoDataSet$age)
plot(density(stressEchoDataSet$age),col= "blue",main = "Distribution of age")

##c)Realiza un gráfico de barras (qqplot) para la variable dose de stressEchoDataSet, usando la librería ggplot2. La división ha de ser de 5 unidades, el título del gráfico será ‘Distribución dosis’ y el color de las barras será rojo.
library("ggplot2")
attach(stressEchoDataSet)
qplot(stressEchoDataSet$dose,geom="bar",breaks=8,xlab= "dosis",main ="Distribucion dosis",col="red")

##d)Realizar un gráfico final conjunto de los gráficos anteriores.
par(mfrow=c(2,2))
boxplot(stressEchoDataSet$age)
hist(stressEchoDataSet$age)
plot(density(stressEchoDataSet$age),col= "blue",main = "Distribution of age")
qplot(stressEchoDataSet$dose,geom="bar",xlab= "dosis",main ="Distribucion dosis",col="red")## no consegui incluir esta grafica en el conjunto .

pdf("PEC1_BOUATRA_BELOUAFI_SAID")
getwd()
