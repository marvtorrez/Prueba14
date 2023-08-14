#Establecer el area de trabajo

setwd("G:/clases/UCC/2023_ucc_estad/Semana 7 agosto")
library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(glmm.hp)

al <- read_excel("alim.xlsx")
View(ed)
names(al)

# agregar la funciónn attach, Permite referenciar los nombres de las columnas de los data.frames

attach(al)
al1<-data.frame(D06, D03)

# p > 0.05, nos indica una distribución normal

shapiro.test(D02)

shapiro.test(D03) 

# un p < 0.05 indica que se acepta la asunción que existen diferencias o sea que la variables no son homg?neas

var.test(Femenina, Masculino)

g1<-ggplot(al1, aes(D03)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g2<-ggplot(al1, aes(log(D03)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(al1, aes(sqrt(D03)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g1
g2
g3

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

d2=al1$D02
d3=al1$D03

boxplot(d2,d3,
        main = "HPG",
        names = c("2", "3"),
        las = 1,
        col = c("orange","red"),
        border = "brown",
        Vertical = TRUE,
        notch = FALSE)

#podemos observar que los datos no cumplen normalidad


t.test(log(D02), log(D03),alternative="two.sided",var.equal=F) 


#Haremos una an?lisis bayesiano

install.packages("BayesFactor")
library(BayesFactor)

?BayesFactor
bf = ttestBF(Femenina, Masculino, paired = TRUE);bf

# un puntaje mayor de 10 se dice que la evidencia es anecdotical en favor de la Ho
# Haremos un gr?fico para ver estos dos promedios

### OTro ejemplo

ph <- read_excel("phi.xlsx")
View(ph)
names(ph)

# agregar la funciónn attach, Permite referenciar los nombres de las columnas de los data.frames

attach(ph)
ph1<-data.frame(Fem, Ma)

# p > 0.05, nos indica una distribución normal

shapiro.test(Fem) 
shapiro.test(Ma) 

# un p < 0.05 indica que se acepta la asunción que existen diferencias o sea que la variables no son homg?neas

var.test(Femenina, Masculino)

g1<-ggplot(ph1, aes(Fem)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g2<-ggplot(ph1, aes(log(Fem)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(ph1, aes(sqrt(Fem)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g1
g2
g3

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

f1=ph1$Fem
m1=ph1$Ma

boxplot(f1,m1,
        main = "Phi",
        names = c("f1", "m1"),
        las = 1,
        col = c("orange","red"),
        border = "brown",
        Vertical = TRUE,
        notch = FALSE)

#podemos observar que los datos no cumplen normalidad


t.test(log(D03), log(D06),alternative="two.sided",var.equal=F) 


#Haremos una an?lisis bayesiano

install.packages("BayesFactor")
library(BayesFactor)

?BayesFactor
bf = ttestBF(Fem, Ma, paired = TRUE);bf

# un puntaje mayor de 10 se dice que la evidencia es anecdotical en favor de la Ho
# Haremos un gr?fico para ver estos dos promedios

#### Comparacion DI

ed <- read_excel("phi.xlsx")
View(ed)
names(ed)

# agregar la funciónn attach, Permite referenciar los nombres de las columnas de los data.frames

attach(ed)
pc<-data.frame(Fem, Ma)

# p > 0.05, nos indica una distribución normal

shapiro.test(Fem)

shapiro.test(Ma) 

# un p < 0.05 indica que se acepta la asunción que existen diferencias o sea que la variables no son homg?neas

var.test(Femenina, Masculino)

g1<-ggplot(ed, aes(Ma)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g2<-ggplot(ed, aes(log(Ma)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(ed, aes(sqrt(Ma)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g1
g2
g3

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

f=ed$Fem
m=ed$Ma

boxplot(f,m,
        main = "Edad",
        names = c("f", "m"),
        las = 1,
        col = c("orange","red"),
        border = "brown",
        Vertical = TRUE,
        notch = FALSE)

#podemos observar que los datos no cumplen normalidad


t.test(Fem, Ma,alternative="two.sided",var.equal=F) 


#Haremos una an?lisis bayesiano

install.packages("BayesFactor")
library(BayesFactor)

?BayesFactor
bf = ttestBF(Femenina, Masculino, paired = TRUE);bf

# un puntaje mayor de 10 se dice que la evidencia es anecdotical en favor de la Ho
# Haremos un gr?fico para ver estos dos promedios

### OTro ejemplo

ph <- read_excel("phi.xlsx")
View(ph)
names(ph)

# agregar la funciónn attach, Permite referenciar los nombres de las columnas de los data.frames

attach(ph)
ph2<-data.frame(di_F,di_M)

# p > 0.05, nos indica una distribución normal

shapiro.test(di_F) 
shapiro.test(di_M) 

# un p < 0.05 indica que se acepta la asunción que existen diferencias o sea que la variables no son homg?neas

var.test(Femenina, Masculino)

g1<-ggplot(ph2, aes(di_M)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g2<-ggplot(ph2, aes(log(di_M)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(ph2, aes(sqrt(di_M)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g1
g2
g3

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

f1=ph1$Fem
m1=ph1$Ma

boxplot(di_F,di_M,
        main = "Phi",
        names = c("f1", "m1"),
        las = 1,
        col = c("orange","red"),
        border = "brown",
        Vertical = TRUE,
        notch = FALSE)

#podemos observar que los datos no cumplen normalidad


t.test(log(di_F), sqrt(di_M),alternative="two.sided",var.equal=F) 


#Haremos una an?lisis bayesiano

install.packages("BayesFactor")
library(BayesFactor)

?BayesFactor
bf = ttestBF(Fem, Ma, paired = TRUE);bf

## pruebra 3

library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(glmm.hp)

hp <- read_excel("HPG.xlsx")
View(ed)
names(hp)

# agregar la funciónn attach, Permite referenciar los nombres de las columnas de los data.frames

attach(hp)
hp1<-data.frame(Cuajachillo, Trinidad)

# p > 0.05, nos indica una distribución normal

shapiro.test(Fem)

shapiro.test(Ma) 

# un p < 0.05 indica que se acepta la asunción que existen diferencias o sea que la variables no son homg?neas

var.test(Femenina, Masculino)

g1<-ggplot(ed, aes(Ma)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g2<-ggplot(ed, aes(log(Ma)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(ed, aes(sqrt(Ma)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g1
g2
g3

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

f=ed$Fem
m=ed$Ma

boxplot(f,m,
        main = "Edad",
        names = c("f", "m"),
        las = 1,
        col = c("orange","red"),
        border = "brown",
        Vertical = TRUE,
        notch = FALSE)

#podemos observar que los datos no cumplen normalidad


t.test(Fem, Ma,alternative="two.sided",var.equal=F) 


#Haremos una an?lisis bayesiano

install.packages("BayesFactor")
library(BayesFactor)

?BayesFactor
bf = ttestBF(Femenina, Masculino, paired = TRUE);bf

# un puntaje mayor de 10 se dice que la evidencia es anecdotical en favor de la Ho
# Haremos un gr?fico para ver estos dos promedios