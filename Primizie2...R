library("zoo")
library("plot3D")
library("magick")
library("rgl")
library("tseries")
library("lmtest")
library(car)
library(DAAG)
library(corrplot)
library(tidyverse)
library(corrgram)
library(AST)
library(STAT)
library('ggplot2')
library('forecast')
library('tseries')
library("data.table")
library(readxl)
library(readr)

#DATI

####Dataset utilizzato e selezione degli anni di interesse.

#(campione_CD <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/durevoli.csv")[22:56])
#(campione_C_ND <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/non_durevoli.csv")[22:56])
#(campione_CD <- colSums(campione_CD))
#(campione_C_ND <- colSums(campione_C_ND))
#(campione_C <- campione_C_ND + campione_CD)
#(campione_C <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/Uff2/COOnsumo.csv"))
(campione_C <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/Uff2/COOOOnsumi.csv"))
#(campione_Y <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/gdp_ita.csv")[22:56])
(campione_YD <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/Uff2/YD.csv"))
#reddito_disponibile_deflazionato <- read_csv("C:/Users/39338/Downloads/reddito disponibile deflazionato.csv")
#reddito_disponibile_deflazionato=reddito_disponibile_deflazionato[-c(4,5),]
#(campione_T <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/gettito2.csv")[6:40])
(campione_DP <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/Uff2/IPC_Eurostat.csv"))
#(campione_DP <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/inflazione.csv"))
#(campione_DP <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/inflazione.csv")[26:130])
#(loans22 <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/Uff2/loans to households for consumption & other purposes .csv"))
(Interest_Rates_households <- read_excel("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/Uff2/Interest Rates households.xlsx"))
#(campione_i <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/estat_irt_st_a_filtered.csv")[4,])

#AGG: Wealth
#AGG: (...)

####Le variabili di interesse.

##Consumi 
#trimestrali
(C = as.numeric(campione_C))
(C=C[9:40])
#annuali
a = c()
Cann <- c()
for(i in 1:length(C)){
  if(length(a)==4){
  Cann <- c(Cann, sum(a))
  a <- c()
  }
  else{
    a <- c(a,C[i])
  }
}
Cann
(Cann=c(Cann,961090.5))

##Reddito disponibile

#trimestrale
(YD = as.data.frame(campione_YD))
(YD=YD[-c(1),])
(YD=as.numeric(YD)[6:37])

#annuale
a = c()
YDann <- c()
for(i in 1:length(YD)){
  if(length(a)==4){
    YDann <- c(YDann, sum(a))
    a <- c()
  }
  else{
    a <- c(a,YD[i])
  }
}
YDann
(YDann=c(YDann,1105291))

##Prezzi

#mensili
(IPC=as.numeric(campione_DP))

#trimestrali
a = c()
IPC <- c()
for(i in 1:length(campione_DP)){
  if(length(a)==3){
    IPC <- c(IPC, mean(as.numeric(a)))
    a <- c()
  }
  else{
    a <- c(a,campione_DP[i])
  }
}
#base 2015
(IPC=IPC/100)
(IPC=IPC[7:38])
#base 2000
(Icambiobase=1/0.7346667)
(IPC2000=IPC*Icambiobase)

##Inflazione

#??(inflazione=IPC2000-1)

##Tasso di interesse reale

#trimestrali
(idata.frame=as.data.frame(Interest_Rates_households))
(i=idata.frame$Tassi)
(ii=t(i)[51:85])
(ii=as.numeric(rev(ii)))
(ii=ii[1:32])
#(...)
#(r = ii-inflazione)

#Tempo

Time <- 1:length(YD)


####La gestione delle variabili d'interesse.

##La trasformazione da valori a prezzi correnti a valori a prezzi costanti

(C = C/IPC2000)
(YD=YD/IPC2000)

##La trasformazione in variazioni logaritmiche.

(logC = log(C))
(logYD = log(YD))
#(logr=log(r))

#Le differenze prime.

(DC1=diff(logC))
(DYD1=diff(logYD))
#(Dr=diff(logr))

##Shift differenze prime per l'autoregressione.

(lagDC <- shift(DC))
(lagDYD = shift(DYD))
#(lagDr <- shift(Dr))


####Rappresentazione delle variabili di interesse.

##Rappresentazione tabellare

(provaC<- ts(C, start = 1999, frequency=4))
(provaYD<- ts(YD, start = 1999, frequency=4))
(prova_ii<-ts(ii, start=1999, end=2008, frequency = 4))
#(prova_r<-ts(r, start=1999, end=2008, frequency = 4))

##Rappresentazione grafica

#######PER DATI NON DESTAGIONALIZZATI?#########

#observed, trend, seasonal, random
(provadecC=decompose(provaC, type = "additive"))
plot(provadecC)
(provadecYD=decompose(provaYD, type = "additive"))
plot(provadecYD)
(provadecii=decompose(prova_ii, type = "additive"))
plot(provadecii)
#(provadecr=decompose(prova_r, type = "additive"))
#plot(provadecr)

#observed
plot(provadecC$x,col="Royal Blue")
par(new=T)
plot(provadecYD$x, col="Red")
abline(v=1999:2007, lty=2, lwd=0.3)
#trend
plot(provadecC$trend,col="Royal Blue")
par(new=T)
plot(provadecYD$trend, col="Red")
abline(v=1999:2007, lty=2, lwd=0.3)
#seasonality
plot(provadecC$seasonal,col="Royal Blue")
par(new=T)
plot(provadecYD$seasonal, col="red")
abline(v=1999:2007, lty=2, lwd=0.3)
#random
plot(provadecC$random, col="Royal Blue")
par(new=T)
plot(provadecYD$random, col="red")

#Confronto
ts.plot(provadecC$x,provadecC$trend, main="Confronto")
ts.plot(provadecYD$x,provadecYD$trend, main="Confronto")
#oppure
plot(provadecC$x,col="Royal Blue")
lines(provadecC$trend, col=3)
abline(v=c(1999:2007), lwd=0.3,lty=4)

#Imparare ad usare GGPLOT2

#Rappresentazione grafica dell'oggetto di analisi del modello.

dddtime=1:31
plot(dddtime, DC, type="l", col="Royal Blue")
par(new=T)
plot(dddtime, DYD, type="l", col="Red")
abline(h=0)

plot(dddtime, DC, type="l", col="Royal Blue")
par(new=T)
#plot(dddtime, Dr, type="l", col="Violet")
abline(h=0)


#LA COSTRUZIONE DEL MODELLO

#I FASE: la determinazione della forma funzionale.

#1. DC=b0+b1lagDC+b2DYD+b3Dr
#2. retta di regressione

#II FASE: la scelta del modello e stima dei parametri.

####Modelli a confronto.

#Modello DConsumo-DReddito

VregrCY <- lm(DC~ DYD)
summary(VregrCY)

range(DC)
range(DYD)
plot(DYD,DC,xlim = c(-0.030,0.045),ylim=c(-0.030,0.045))
abline(VregrCY)
abline(h=0,v=0,col="Dark Gray")

#Modello DConsumo-DReddito-Dr
#(...)

#Modello AR

ARregr <- lm(DC~lagDC)
summary(ARregr)

ARregr <- lm(DC~lagDC + shift(lagDC))
summary(Aregr)

#lag ottimale
lm(DC~lagDC)
lag(lagDC)
BIC(lm(DC~lagDC))
BIC(lm(DC~lag(lagDC)))
BIC(lm(DC~lag(lag(lagDC))))

####L'introduzione di variabili qualitative.
#Idee:
#I. riduzione delle asimmetria informativa;
#II. riduzione delle incompletezze di mercato;
#III. (...)


#Metodi di scelta del modello ottimale.

#Backward
#llagDC=lag(lagDC)
#lagDC=lagDC[3:38]
#llagDC=llagDC[3:38]
#DC=DC[3:38]
#DYD=DYD[3:38]
#Dr=Dr[3:38]
#completo=lm(DC~lagDC+llagDC+DYD+Dr)
#step(completo, direction = "backward")

#oppure, guardando al p-value..
#VregrMCRt_perc1<-update(VregrMCRt_perc, . ~ .-Dr) 
#summary(VregrMCRt_perc1)
#VregrMCRt_perc2<-update(VregrMCRt_perc1, . ~ .-lag(lagDC)) 
#(summary(VregrMCRt_perc2))

#Forward
#base=lm(DC~DYD)
#step(base, scope = formula(completo), direction = "forward")

#Stepwise
#step(completo, direction = "both")

#Test ANOVA per confrontare i diversi modelli ottenuti
#anova(VregrMCRt_perc,VregrMCRt_perc1) 
#anova(VregrMCRt_perc1,VregrMCRt_perc2)

#++Metodo Prof. ultima lezione

####Modello scelto

modello <- lm(DC~lagDC+lag(lagDC)+ DYD)
#+Dr

###I risultati del modello
(Analisi=summary(modello))

#Gli intervalli di confidenza per ciascun parametro.

confint(modello,level=0.95)

lm.beta(modello)

#Gli intervalli di confidenza per ciascun valore di DC.

(conf<-predict(modello, level=0.99, interval="confidence"))

#Gli intervalli di predizione per ciascun valore di DC.

(conf<-predict(modello, level=0.99, interval="prediction"))

#(newdata=data.frame(DC=c(0.44))
#(prev<-predict(VregrMCRt_perc, newdata = newdata, level=0.99, interval="prediction"))

#Test ANOVA su ciascuna variabile indipendente

anova(modello)

###Analisi grafica

scatterplotMatrix(~DC+DYD+lagDC+lag(lagDC), col="black", pch=20, regLine = list(method=lm, lty=1, lwd=2, col="chartreuse3"), smooth=FALSE, diagonal=list(method ="histogram", breaks="FD"), main="Matrice di dispersione con rette di regressione", data=ais) 

plot(DYD,DC,xlim = c(-0.030,0.045),ylim=c(-0.030,0.045),pch=1, cex=1)
abline(a=modello$coefficients[1],b=modello$coefficients[4])
abline(h=0,v=0,col="Dark Gray")

plot(lagDC,DC,xlim = c(-0.030,0.045),ylim=c(-0.030,0.045))
abline(a=modello$coefficients[1],b=modello$coefficients[2])
abline(h=0,v=0,col="Dark Gray")

plot(lag(lagDC),DC,xlim = c(-0.030,0.045),ylim=c(-0.030,0.045))
abline(a=modello$coefficients[1],b=modello$coefficients[3])
abline(h=0,v=0,col="Dark Gray")


#III FASE: la validazione del modello.

###Analisi di stazionarieta'

adf.test(DC)
plot.ts(DC, col="red")
adf.test(DYD)
par(new=TRUE)
plot.ts(DYD, col="green")
abline(c(a=0,b=0))

###L'analisi dei residui

(residui=summary(modello)$residuals)
par(mfrow=c(2,2))
plot(modello)

#errori a media nulla

t.test(residui)

par(mfrow=c(1,1))
plot(modello$fitted.values,modello$residuals)
abline(c(a=0,b=0))

#errori omoschedastici

bptest(modello)

#errori con distribuzione normale

shapiro.test(residui)

qqnorm(residui)
qqline(residui)

jarque.bera.test(residui)

hist(residui, prob=T, main="Distribuzione dei residui", xlab="Residui")
par(new=T)
plot(density(residui,kernel="gaussian"),main="Distribuzione dei residui:
lisciamento")
#sovrapporre

#errori non autocorrelati

dwtest(modello)
#ooppure
durbinWatsonTest(modello)

#..correlogramma

acf(residui, lag.max = 39, main="Correlogramma dei residui")
acf(residui, main="Correlogramma dei residui")
Pacf(residui, main="Correlogramma dei residui")


###Analisi del valore informativo di ciascuna variabile

#VIF

vif(modello)

#commento VIF:
#Il vif misura la collinearita' tra le varibili indipendenti,
#in particolare, al cresce della collinearita' cresce il vif e,
#per valori superiori a 15, solitamente, conviene riconsiderare
#le variabili esplicative utilizzate.

###Gestione degli outliers
#Come si interpreta?

outlierTest(modello)

#UN'APPPROFONDIMENTO..

###analisi su caratteristiche e relazione tra C e Y
###Studiare bene la correlazione parziale e integrarla

#Y prociclica
(r = cor(DC,DYD))
(r^2)

#bozza: Y leading o logging?
#ciclo for

#..grangertest(DC~DY, order=2)

#Traslazione della serie di k (valori positivi (verde) anticipano; valori negativi (rosso) posticipano)
#ts.xy_a12 <-lag(ts.xy, k = 12); ts.xy_r12 <-lag(ts.xy, k = -12)
#plot.ts(ts.xy, lwd=1.5, xlim = c(1999,2012))
#lines(ts.xy_a12, col=3); lines(ts.xy_r12, col=2); abline(v=c(1999:2001,2009:2011), lwd=0.3,lty=4)

#bozza: persistenza di C e Y attraverso l'autocorrelazione
#plot(c(1:length(C)), C)
#plot(c(1:length(Y)), Y)

#bozza: variabilita' di C e Y
#var(DC)
#var(DY)
#var(DC)/var(DY)

###bozza analisi della correlazione nel tempo

#(cor(dati1))
#corrplot(cor(dati1), method = "pie")

##bozza organizzazione risultati
#dataframe riassuntivo

#(dati2=data.frame(VariazioniC=DC, VariazioniY=DY, Variazioni_stimateC=Vregr$fitted.values, Residui=residui))

#plot(Y,C, col='red')
#par(new=T)
#plot(arima.sim(model=list(order(1,1,4)), n=39))

####PREVISIONI

#Descrizione modello
#VregrMCRt_perc <- lm(shift(DC)~ shift(DY) + shift(Dr) + shift(lagDC))
#I risultati del modello
#(DEF=summary(VregrMCRt_perc))
#coeff=summary(VregrMCRt_perc)$coefficients
#DC[[38]]
#predict(VregrMCRt_perc, c(shift(DY)[[37]],shift(Dr)[[37]],shift(lagDC)[[37]]))
#(Y = -0.0001977+0.5664152*shift(DY)[[37]]+0.021624*shift(Dr)[[37]]-0.0452063*shift(lagDC)[[37]])
#Descrizione modello
#VregrMCRt_perc <- lm(shift(DC)~ shift(DY) + shift(Dr))
#I risultati del modello
#(DEF=summary(VregrMCRt_perc))
#coeff=summary(VregrMCRt_perc)$coefficients
#DC[[38]]
#predict(VregrMCRt_perc, c(shift(DY)[[37]],shift(Dr)[[37]],shift(lagDC)[[37]]))
#(Y = 0.001551+0.580182*shift(DY)[[37]]+0.016298*shift(Dr)[[37]])
