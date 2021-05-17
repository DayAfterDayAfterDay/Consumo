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

#DATI

#####Dati annuali.
#####Dati mensili.

#####Dati trimestrali.

####Daset utilizzato

(campione_CD <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/durevoli.csv")[18:56])
(campione_C_ND <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/non_durevoli.csv")[18:56])

(campione_CD <- colSums(campione_CD))
(campione_C_ND <- colSums(campione_C_ND))
(campione_C <- campione_C_ND + campione_CD)

(campione_Y <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/gdp_ita.csv")[18:56])
(campione_T <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/gettito2.csv")[2:40])
(campione_DP <- read.csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/inflazione.csv")[14:130])
(campione_i <- read_csv("C:/Users/39338/Desktop/Universita' Secondo Anno/Statistica/Applicativo R/Progetto Ricerca/Tesina/UFFICIALE/Consumo-main/estat_irt_st_a_filtered.csv")[4,])
#AGG: Wealth
#AGG: (...)

####Le variabili di interesse.

#Consumi

(C = as.numeric(campione_C))

#Reddito

(Y = as.numeric(campione_Y))

#Tasse e pressione fiscale

(Tax = as.numeric(campione_T)*2)
(t_perc <- Tax/Y)

#Reddito disponibile

(YD = Y-(Tax))

#Tassi d'interesse

ii=as.numeric(campione_i)[15:24]
i2 = c()
for(k in ii){
  i2 <- c(i2, rep(k, 4))
}
(ii <- i2[1:39])

#Prezzi

a = c()
infl <- c()
counter <- 0
for(i in 1:length(campione_DP)){
  counter <- counter+1
  if(counter%%3==0){
    counter <- 0
    infl <- c(infl, prod(as.numeric(a)))
    a <- c()
  }
  else{
    a <- c(a,campione_DP[i])
  }
}
(DP <- infl)
(infl <- infl+1)

#Tempo

Time <- 1:length(Y)

####La gestione delle variabili d'interesse.

##La trasformazione in variabili reali.

(C = C/infl)
(Y = Y/infl)
(Tax = Tax/infl)
(r = ii-DP)

##La trasformazione in variabili logaritmiche.

logC = log(C)
logY = log(Y)
logYD = log(Y)
#logT = log(Tax)
#logt_perc = log(t_perc)
logt_perc2 <- log(t_perc+.14) 
#logY = logY+log(1-t_perc)
#(logr=log(r))

##Il calcolo delle differenze prime di ciascuna variabile.

#(Dr=diff(logr)[1:38])
Dr=diff(r)
DC = c()
DY = c()
DYD = c()
DT = c()
Dt_perc = c()
Dt_perc2 = c()
n = length(logY)

for(i in 1:n-1){
  DC <- c(DC, c(logC[i+1]-logC[i]))
}
for(i in 1:n-1){
  DY <- c(DY, c(logY[i+1]-logY[i]))
}
for(i in 1:n-1){
  DYD <- c(DYD, c(logYD[i+1]-logYD[i]))
}
for(i in 1:n-1){
  DT <- c(DT, c(logT[i+1]-logT[i]))
}
for(i in 1:n-1){
  Dt_perc <- c(Dt_perc, c(logt_perc[i+1]-logt_perc[i]))
}
for(i in 1:n-1){
  Dt_perc2 <- c(Dt_perc2, c(logt_perc2[i+1]-logt_perc2[i]))
}

Dtime = 1:length(DC)

# Shift differenze prime per l'autoregressione.

(lagDC <- shift(DC))
(lagDY <- shift(DY))
(lagDYD = shift(DYD))
(lagDT <- shift(DT))
(lagDr <- shift(Dr))

##Il calcolo delle medie mobili di ciascuna variabile.
#DCM = c(rollmean(DC, 4),rollmean(DC, 4)[32:34] )
#DTM = c(rollmean(DT, 4),rollmean(DT, 4)[32:34] )
#DYM = c(rollmean(DY, 4),rollmean(DY, 4)[32:34] )


####Rappresentazione delle variabili di interesse.

##Rappresentazione tabellare

#Adattare dataframe come modellato dal documento..

#Dati trimestrali
(provaC<- ts(C, start = 1999, frequency=4))
(provaY<- ts(Y, start = 1999, frequency=4))
(provaT<- ts(Tax, start = 1999, frequency=4))
(provat_perc<- ts(t_perc, start = 1999, frequency=4))
(prova_i<-ts(i, start=1999, end=2008, frequency = 4))

#Dati annuali

#Cy=c()
#counter <- 0
#for(i in 1:length(C)){
#counter <- counter+1
#if(counter%%4==0){
#counter <- 0
#Cy <- c(C[i]+C[i+1]+C[i+2]+C[i+3])
#}
#}

#(prova1C<- ts(C, start = 1999, end=2008, frequency=1))
#(prova1Y<- ts(Y, start = 1999, end=2008,frequency=1))
#(prova1T<- ts(Tax, start = 1999,  end=2008, frequency=1))
#(prova1t_perc<- ts(t_perc, start = 1999, end=2008, frequency=1))
#(prova1_i<-ts(i, start=1999, end=2008, frequency =1))

#as.ts ; is.ts

#???????
#(dati <- data.frame(consumo=c(C), reddito=c(Y), tasse=c(Tax)))
#matrici
#(datimat <- as.matrix(dati))

#liste

##Metodi di decomposizione delle serie storiche

##metodo 0

#observed, trend, seasonal, random
(provadecC=decompose(provaC, type = "additive"))
plot(provadecC)
(provadecY=decompose(provaY, type = "additive"))
plot(provadecY)
(provadecT=decompose(provaT, type = "additive"))
plot(provadecT)
#(provadect_perc=decompose(provat_perc, type = "additive"))
#plot(provadect_perc)

#observed
#trend
#seasonal
plot(provadecC$seasonal)
par(new=T)
plot(provadecY$seasonal, col="red")
par(new=T)
plot(provadecT$seasonal, col="green")
#random

###????
ts.plot(provaC, col=4)
abline(v=1999:2009, lty=2, lwd=0.3)
ts.plot(provadecC$x,provadecC$trend,provadecC$seasonal,provadecC$random, main="Confronto")
ts.plot(provadecY$x,provadecY$trend,provadecY$seasonal,provadecY$random, main="Confronto")

plot.new()
plot(provadecC$x)
lines(provadecC$trend, col=3)
lines(provadecC$seasonal, col=4)
abline(v=c(1999:2001,2009:2011), lwd=0.3,lty=4)

plot(provadecC$random)

#random=differenze prime?

#metodo 1 "loess"

(prova1decC=stl(provaC, s.window = "periodic"))
plot(prova1decC, main = "Decomposizione C con la funzione stl")
(prova1decY=stl(provaY, s.window = "periodic"))
plot(prova1decY, main = "Decomposizione Y con la funzione stl")
(prova1decT=stl(provaT, s.window = "periodic"))
plot(prova1decT, main = "Decomposizione T con la funzione stl")
(prova1dect_perc=stl(provat_perc, s.window = "periodic"))
plot(prova1dect_perc, main = "Decomposizione T con la funzione stl")

# metodo 2 tramite funzione tsr()...

#(prova2decC=tsr(provaC~poly(1)+c))
#plot(prova2decC)
#(prova2decY=tsr(provaY~poly(1)+c))
#plot(prova2decY, main = "Decomposizione Y con la funzione stl")
#(prova2decT=tsr(provaT~poly(1)+c))
#plot(prova2decT, main = "Decomposizione T con la funzione stl")

#(..risolvere..)

#Confronto tra metodi di decomposizione

trend.stl=prova1decC$time.series[,2]

plot(provadecC$trend,main="Comparazione del trend stimato")
lines(trend.stl, col="blue")

#USARE GGPLOT2

##Lisciamento delle serie storiche..(pg.20)

##Analisi di eventuali trend nella stagionalita'..(pg.22ss.)

##Rappresentazioni grafiche

#Valori assoluti
plot(Time, C)
plot(Time,Y)
plot(Time, Tax)

#Valori logaritmici
plot(Time, logC)
plot(Time, logY)
plot(Time, logT)

#Differenze prime

plot(Dtime, DCM, type="l", col="red")
abline(a=0, b=0, col = "black")

plot(Dtime, DYM, type = "l", col='green')
abline(a=0, b=0, col = "black")

plot(Dtime, DTM, type="l", col='blue')
abline(a=0, b=0, col = "black")


#LA COSTRUZIONE DEL MODELLO

#I FASE: la determinazione della forma funzionale.
#--
#1. DC=f(DY,DTax) 

####Lo studio della funzione ipotizzata

eq = function(b,b1,b2,x=0,z=0){b+b1*log(x)+2*b1*log(x)+b2*log((1-z))}
eq1 = function(b,b1,x){b+b1*log(x)}
der <- function(b1,b2,x,z){b1*(1/x)-b2*(1/(1-z))}
plot(eq(b=3.552e+04,b1=2.523e-01,b2=1.803e+00,x=Y,z=t_perc), type='l')
plot(eq1(b=3.552e+04,b1=2.523e-01,x=Y), type="l", col="red")
plot(der(b1=2.523e-01,b2=1.803e+00,x=Y,z=t_perc), type="l", col="green")
plot3d(Y, t_perc, C)
play3d(spin3d(axis = c(0, 0, 1), rpm = 1), duration = 100000)

#2. retta di regressione
#--
#1. DC=f(DY,DTax)
#2. curva di regressione
#--

#II FASE: la stima dei parametri.

###Regressione in termini assoluti

plot(Y, C)
AregrY <- lm(C ~ Y)
summary(AregrY)
abline(c(AregrY$coefficients[1],AregrY$coefficients[2]))

plot(Tax, C)
AregrT <- lm(C ~ Tax)
summary(AregrT)
abline(c(AregrT$coefficients[1],AregrT$coefficients[2]))

plot(t_perc, C)
Aregrt <- lm(C ~ t_perc)
summary(Aregrt)
abline(c(Aregrt$coefficients[1],Aregrt$coefficients[2]))

AregrMT <- lm(C ~ Y + Tax)
summary(AregrMT)

(Ab00=AregrMT$coefficients[1])
(Ab11=AregrMT$coefficients[2])
(Ab22=AregrMT$coefficients[3])

plot3d(Y, Tax, C)
play3d(spin3d(axis = c(0, 0, 1), rpm = 3), duration = 100000)

AregrMt <- lm(C ~ Y + t_perc)
summary(AregrMt)

(Ab00=AregrMt$coefficients[1])
(Ab11=AregrMt$coefficients[2])
(Ab22=AregrMt$coefficients[3])

plot3d(Y, t_perc, C)
play3d(spin3d(axis = c(0, 0, 1), rpm = 3), duration = 100000)


###Regressione in logaritmo
plot(logY, logC)
LregrY <- lm(logC ~ logY)
summary(LregrY)
abline(c(LregrY$coefficients[1],LregrY$coefficients[2]))

plot(logT, logC)
LregrT <- lm(logC ~ logT)
summary(LregrT)
abline(c(LregrT$coefficients[1],LregrT$coefficients[2]))

logt=log(t_perc)
plot(logt, C)
Lregrt <- lm(logC ~logt)
summary(Lregrt)
abline(c(Lregrt$coefficients[1],Lregrt$coefficients[2]))

LregrM <- lm(logC ~ logY + logT)
summary(Lregr)

Lb00=LregrM$coefficients[1]
Lb11=LregrM$coefficients[2]
Lb22=LregrM$coefficients[3]

plot3d(logY, logT, logC)
play3d(spin3d(axis = c(0, 0, 1), rpm = 3), duration = 100000)

LregrMt <- lm(C ~ Y + t_perc)
summary(LregrMt)

(Ab00=AregrMt$coefficients[1])
(Ab11=AregrMt$coefficients[2])
(Ab22=AregrMt$coefficients[3])

plot3d(Y, t_perc, C)
play3d(spin3d(axis = c(0, 0, 1), rpm = 3), duration = 100000)

###Regressione in variazioni

#Modello AR

plot(DYD, DC)
par(new=T)
plot(DYD, lagDC, col="red")


Aregr <- lm(DC~lagDC + shift(lagDC))
summary(Aregr)

##lag ottimale
lm(DC~lagDC)
lag(lagDC)
BIC(lm(DC~lagDC))
BIC(lm(DC~lag(lagDC)))
BIC(lm(DC~lag(lag(lagDC))))
BIC(lm(DC~lag(lag(lag(lagDC)))))

#Consumo-Reddito

VregrCR <- lm(DC~ DY)
summary(VregrCR)

VCRb00=VregrCR$coefficients[1]
VCRb11=VregrCR$coefficients[2]

plot(DY,DC)
abline(c(VCRb00,VCRb11))
#abline(c(a=0, b=0))
#abline(c(a=0, b=mean(Y)))

#Consumo-TasseM

VregrCTM <- lm(DC ~ DTM)
summary(VregrCTM)

VCTMb00=VregrCTM$coefficients[1]
VCTMb11=VregrCTM$coefficients[2]

plot(DTM,DC)
abline(c(VCTMb00,VCTMb11))
#abline(c(a=0, b=0))
#abline(c(a=0, b=mean(Y)))

#SCELTA DEL MODELLO.

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

VregrMCRt_perc1<-update(VregrMCRt_perc, . ~ .-Dr) 
summary(VregrMCRt_perc1)

VregrMCRt_perc2<-update(VregrMCRt_perc1, . ~ .-lag(lagDC)) 
(summary(VregrMCRt_perc2))

#Forward
#base=lm(DC~DYD)
#step(base, scope = formula(completo), direction = "forward")

#Stepwise

step(completo, direction = "both")

#MODELLO SCELTO: Consumo-Reddito-pressione fiscale
#domanda..non si dovrebbero considerare anche gli effetti combinati causati dalla variazione di due o piu' variabili contemporaneamente?

#Descrizione modello
VregrMCRt_perc <- lm(DC~lagDC + lag(lagDC)+ DYD + Dr)
#-valori osservati
#b1 = cov(DY, DC)/var(DY)
#b1
#DC
#-valori stimati
#VregrMCRt_perc$fitted.values
#-residui
#VregrMCRt_perc$residuals

#I risultati del modello

(DEF=summary(VregrMCRt_perc))
(coeff=summary(VregrMCRt_perc)$coefficients)

#domanda: intercetta=trend indipendente dalle variabili esplicative??

plot(VregrMCRt_perc)
scatterplotMatrix(~DC+DY+lagDC+lag(lagDC), col="black", pch=20, regLine = list(method=lm, lty=1, lwd=2, col="chartreuse3"), smooth=FALSE, diagonal=list(method ="histogram", breaks="FD"), main="Matrice di dispersione con rette di regressione", data=ais) 


plot(DY,DC,pch=1, cex=1)
abline(c(coeff[1],coeff[4]))
abline(h=0,v=0)

#...
plot(Dr,DC,pch=1, cex=1)
abline(c(coeff[1],coeff[5]))
abline(h=0,v=0)

#Consumo-Consumo(t-1)-Reddito-TasseM
#(DCC <- c(DC[[1]], DC[1:37]))
#(DCCC <- c(DC[1:2], DC[1:36]))
#(DCCCC <- c(DC[1:3], DC[1:35]))
#(DTT <- c(DT[[1]], DT[1:37]))
#(DYY <- c(DY[[1]], DY[1:37]))

#Vregr <- lm(DC~ DY + DYY + DT + DTT + DCC+ DCCC + DCCCC)
#summary(Vregr)

#plot(DY,DC)
#Vb000=Vregr$coefficients[1]
#Vb111=Vregr$coefficients[2]
#abline(c(Vb000,Vb111))

#(DTT <- c(DT[[1]], DT[1:37]))
#(DYY <- c(DY[[1]], DY[1:37]))

#(...)

#commento dell'output:
#intervalli di confidenza per ciascun parametro.

confint(VregrMCRt_perc)
confint(VregrMCRt_perc, level=0.99)

#- analizzare la variabilita' degli stimatori b00 e b11 misurata dallo St.Error;
#- notiamo che, nel caso di b00, non rifiutiamo l'ipotesi che questa assuma il valore stimato;
#- notiamo che, nel caso di b11, rifiutiamo l'ipotesi che questa assuma il valore stimato;
#- come si interpreta lo standard error residuo?
#- notiamo che l'R^2 e' molto basso, sara' necessario aumentare il numero di variabili indipendenti per spiegare meglio il modello.

#Test ANOVA su ciascuna variabile indipendente

anova(VregrMCRt_perc)

#Test ANOVA per confrontare i diversi modelli ottenuti

anova(VregrMCRt_perc,VregrMCRt_perc1) 
anova(VregrMCRt_perc1,VregrMCRt_perc2) 

#- testata la normalita', calcolarsi e riportare gli intervalli di confidenza dei bk e Y;
#- plottare le rispettive distribuzioni normali
#- calcolare l'intervallo predittivo di Yn+1 e analizzarlo.

####L'introduzione di variabili qualitative.

#Idee:
#I. riduzione delle asimmetria informativa;
#II. riduzione delle incompletezze di mercato;
#III. (...)

#L'intervallo di confidenza di DC

(conf<-predict(VregrMCRt_perc, level=0.99, interval="confidence"))

#L'intervallo di previsione di DC
#(newdata=data.frame(DC=c(0.44))
#(prev<-predict(VregrMCRt_perc, newdata = newdata, level=0.99, interval="prediction"))

#III FASE: la validazione del modello.

###Analisi di stazionarieta'

adf.test(DC)
plot.ts(DC, col="red")
adf.test(DY)
par(new=TRUE)
plot.ts(DY, col="green")
abline(c(a=0,b=0))

###Test Anova sui parametri della regressione

anova(VregrMCRTM)

#commento dei risultati

###L'analisi dei residui

residui=summary(VregrMCRt_perc)$residuals

#errori a media nulla

t.test(residui)

#errori omoschedastici

bptest(Vregr)

plot(VregrMCRTM$fitted.values,VregrMCRTM$residuals)
abline(c(a=0,b=0))

#errori con distribuzione normale

shapiro.test(residui)

qqnorm(residui)
qqline(residui)

jarque.bera.test(residui)

hist(residui, prob=T, main="Distribuzione dei residui", xlab="Residui")
plot(density(residui,kernel="gaussian"),main="Distribuzione dei residui:
lisciamento")
#sovrapporre

#errori non autocorrelati

dwtest(VregrMCRt_perc)

durbinWatsonTest(VregrMCRt_perc)

#..correlogramma

acf(residui, lag.max = 39, main="Correlogramma dei residui")
acf(residui, main="Correlogramma dei residui")
Pacf(residui, main="Correlogramma dei residui")


###Analisi del valore informativo di ciascuna variabile

#VIF

vif(VregrMCRt_perc)

#commento VIF:
#Il vif misura la collinearita' tra le varibili indipendenti,
#in particolare, al cresce della collinearita' cresce il vif e,
#per valori superiori a 15, solitamente, conviene riconsiderare
#le variabili esplicative utilizzate.

###Gestione degli outliers
#Come si interpreta?

outlierTest(VregrMCRTM)

#ANALISI DEI RISULTATI DEL MODELLO.

###analisi su caratteristiche e relazione tra C e Y
###Studiare bene la correlazione parziale e integrarla

#Y prociclica
(r = cor(DC,DY))
(r^2)

#bozza: Y leading o logging?
#ciclo for

#..grangertest(DC~DY, order=2)

#Traslazione della serie di k (valori positivi (verde) anticipano; valori negativi (rosso) posticipano)
#ts.xy_a12 <-lag(ts.xy, k = 12); ts.xy_r12 <-lag(ts.xy, k = -12)
#plot.ts(ts.xy, lwd=1.5, xlim = c(1999,2012))
#lines(ts.xy_a12, col=3); lines(ts.xy_r12, col=2); abline(v=c(1999:2001,2009:2011), lwd=0.3,lty=4)

#bozza: persistenza di C e Y attraverso l'autocorrelazione
plot(c(1:length(C)), C)
plot(c(1:length(Y)), Y)

#bozza: variabilita' di C e Y
var(DC)
var(DY)
var(DC)/var(DY)

###bozza analisi della correlazione nel tempo

(cor(dati1))
corrplot(cor(dati1), method = "pie")

##bozza organizzazione risultati
#dataframe riassuntivo

(dati2=data.frame(VariazioniC=DC, VariazioniY=DY, Variazioni_stimateC=Vregr$fitted.values, Residui=residui))

plot(Y,C, col='red')
par(new=T)
plot(arima.sim(model=list(order(1,1,4)), n=39))

####PREVISIONI?????!

#Descrizione modello
VregrMCRt_perc <- lm(shift(DC)~ shift(DY) + shift(Dr) + shift(lagDC))
#I risultati del modello

(DEF=summary(VregrMCRt_perc))
coeff=summary(VregrMCRt_perc)$coefficients
DC[[38]]
predict(VregrMCRt_perc, c(shift(DY)[[37]],shift(Dr)[[37]],shift(lagDC)[[37]]))
(Y = -0.0001977+0.5664152*shift(DY)[[37]]+0.021624*shift(Dr)[[37]]-0.0452063*shift(lagDC)[[37]])

####PREVISIONI?????!

#Descrizione modello
VregrMCRt_perc <- lm(shift(DC)~ shift(DY) + shift(Dr))
#I risultati del modello

(DEF=summary(VregrMCRt_perc))
coeff=summary(VregrMCRt_perc)$coefficients
DC[[38]]
predict(VregrMCRt_perc, c(shift(DY)[[37]],shift(Dr)[[37]],shift(lagDC)[[37]]))
(Y = 0.001551+0.580182*shift(DY)[[37]]+0.016298*shift(Dr)[[37]])
