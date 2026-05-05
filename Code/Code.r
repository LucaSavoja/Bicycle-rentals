##Introduzione al dataset

##Importo il dataset
dati<-read.csv("day.csv", header = TRUE, sep = ",")
attach(dati)

#Modifichiamo le variabili e i valori delle variabili per rendere il dataset più comprensibile
dati$instant <- NULL
dati$season <- ifelse(dati$season == 1, "winter",
                  ifelse(dati$season == 2, "spring",
                    ifelse(dati$season == 3, "summer",
                      ifelse(dati$season == 4, "fall", dati$season))))
                      
dati <- dati[dati$yr == 1, ]

dati$yr <- NULL

dati$mnth <- ifelse(dati$mnth == 1, "January",
              ifelse(dati$mnth == 2, "February",
              ifelse(dati$mnth == 3, "March",
              ifelse(dati$mnth == 4, "April",
              ifelse(dati$mnth == 5, "May",
              ifelse(dati$mnth == 6, "June",
              ifelse(dati$mnth == 7, "July",
              ifelse(dati$mnth == 8, "August",
              ifelse(dati$mnth == 9, "September",
              ifelse(dati$mnth == 10, "October",
              ifelse(dati$mnth == 11, "November",
              ifelse(dati$mnth == 12, "December", dati$mnth))))))))))))


dati$holiday <- ifelse(dati$holiday == 0, "No",
                       ifelse(dati$holiday == 1, "Yes", dati$holiday))

dati$weekday <- ifelse(dati$weekday == 0, "Sunday",
                  ifelse(dati$weekday == 1, "Monday",
                  ifelse(dati$weekday == 2, "Tuesday",
                  ifelse(dati$weekday == 3, "Wednesday",
                  ifelse(dati$weekday == 4, "Thursday",
                  ifelse(dati$weekday == 5, "Friday",
                  ifelse(dati$weekday == 6, "Saturday", dati$weekday)))))))

dati$workingday <-  ifelse(dati$workingday == 0, "No",
                       ifelse(dati$workingday == 1, "Yes", dati$workingday))
                       
                       
dati$weathersit <- ifelse(dati$weathersit == 1, "Good",
                     ifelse(dati$weathersit == 2, "Moderate",
                       ifelse(dati$weathersit == 3, "Bad", dati$weathersit)))
                          
dati$temp <- (dati$temp * 47) - 8
dati$atemp <- (dati$atemp * 66) - 16
dati$windspeed <- dati$windspeed * 67

write.csv(dati, "day_2012.csv", row.names = FALSE)


##Analisi delle variabili
dati<-read.csv("day_2012.csv", header = TRUE, sep = ",")
attach(dati)

#Per poter comprendere al meglio le analisi che verranno fatte in questa tesina reputo utile introdurre le variabili che tratteremo attraverso alcune loro rappresentazioni grafiche.(nota a piè di pagina: "La panoramica delle variabili riguarderà solo le osservazioni relative al 2012 poiché la quasi totalità delle analisi verterà su queste osservazioni.") 
#Partendo dalle variabili qualitative possiamo farci un'idea sulla loro distribuzione attraverso i loro grafici a barre.

library(lattice)
pdf("Barplot weathersit.pdf")
tabella<- table(weathersit)
tabella_ord<- tabella[order(tabella, decreasing=T)]
barchart(tabella_ord, xlab = "Weather condition", ylab = "Frequency", main = "Barplot weathersit", horizontal=F)
dev.off()

pdf("Barplot Workingday.pdf")
tabella<- table(workingday)
tabella_ord<- tabella[order(tabella, decreasing=T)]
barchart(tabella_ord, xlab = "Workingday", ylab = "Frequency", main = "Barplot workingday", horizontal=F)
dev.off()

#Queste due variabili qualitative non sono le uniche presenti nel nostro dataset ma sono le uniche per le quali ha senso visualizzare la loro distribuzione.

#Adesso passiamo alle variabili quantitative. Qui reputo utile visualizzare i diagrammi a scatole e baffi di tutte e 7 le variabili.

variabili <- c(8, 9, 10, 11, 12, 13, 14)

pdf("Diagrammi a scatole e baffi.pdf")
layout_matrix <- rbind(1:4, c(5, 6, 7, 0))
layout(layout_matrix)

par(mar = c(3, 3, 3, 1))

for(i in seq_along(variabili)) {
  boxplot(dati[, variabili[i]],
          main = names(dati)[variabili[i]],
          col = "grey")
}
dev.off()

#Adesso vediamo due matrici di diagramma di dispersione per vedere la relazione che vi è tra le variabili evidenziando anche il coefficiente di correlazione per ciascua coppia.

panel.cor.fixed <- function(x, y, digits = 2, prefix = "", cex.cor = 1) {
  usr_old <- par("usr")
  on.exit(par(usr = usr_old))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y, use = "complete.obs")
  
  txt <- paste0(prefix, format(round(r, digits), nsmall = digits))
  
  text(0.5, 0.5, txt, cex = cex.cor)
}

upper.panel.simple <- function(x, y, ...) {
  points(x, y, ...)
}

pdf("Matrice di diagrammi di dispersione 1.pdf")
pairs(dati[, c(8, 9, 12, 13, 14)],
      lower.panel = panel.cor.fixed,
      upper.panel = upper.panel.simple,
      main = "Matrice di diagrammi di dispersione")
dev.off()

#Dalla prima matrice osserviamo un risultato ovvio, ovvero, una relazione lineare quasi perfetta tra la temperatura e la temperatura percepita. Inoltre reputo molto interessante porre l'attenzione sulla relazione che vi è tra la temperatura e i clienti occasionali e tra la temperatura e i clienti registrati (abbonati). In entrambi i casi la nuvola dei punti sembra seguire un andamento lineare positivo ma per quanto riguarda i clienti occasionali la nuvola si distribuisce lungo una retta con inclinazione maggiore rispetto ai clienti registrati, questo significa che con l'aumentare della temperatura vediamo una maggiore elasticità tra i clienti registratti rispetto a quelli occasionali, ovvero all'aumentare della temperatura vedremo un aumento maggiore nel numero dei clienti registrati rispetto quelli casuali. Per quanto riguarda la relazione tra i clienti totali e i clienti registrati invece notiamo che vi è una certa linearità, questo dimostra come la maggior parte dei clienti siano registrati e quelli occasionali abbiano un peso minimo sulla clientela complessiva, possiamo però notare come questo peso aumenti all'aumentare dei clienti totali, infatti la nuovola dei punti ad un certo punto sembra biforcarsi.


panel.cor.fixed <- function(x, y, digits = 2, prefix = "", cex.cor = 1) {
  usr_old <- par("usr")
  on.exit(par(usr = usr_old))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y, use = "complete.obs")
  
  txt <- paste0(prefix, format(round(r, digits), nsmall = digits))
  
  text(0.5, 0.5, txt, cex = cex.cor)
}

upper.panel.simple <- function(x, y, ...) {
  points(x, y, ...)
}
pdf("Matrice di diagrammi di dispersione 2.pdf")
pairs(dati[, c(10,11,12,13,14)], 
      lower.panel = panel.cor.fixed,
      upper.panel = upper.panel.simple,
	  main = "Matrice di diagrammi di dispersione")
dev.off()

#Dalla seconda matrice possiamo notare come l'umidità non sembri avere un impatto significativo sul numero di clienti giornalieri. E' interessante osservare invece come ad alti valori di vento corrispondono solo bassi valori di clienti casuali, cosa che non si verifica con i clienti registrati. Questo vuol dire che quando fa molto vento i clienti occasionali preferiscono trovare mezzi di trasoporto alternativi mentre gli abbonati comunque decideranno di noleggire la bici. Tutto questo ha senso perché dopo aver pagato un abbonamento è normale che si sia più propensi ad usuffruire del servizio.



###Inferenza con una variabile 
##Inferenza per un parametro di tendenza centrale
#In questa sezione procederemo a fare inferenza relativamente alla media e alla mediana della variabile temp utilizzando vari tipi di test. 

#Test t di student
#Iniziamo con il test t di student, il quale prevede l'assunzione di normalità per la variabile e poniamo come ipotesi nulla la seguente, H0: μ = 16.
t.test(temp, alternative = "two.sided", mu = 16)
#Il P-value assume il valore 0.4767, dunque ci troviamo all'interno dell'area di accettazione dell'Ipotesi Nulla. A conferma di ciò possiamo vedere che la nostra ipotesi (μ = 16) si trova all'interno dell'intervallo di confidenza fissato il livello di significatività a 5%.

#Test dei segni
#In questo caso facciamo inferenza sulla mediana così da non restringere troppo il modello statistico e non facciamo nessuna assunzione sulla variabile (distribution-free). Poniamo come Ipotesi Nulla la seguente H0: λ = 16
binom.test(length(temp[temp > 16]), length(temp), p = 1/2, alternative = "two.sided")
#Il P-value assume il valore 0.9583 dunque anche in questo caso accettiamo l'Ipotesi Nulla.
#Visualizziamo l'intervallo di confidenza.
median(temp)
sd<-sort(temp)
sd[qbinom(0.025, length(temp), 1/2)]
sd[qbinom(0.975, length(temp), 1/2)]
#Possiamo notare come sia il P-value che l'ampiezza dell'intervallo siano maggiori rispeto al t test, questo perché le assunzioni fatte nel test dei segni sono molto più morbide ed il processo inferenziale risulta più incerto.


#Test di Wilcoxon
#In questo caso l'unica assunzione che faremo sarà quella di simmetria della variabile rispetto alla mediana ed anche qui poniamo come Ipotesi Nulla H0: λ = 16. 
wilcox.test(temp, alternative = "two.sided", mu = 16, conf.int = TRUE)
#Anche qui il p-value assume un valore abbastanza alto e dunque accettiamo l'Ipotesi che la mediana sia uguale a 16.
#In questo caso possiamo notare come sia il P-value che l'ampiezza dell'intervallo di confidenza siano molto simili a quelli ottenuti con il t test, questo dimostra che l'assunzione di simmetria aumenta l'efficienza della procedura distribution-free.


#Test di permutazione dei segni
#Con questo test assumiamo non solo che la variabile sia simmetrica rispetto la mediana ma anche che il campione sia scambiabile. Anche qui la nostra Ipotesi Nulla è H0: λ = 16. 
install.packages("exactRankTests")
library(exactRankTests)
perm.test(temp, paired = FALSE, alternative = "two.sided", mu = 16)
#Possiamo osservare come il P-value sia quasi identico a quello ottenuto con il t test (dunque accettiamo l'Ipotesi Nulla) nonostante in questo approccio non abbiamo fatto neanche l'assunzione di indipendenza delle osservazioni.


#Test bootstrap
#Questo test consiste nel prendere una gran numero di campioni (nel nostro caso 10000) e per ciascuno di essi se ne calcola la media così da poter creare un istogramma con tutte le medie ottenute.
Boot.mean <- numeric(10000)
pdf("Istogramma bootstrap.pdf")
for (i in 1:10000) Boot.mean[i] <- mean(sample(temp, replace = T))
hist(Boot.mean, xlab = "Bootstrap sample mean", ylab = "Density", main = "Histogram")
dev.off()
#Possiamo vedere dall'istogramma che la distribuzione non è centrata in 16 anche se esso non è un valore troppo lontano dal centro della distribuzione. 

#Adesso eseguiamo il test.
library(boot)
m <- function(x, w) sum(temp * w)
boot(temp, m, R = 9999, stype = "w")

boot.ci(boot(temp, m, R = 9999, stype = "w"), conf = 0.95, type = c("norm", "basic", "perc", "bca"))
#Come possiamo vedere tutti e 4 gli intervalli di confidenza che otteniamo (ottenuti con 4 metodi differenti) hanno un'ampiezza minore rispetto all'intervallo ottenuto con il t test e tutti e 4 contengono il valore 16, ragion per cui anche con il metodo bootstrap accettiamo l'Ipotesi Nulla.



#Campioni appaiati
#Adesso considereremo il dataset comprensivo dei dati relativi sia al 2011 che al 2012 e analizzeremo la variabile temp per entrambi gli anni così da vedere attraverso l'analisi per campioni appaiati se ci sono state differenze. Il 2012 è stato un anno bisestile dunque procedo a togliere il 29 febbraio così da avere due dataset di uguale lunghezza.
dati1<-read.csv("day.csv", header = TRUE, sep = ",")
attach(dati1)
dati1$instant <- NULL
dati1$season <- ifelse(dati1$season == 1, "winter",
                  ifelse(dati1$season == 2, "spring",
                    ifelse(dati1$season == 3, "summer",
                      ifelse(dati1$season == 4, "fall", dati1$season))))
                      
dati1 <- dati1[dati1$yr == 0, ]

dati1$yr <- NULL                     
                      
dati1$mnth <- ifelse(dati1$mnth == 1, "January",
              ifelse(dati1$mnth == 2, "February",
              ifelse(dati1$mnth == 3, "March",
              ifelse(dati1$mnth == 4, "April",
              ifelse(dati1$mnth == 5, "May",
              ifelse(dati1$mnth == 6, "June",
              ifelse(dati1$mnth == 7, "July",
              ifelse(dati1$mnth == 8, "August",
              ifelse(dati1$mnth == 9, "September",
              ifelse(dati1$mnth == 10, "October",
              ifelse(dati1$mnth == 11, "November",
              ifelse(dati1$mnth == 12, "December", dati1$mnth))))))))))))


dati1$holiday <- ifelse(dati1$holiday == 0, "No",
                       ifelse(dati1$holiday == 1, "Yes", dati1$holiday))

dati1$weekday <- ifelse(dati1$weekday == 0, "Sunday",
                  ifelse(dati1$weekday == 1, "Monday",
                  ifelse(dati1$weekday == 2, "Tuesday",
                  ifelse(dati1$weekday == 3, "Wednesday",
                  ifelse(dati1$weekday == 4, "Thursday",
                  ifelse(dati1$weekday == 5, "Friday",
                  ifelse(dati1$weekday == 6, "Saturday", dati1$weekday)))))))

dati1$workingday <-  ifelse(dati1$workingday == 0, "No",
                       ifelse(dati1$workingday == 1, "Yes", dati1$workingday))
                       
                       
dati1$weathersit <- ifelse(dati1$weathersit == 1, "Good",
                     ifelse(dati1$weathersit == 2, "Moderate",
                       ifelse(dati1$weathersit == 3, "Bad", dati1$weathersit)))
                          
dati1$temp <- (dati1$temp * 47) - 8
dati1$atemp <- (dati1$atemp * 66) - 16
dati1$windspeed <- dati1$windspeed * 67


#Tolgo il 29 febbraio dall'anno bisestile (2012)
dati <- dati[dati$dteday != "2012-02-29", ]


#Analisi per campioni appaiati
#Ci creiamo la variabile differenza che non è altro che la differenza tra la variabile temp nel 2012 e la variabile temp nel 2011.
Difference <- dati$temp - dati1$temp
#A questo punto effettuiamo l'analisi visualizzando per prima cosa la stima di nucleo della differenza tra le due variabili. Il parametro h è stato ottenuto attraverso il metodo della cross-validation (h=1.141399).
library(sm)
pdf("Stima di nucleo difference.pdf")
sm.density(Difference, hcv(Difference, hstart = 0.01, hend = 100), yht = 0.1, xlim = c(-20, 20), xlab = "Stroke index difference")
title(main = "Kernel density estimation ('CV' h = 1.141399)")
dev.off()
#Valore h con cross-validation
h_value <- hcv(Difference, hstart = 0.01, hend = 100)

#Da questa visualizzazione possiamo notare come la distribuzione della variabile differenza sembri essere centrata in zero.


#Come seconda cosa osserviamo la distribuzione delle osservazioni attraverso il confronto tra i quantili teorici e quelli campionari, così da vedere di quanto la distribuzione della differenza si discosta dalla distribuzione di una normale.
pdf("Q-Q difference.pdf")
qqnorm(Difference)
qqline(Difference)
dev.off()
#Come ultima cosa effettuamo un t test ponendo la seguente Ipotesi Nulla, H0: μ = 0
t.test(Difference, alternative = "two.sided", mu = 0)

#Il P-value ci dice che bisogna rifiutare l'ipotesi che la media della variabile sia zero.


#Test di Kolmogorov (atemp solo per l'inverno)
#Considerando soltanto il periodo invernale (season = winter) potremmo ipotizzare che la variabile atemp (temperatura percepita) abbia una distribuzione approssimabile con una distribuzione Normale. Per verificare questa ipotesi utilizziamo il test di Kolmogorov.

dati2 <- subset(dati, season == "winter")

ks.test(dati2$atemp, "pnorm", mean(dati2$atemp), var(dati2$atemp)^(1/2))

#Dal test risulta un P-value pari a 0.3879, dunque accettiamo l'ipotesi che la distribuzione della variabile atemp sia approssimabile con una Normale. Adesso procediamo a dare una rappresentazione grafica della funzione di ripartizione Empirica (della variabile atemp) e Teorica (della Normale).
pdf("Funzioni di ripartizione empiriche e teoriche.pdf")
plot(ecdf(dati2$atemp), do.points = F, verticals = T, xlab = "Transformed tensile strength", ylab = "Probability", main = "Distribution function")
rug(dati2$atemp)
plot(function(x) pnorm(x), -10, 25, lty = 3, ylab = "Probability", add = T)
legend(0.6, 0.2, c("Empirical", "Theorical"), lty = c(1, 3))
dev.off()

#Test Chi-quadrato
#Analizzando la variabile workingday ci si aspetta che circa 1/3 dei giorni siano festivi mentre i restanti 2/3 lavorativi. Per testare questa ipotesi utilizziamo il test Chi-quadrato così da poter confrontare i valori osservati della variabile con quelli teorici. Come prima cosa relizziamo una rappresentazione grafica che ci permette di confrontare le probabilità dei valori osservati e quelli teorici.

table(dati$workingday)
p_th <- 2/3  #ipotizziamo che la probabilità teorica per "Yes" sia 2/3 (ho usato le frequenze 249/365 per yes e 116/365 per No)
Theory.Probs <- c(1 - p_th, p_th)  #rispettivamente per "No" e "Yes" -> queste sono le nostre ipotesi

#Calcoliamo i conteggi osservati e otteniamo le probabilità osservate
Counts <- table(dati$workingday)
Obs.Probs <- Counts / sum(Counts)

#Costruiamo la lista di dati: le etichette vengono replicate in modo da avere 2 gruppi
h <- list(Workingday = c(levels(dati$workingday), levels(dati$workingday)),
          Type       = c(rep("Teoriche", 2), rep("Osservate", 2)),
          Probs      = c(Theory.Probs, as.numeric(Obs.Probs)))
  
#Creiamo la tabella di contingenza        
dati$workingday <- factor(dati$workingday)

Table <- xtabs(Probs ~ ., data = h)
print(Table) #Utile perchè vediamo in cifre il confronto tra valori osservati e teorici


#Tracciamo il grafico a barre:
pdf("Distribuzione delle Probabilità: Teoriche vs Osservate per workingday.pdf")
barplot(t(Table),
        beside = TRUE,
        names.arg = levels(dati$workingday),
        xlab = "Working Day",
        ylab = "Probability",
        main = "Distribuzione delle Probabilità: Teoriche vs Osservate per workingday")

legend("topleft",
       legend = c("Teoriche", "Osservate"),
       fill = c("black", "grey"),
       inset = c(0.05, 0.05),
       bty = "n")
dev.off()       
#Possiamo notare una certa somiglianza tra i valori osservati e quelli teorici. Per vedere se le ipotesi fatte sulle probabilità si possono accettare effettuiamo il test Chi-quadrato.       
       
chisq.test(Counts, p = Theory.Probs)
       
#Come c'era da immaginarsi il p-value assume un valore piuttosto alto (p-value = 0.5292), dunque accettiamo l'ipotesi di base.



###Inferenza a due variabili
#Preparo il dataset
dataset<-read.csv("day.csv", header = TRUE, sep = ",")
attach(dataset)

#Modifichiamo le variabili e i valori delle variabili per rendere il dataset più comprensibile
dataset$instant <- NULL
dataset$season <- ifelse(dataset$season == 1, "winter",
                  ifelse(dataset$season == 2, "spring",
                    ifelse(dataset$season == 3, "summer",
                      ifelse(dataset$season == 4, "fall", dataset$season))))                     

dataset$mnth <- ifelse(dataset$mnth == 1, "January",
              ifelse(dataset$mnth == 2, "February",
              ifelse(dataset$mnth == 3, "March",
              ifelse(dataset$mnth == 4, "April",
              ifelse(dataset$mnth == 5, "May",
              ifelse(dataset$mnth == 6, "June",
              ifelse(dataset$mnth == 7, "July",
              ifelse(dataset$mnth == 8, "August",
              ifelse(dataset$mnth == 9, "September",
              ifelse(dataset$mnth == 10, "October",
              ifelse(dataset$mnth == 11, "November",
              ifelse(dataset$mnth == 12, "December", dataset$mnth))))))))))))


dataset$holiday <- ifelse(dataset$holiday == 0, "No",
                       ifelse(dataset$holiday == 1, "Yes", dataset$holiday))

dataset$weekday <- ifelse(dataset$weekday == 0, "Sunday",
                  ifelse(dataset$weekday == 1, "Monday",
                  ifelse(dataset$weekday == 2, "Tuesday",
                  ifelse(dataset$weekday == 3, "Wednesday",
                  ifelse(dataset$weekday == 4, "Thursday",
                  ifelse(dataset$weekday == 5, "Friday",
                  ifelse(dataset$weekday == 6, "Saturday", dataset$weekday)))))))

dataset$workingday <-  ifelse(dataset$workingday == 0, "No",
                       ifelse(dataset$workingday == 1, "Yes", dataset$workingday))
                       
                       
dataset$weathersit <- ifelse(dataset$weathersit == 1, "Good",
                     ifelse(dataset$weathersit == 2, "Moderate",
                       ifelse(dataset$weathersit == 3, "Bad", dataset$weathersit)))
                          
dataset$temp <- (dataset$temp * 47) - 8
dataset$atemp <- (dataset$atemp * 66) - 16
dataset$windspeed <- dataset$windspeed * 67


#Adesso procederemo ad effettuare un'analisi con due campioni relativi alla variabile cnt. Il dataset che useremo sarà quello che presenta i dati relativi sia al 2011 che al 2012, dunque i due campioni sono rispettivamente i valori di cnt per ciascun giorno del 2011 e del 2012. Sappiamo bene che i due campioni siano non del tutto indipendenti (il numero di clienti presenta stagionalità e potrebbe esserci anche autocorrelazione) ma per esercizio li considereremo tali. Per cominciare vediamo una loro rappresentazione grafica attraverso un diagramma a scatole e baffi.
attach(dataset)

cnt0 <- cnt[yr == "2011"]
cnt1 <- cnt[yr == "2012"]

pdf("Diagramma a scatole e baffi condizionato.pdf")
boxplot(cnt ~ yr, boxwex = 0.3, ylab = "Numero clienti (sia abbonati che non)", xlab = "Anno", main = "Diagramma a scatole e baffi condizionato", col=c("orange", "light green")) 
rug(cnt0, side = 2, ticksize = 0.02, col = "orange")
rug(cnt1, side = 4, ticksize = 0.02, col = "light green")
dev.off() 

#Dai due diagrammi possiamo vedere come la distribuzione del secondo campione (2012) sia molto più spostata verso destra evidenziando un aumento del numero di clienti. **Grazie alla funzione rug possiamo notare come anche nel 2012 ci siano stati giorni con pochi clienti ma il numero di questi giorni è di molto inferiore rispetto al 2011 (nel 2012 si vede una minore densità di tacche per valori bassi della variabile).** Adesso per vedere meglio le due distribuzioni useremo metodi di stima di nucleo con parametro di smorzamento.

#Creo i vettori cnt.1 e cnt.2 che sono le determinazioni assunte dalla variabile cnt rispettivamente nel 2011 e nel 2012. Li useremo spesso.
cnt.1 <- split(cnt, yr)[[1]]
cnt.2 <- split(cnt, yr)[[2]]

#I valori del parametro di smorzamtno ricavati attraverso il metodo cross-validation sono i seguenti.

library(sm)
hcv(cnt.1, hstart = 0.1, hend = 500)
hcv(cnt.2, hstart = 0.1, hend = 500)

#Adesso visualizziamo la distribuzione della variabile sui due campioni.
pdf("Smorzamento cnt.1.pdf")
sm.density(cnt.1, hcv(cnt.1, hstart = 0.1, hend = 500), yht = 0.0004, xlim = c(0, 6100), xlab = "Clienti nel 2011")
title(main = "Kernel density estimation ('CV' h = 252.063)")
dev.off() 

pdf("Smorzamento cnt.2.pdf")
sm.density(cnt.2, hcv(cnt.2, hstart = 0.1, hend = 500), yht = 0.0004, xlim = c(0, 9000), xlab = "Clienti nel 2012")
title(main = "Kernel density estimation ('CV' h = 216.4861)")
dev.off() 

#Adesso abbiamo un'idea più chiara della differenza che vi è tra le distribuzioni della variabile cnt nei due campioni.


#Adesso prima di fare un test t di student a due campioni procediamo col verificare l'ipotesi di omoschedasticità dei due campioni attraverso un test F di Fisher.

var.test(cnt.1, cnt.2, paired = F, alternative = "two.sided")

#Il P-value ci porta a rifiutare l'ipotesi di omoschedasticità, del resto il rapporto fra le varianze dei due campioni è pari a circa 0.59, un valore parecchio lontano da 1. Nonostante non sia possibili assumere l'omoschedasticità proseguiremo comunque implementando il test t di student (specificando var.equal = F all'interno del comando t.test). Qui l'ipotesi nulla è l'uguaglianza tra le medie (H0: μ1 = μ2).

t.test(cnt.1, cnt.2, var.equal = F, alternative = "two.sided")

#Il P-value assume un valore molto basso dunque non possiamo accettare l'ipotesi di omogeneità delle medie dei due campioni.


#Proseguiamo con altri due test, il test di Mann-Withney e il test di Permutazione. In entrambi i casi la nostra Ipotesi Nulla riguarda la mediana della distribuzione dei due campioni (H0: λ1 = λ2).

#test di Mann-Withney
wilcox.test(dataset$cnt ~ dataset$yr, alternative = "two.sided")

#test di Permutazione
install.packages("exactRankTests")
library(exactRankTests)

perm.test(dataset$cnt ~ dataset$yr, paired = F, alternative = "two.sided")

#In entrambi i casi otteniamo un P-value prossimo allo zero, dunque rifiutiamo anche l'Ipotesi di omogeneità delle mediane.

#Anche qui, come abbiamo visto nell'inferenza con una variabile, un altro test possibile è il test bootstrap.

Boot.meandif <- numeric(10000)
Boot.sample <- numeric(length(dataset$cnt))
for (i in 1:10000) {Boot.sample <- sample(dataset$cnt, replace = T);
Boot.cnt1 <- Boot.sample[c(1:length(cnt.1))];
Boot.cnt2 <- Boot.sample[c((length(cnt.1) + 1):length(dataset$cnt))]; Boot.meandif[i] <- mean(Boot.cnt1) - mean(Boot.cnt2)}
2 * length(Boot.meandif[Boot.meandif < mean(cnt.1) - mean(cnt.2)]) / 10000

#La significatività del test ci porta ancora una volta a  rifiutare l'Ipotesi di omogeneità delle medie


#Test di Kolmogorov-Smirnov
#Questo test non riguarda parametri di tendenza centrali come quelli appena visti, ma riguarda le funzioni di ripartizione dei due campioni. Prima di procedere col test diamo una rappresentazione grafica delle due funzioni.

pdf("Funzioni di ripartizione.pdf")
plot(ecdf(cnt.1), do.points = F, verticals = T, 
     xlim = c(0, 9000), lty = 1, col = "orange", 
     xlab = "Numero totale di clienti giornalieri", 
     ylab = "Probabilità", 
     main = "Funzioni di ripartizione empiriche")

plot(ecdf(cnt.2), do.points = F, verticals = T, 
     lty = 1, col = "light green", add = TRUE)

legend(1.5, 0.9, c("2011", "2012"), 
       col = c("orange", "light green"), lty = 1)
dev.off() 

#Già dal grafico possiamo vedere come le due funzioni di ripartizione empiriche siano molto differenti tra loro, procediamo effettuando il test di Kolmogorov-Smirnov.

ks.test(cnt.1, cnt.2)

#Possiamo vedere come il p-value ci conferma quanto detto prima, le due funzioni di ripartizione empiriche sono molto diverse tra loro dunque non possiamo accettare l'ipotesi che siano uguali. 


##Correlazione
#Variabili Quantitative

#Prendiamo in considerazione una Variabile Casuale Bivariata (X,Y), ovvero una variabile casuale che assume contemporaneamente le determinazioni delle variabili X=cnt e Y=hum. Come prima cosa rappresentiamo il diagramma di dispersione con le curve di livello.
dati<-read.csv("day_2012.csv", header = TRUE, sep = ",")
dati$cnt <- as.numeric(as.character(dati$cnt))

library(sm)
pdf("Curve di livello.pdf")
plot(dati$hum, dati$cnt, xlab = "Umidità (in %)", ylab = "Numero totale di clienti giornalieri")
sm.density(dati[, c(10, 14)], hcv(dati[, c(10, 14)]), display = "slice", props = c(75, 50, 25), add = T)
title(main = "Kernel density estimation ('CV' h1 = 0.0317, h2 = 423.489)")
dev.off() 

mean(dati$hum)
mean(dati$cnt)

#Da questo grafico possiamo dedurre che la nostra Variabile Casuale Bivariata non sia distribuita Normalmente (per esserlo le curve di livello sarebbero dovute essere centrate sui valori medi delle due variabili), questo implica che nel caso calcolassimo il coefficiente di correlazione e risultasse pari a zero questo non basterebbe per dire che le componenti marginali X ed Y siano Indipendenti. Facciamo il test per l'Indipendenza di Pearson (approccio classico) con H0: ρ = 0.

cor.test(dati$hum, dati$cnt, method = "pearson")

#Possiamo accettare l'Ipotesi Nulla consci del fatto che questo non basta per definire l'indipendenza tra le componenti marginali X ed Y.


#Adesso facciamo altri due test per l'Indipendenza, ovvero, il test di Spearman e il test di Kendall. Questi due test sono distribution-free e vengono utilizzati quando, come nel nostro caso, abbiamo una Variabile Casuale Bivariata non distribuita normalmente.

cor.test(dati$hum, dati$cnt, method = "spearman")

cor.test(dati$hum, dati$cnt, method = "kendall")

#In entrambi i casi otteniamo un p-value molto alto, dunque anche questa volta accettiamo l'Ipotesi di indipendenza (H0: ρ = 0).



##Variabili Qualitative 
#Prendiamo in esame le variabili qualitative workingday e weathersit. Possiamo chiederci se tra di esse ci sia o meno un rapporto di dipendenza (chiaramente è ragionevole pensare che le due variabili siano del tutto indipendenti l'uno dall'altra senza necessariamente fare un test ma lo faremo per esercizio). Per verificare l'esistenza di una dipendenza utilizzeremo il test Chi-quadrato ma prima visualizziamo la loro tabella a doppia entrata e il grafico a barre condizionato. 

#Ho ordianto la variabile in maniera tale che nel grafico mi spunti ordinata. (Bad Moderate e Good)
dati$weathersit <- factor(dati$weathersit, levels = c("Good", "Moderate", "Bad"))

table(dati$weathersit, dati$workingday)
#Dalla tabella a doppia entrata possiamo notare come alcune frequenze siano molto basse. Adesso facciamo la tabella delle frequenze attese per vedere se ci sono celle con frequenze <1.

chisq.test(table(dati$weathersit, dati$workingday))$expected
#Non ci sono frequenze attese <1 ed n>30 dunque l'approssimazione della distribuzione chi-quadrato per il calcolo del p-value è abbastanza affidabile.

library(lattice)
pdf("Grafico a barre condizionato.pdf")
barchart(table(dati$weathersit, dati$workingday), ylab = "Weather conditions", auto.key = list(title = "workingday", cex = 0.8))
dev.off()
#Adesso facciamo il test Chi-quadrato ponendo come Ipotesi l'indipendenza fra le due variabili, ovvero H0: πjl = πj+ * π+l per ogni (j,l).

chisq.test(table(dati$weathersit, dati$workingday))

#Possiamo vedere che il P-value assume valore 0.5137 dunque accettiamo l'Ipotesi Nulla, c'è modo di credere che le due variabili siano indipendenti. Per completezza effettuiamo anche il test esatto di Fisher.

fisher.test(table(dati$weathersit, dati$workingday))

#Anche il test esatto di Fisher ci conferma che le due variabili siano Indipendenti.


###Inferenza a più variabili
#Analisi della varianza
#Anche qui analizzeremo la variabile cnt ma questa volta realizzeremo una partizione differente del dataset. Per effettuare la partizione useremo infatti la variabile weekday e dunque otterremo 7 campioni indipendenti la cui analisi avverrà per mezzo dell'analisi della varianza. Per cominciare rappresentiamo i diagrammi a scatole e baffi della variabile cnt relativi ai 7 giorni della settimana.

#Ordino la variabile weekday
dati$weekday <- factor(dati$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

cnt.1 <- split(dati$cnt, dati$weekday)[[1]]
cnt.2 <- split(dati$cnt, dati$weekday)[[2]]
cnt.3 <- split(dati$cnt, dati$weekday)[[3]]
cnt.4 <- split(dati$cnt, dati$weekday)[[4]]
cnt.5 <- split(dati$cnt, dati$weekday)[[5]]
cnt.6 <- split(dati$cnt, dati$weekday)[[6]]
cnt.7 <- split(dati$cnt, dati$weekday)[[7]]

pdf("Diagrammi a scatole e baffi 2.pdf")
boxplot(dati$cnt ~ dati$weekday, main = "Boxplot", ylab="Total number of customers", xlab="Days of the week")
dev.off()

#Leggendo i diagrammi da sinistra verso destra possiamo notare come il range interquartile vada prima riducendosi raggiungendo il suo minimo il giovedì per poi tornare ad aumentare raggiungendo il suo massimo domenica. La mediana invece prima aumenta, raggiungendo un suo massimo giovedì, e poi diminuisce raggiungendo il suo minimo domenica. Dunque, considerando che il numero dei differenti giorni della settimana sono più o meno uguali fra loro (il numero di lunedì è più o meno uguale al numero dei martedì e così via), è possibile affermare che il maggior numero di clienti si concentra nei giorni centrali della settimana.

#Adesso effettuiamo le stime di nucleo delle funzioni di densità della variable nei 7 campioni usando per ciascuno un parametro di smorzamento diverso.

library(sm)
pdf("Funzioni di densità giorni della settimana.pdf")
par(mfrow = c(3, 3))
sm.density(cnt.1, hnorm(cnt.1), yht = 0.00028, xlim = c(0, 11000), xlab = "Monday")
title(main = "h = 869.3957")
sm.density(cnt.2, hnorm(cnt.2), yht = 0.00028, xlim = c(0, 11000), xlab = "Tuesday")
title(main = "h = 779.7903")
sm.density(cnt.3, hnorm(cnt.3), yht = 0.00028, xlim = c(0, 11000), xlab = "Wednesday")
title(main = "h = 855.125")
sm.density(cnt.4, hnorm(cnt.4), yht = 0.00028, xlim = c(0, 11000), xlab = "Thursday")
title(main = "h = 737.985")
sm.density(cnt.5, hnorm(cnt.5), yht = 0.00028, xlim = c(0, 11000), xlab = "Friday")
title(main = "h = 765.0062")
sm.density(cnt.6, hnorm(cnt.6), yht = 0.00028, xlim = c(0, 11000), xlab = "Saturday")
title(main = "h = 992.9403")
sm.density(cnt.7, hnorm(cnt.7), yht = 0.00028, xlim = c(0, 11000), xlab = "Sunday")
title(main = "h = 920.5756")
par(mfrow = c(1, 1))
dev.off()

hnorm(cnt.1)
hnorm(cnt.2)
hnorm(cnt.3)
hnorm(cnt.4)
hnorm(cnt.5)
hnorm(cnt.6)
hnorm(cnt.7)

#Dalle funzioni di densità stimate abbiamo conferma di quanto detto nell'analisi dei diagrammi a scatole e baffi. Adesso facciamo l'anlisi della varianza ponendo come Ipotesi Nulla l'ugualglianza delle medie dei 7 diversi campioni (H0: μ1 = μ2 = ... = μ7).

summary(aov(dati$cnt ~ dati$weekday))

#Il p-value ottenuto ci porta a rifiutare l'ipotesi Nulla dunque possiamo affermare che il giorno della settimana incide sul numero di clienti. Per vedere da quali coppie di medie dipende il rifiuto utilizziamo il test di Tukey.

TukeyHSD(aov(dati$cnt ~ dati$weekday), "dati$weekday")

#Dal test non risultano intervalli che non contengono lo zero però è possibile vederne alcuni in cui si vede un forte sbilanciamento tra limite inferiore e limite superiore (Es. lwr negativo ma piccolo e upr positivo ma molto grande). Gli intervalloi in questione sono Thursday-Monday, Sunday-Wednesday, Sunday-Thursday, Sunday-Friday. Un altro test da implemetanre è quello di Kruskal-Wallis.

kruskal.test(dati$cnt ~ dati$weekday)

#Il risultato è molto simile a quello ottenuto con l'analisi della varianza e dunque ci porta a rifiutare l'Ipotesi Nulla.


### Regressione
## Regressione Lineare
# Vogliamo verificare se tra il numero totale di clienti (cnt) e la temperatura (temp) vi sia una relazione lineare. Chiaramente è logico pensare ci sia non solo una relazione lineare ma anche che essa sia positiva (all'aumentare della temperatura aumenta il numero di persone che con o senza abbonamento noleggiano la bici) ma vogliamo provarlo. Per prima cosa stimiamo i parametri del modello di regressione lineare.

lm(dati$cnt ~ dati$temp)

#Come previsto la retta di regressione stimata ha un'inclinazione positiva. Adesso rappresentiamo graficamente il diagramma di dispersione delle due variabili e lo corrediamo con la retta di regressione appena stimata.

pdf("Regressione lineare.pdf")
plot(dati$temp, dati$cnt, xlab = "Temperature (°C)", ylab = "Total number of customers", main = "Scatter plot")
abline(lm(dati$cnt ~ dati$temp))
dev.off()

#Una volta creato il modello di regressione procediamo con la sua analisi attraverso una serie di grafici che, guardando il modello da diverse prospettive, mettono in risalto alcuni suoi aspetti.

pdf("Diagramma di Anscombe.pdf")
par(mfrow = c(2, 2))
plot(lm(dati$cnt ~ dati$temp), which = c(1:4), add.smooth = F)
par(mfrow = c(1, 1))
dev.off()

#In alto a sinistra possimo vedere il diagramma di Anscombe che ci mostra come si discostano le singole unità osservate dai valori stimati dal modello. Possiamo vedere che i punti si distribuiscono non propriamente in modo casuale denotando una non buona adattabilità dei dati al modello. Inoltre abbiamo tre giorni del 2012 (precisamente il 113esmio, il 189esimo e il 303esimo giorno) in cui si sono avuti pochi clienti rispetto alle temperature medie di quelle giornate (infatti abbiamo alti errori negativi). In basso a sinistra si riportano le redici dei valori assoluti dei residui standardizzati. In alto a destra si riporta il diagramma quantile-quantile in cui vengono riportati i residui standardizzati ordinati rispetto ai quantili della distribuzione Normale Standardizzata dove possiamo vedere che i punti si distribuiscono per buona parte lungo la bisettrice rendendo credibile l'Ipotesi di Normalità. Per finire in basso a destra abbiamo Il diagramma con le distanze di Cook dove vediamo quali sono i giorni che impattano maggiormente sul nostro modello (Un modello lineare è fortemente influenzato dagli outliers peroprio per via della sua rigidità).

#Presentiamo nuovamente il diagramma di dispersione con la retta di regressione evidenziando questa volta anche le osservazioni critiche che sono state evidenziate nei 4 grafici precedenti.

pdf("Diagramma di dispersione con outliers.pdf")
plot(dati$temp, dati$cnt, xlab = "Temperature (°C)", ylab = "Total number of customers", main = "Scatter plot")
abline(lm(dati$cnt ~ dati$temp))
text(x = dati$temp[113] + 0.3, y = dati$cnt[113], labels = "113", adj=0)
text(x = dati$temp[189] - 1, y = dati$cnt[189] -220, labels = "189", adj=0)
text(x = dati$temp[303] + 0.3, y = dati$cnt[303], labels = "303", adj=0)
text(x = dati$temp[390] - 0.5, y = dati$cnt[390], labels = "390", adj=0)
text(x = dati$temp[190] - 2.1, y = dati$cnt[190] - 100, labels = "190", adj=0)
text(x = dati$temp[361] + 0.3, y = dati$cnt[361], labels = "361", adj=0)
dev.off()

#Vediamo adesso un modello di regressione più flessibile ovvero la regressione lineare locale. Questo tipo di regressione lo abbiamo già incontrato e come abbiamo detto in precedenza esso a differenza del modello di regressione lineare classico si adatta localmente alle osservazioni.

library(sm)
pdf("Regressione lineare locale.pdf")
plot(dati$temp, dati$cnt, xlab = "Temperature (C°)", ylab = "Total number of customers", main = "Scatter plot")
sm.regression(dati$temp, dati$cnt, method = "df", add = T)
dev.off()

#Con questo modello appare molto più chiara la relazione fra le due variabili. All'aumentare della temepratura il numero di clienti aumenta ma questo avviene fino al raggiungimento di una temperatura "critica" oltre la quale il numero di clieni diminuisce. Questo è anche logico se ci pensiamo temperature eccessivamente basse o eccessivamente alte disincentivano il noleggio di biciclette.


#Adesso torniamo alla regressione lineare e verifichiamo la validità del modello considerando la seguente Ipotesi Nulla, H0: β1 = 0 (Nessun legame lineare fra le variabili).

summary(lm(dati$cnt ~ dati$temp))

#Come possiamo osservare entrambi i coefficienti (Intercetta e Coefficiente Angolare) risultano significativi dunque rifiutiamo l'Ipotesi Nulla. La varianza spiegata dal modello è circa il 50% ma adesso facciamo nuovamente il test con il modello privo di intercetta.

summary(lm(dati$cnt ~ -1 + dati$temp))

#Possiamo notare come in questo caso la varianza spiegata dal modello salga quasi al 90%, dunque otteniamo un modello ancora migliore.


##Regressione con trasformate
#Osservando la disposizione dei punti nel diagramma di dispersione delle variabili casual (clienti occasionali) e cnt (clienti totali) può venire l'intuizione che un buon predittore della variabile cnt possa essere il logaritmo di casual.

attach(dati)

pdf("Diagramma di dispersione casual-cnt.pdf")
plot(casual, cnt, xlab="Casual", ylab="cnt", main="Scatter plot")
dev.off()

#Dunque reputo opportuno effettuare una trasformazione logaritmica della variabile casual e fare un'analisi del modello di regressione lineare ottenuto in seguito alla trasformazione. 

summary(lm(cnt ~ log(casual)))

#Dati i valori dei p-value possiamo confermare il legame logaritmico presente tra le due variabili. Adesso rappreseniamo il diagramma di dispersione con la relativa funzione di regressione che abbiamo stimato.

pdf("Diagramma di dispersione log(casual)-cnt.pdf")
plot(casual, cnt, xlab="Casual", ylab="cnt", main="Scatter plot")
lines(seq(1, 3500, 1), predict(lm(cnt ~ log(casual)), data.frame(casual = seq(1, 3500, 1))))
dev.off()

##Regressione di Poisson
#Adesso prendiamo in considerazione sempre le variabili Casual e cnt ma questa volta poniamo cnt come regressore e casual come variabile di risposta. Poichè la variabile di risposta assume solo valori interi positivi possiamo considerare tali valori come realizzazioni di Variabili Casuali di Poisson e possiamo realizzare una regressione di Poisson.

summary(glm(dati$casual ~ dati$cnt, poisson))

#Ad una prima occhiata potremmo pensare che questo si un buon modello, del resto entrambi i coefficienti stimati risultano essere significtivi, il problema sta nella devianza residua. La devianza residua dovrebb essere un valore prossimo al numero di gradi di libertà ma nel nostro caso è un valore molto maggiore; questo significa che è presente super dispersione e dunque il modello è si buono ma non è in grado di spiegare adeguatamente la variazione osservata nei dati. A questo punto usiamo un metodo di quasi-verosimiglianza.

summary(glm(dati$casual ~ dati$cnt, quasipoisson))

#Il parametro di dispersione così elevato ci conferma la presenza di super dispersione, infatti il modello stima che la varianza dei dati sia circa 298 volte quella stimata dal modello di Poisson. Rappresentiamo graficamente il diagramma di dispersione con la relativa funzione di regressione stimata.


attach(dati)
pdf("Regressione di Poisson.pdf")
plot(cnt, casual, xlab = "cnt", ylab = "casual", main = "Scatter plot")
lines(seq(0, 9000, 1), exp(predict(glm(casual ~ cnt, quasipoisson), data.frame(cnt = seq(0, 9000, 1)))))
dev.off()

## Regressione Logistica
# Adesso consideriamo la relazione che intercorre tra la variabile casual e workingday. La variabile workingday è la nostra variabile di risposta ed essendo una variabile dicotomica possiamo usare come modello lineare generalizzato una regressione logistica. Analizziamo dunque la dipendenza del numero di clienti occasionali dal tipo di giorno (lavorativo o festivo).

#Trasformiamo le determinazioni della variabile in modo tale che "yes" = 1 e "no" = 0.
dati$workingday <-  ifelse(dati$workingday == "No", 0,
                       ifelse(dati$workingday == "Yes", 1, dati$workingday))
                       
#Converto la variabile i variabile numerica                       
dati$workingday <- as.numeric(as.character(dati$workingday))

#modello logit
summary(glm(dati$workingday ~ dati$casual, binomial))
#L'analisi del modello di regressione logistica ci porta a dire che il modello si adegua bene ai nostri dati, procediamo col visualizzare il diagramma di dispersione con la relativa funzione di regressione logistica stimata.

attach(dati)
pdf("Modello Logit casual-workingday.pdf")
plot(casual, workingday, xlab = "Casual", ylab = "Workingday", main = "Scatter plot")
lines(seq(0, 3500, 1), predict(glm(workingday ~ casual, binomial), data.frame(casual = seq(0, 3500, 1)), type = "response"))
dev.off()

#Il modello stimato ci dice che all'aumentare del numero di clienti occasionali accumulati nel corso di una giornta diminuisce la probabilità che quello sia un giorno lavorativo. Questo significa che chi non ha un abbonamento predilige i giorni festivi per noleggiare una bici. Viene spontaneo chiedersi se vale lo stesso per chi possiede un abbonamento.

summary(glm(dati$workingday ~ dati$registered, binomial))
#Anche in questo caso il modello si adatta molto bene.

pdf("Modello Logit registered-workingday.pdf")
plot(registered, workingday, xlab = "Registered", ylab = "Workingday", main = "Scatter plot")
lines(seq(0, 7000, 1), predict(glm(workingday ~ registered, binomial), data.frame(registered = seq(0, 7000, 1)), type = "response"))
dev.off()

#Possiamo notare come per i possessori di abbonamento il modello ci dice tutto il contrario. All'aumentare del numero di clienti abbonati di una giornata aumenta la probabilità che quello sia un giorno lavorativo. Dunque i clienti che si abbonano lo fanno perché hanno necessità di spostarsi per esigenze lavorative mentre chi non sente il bisogno di abbonarsi non lo fa perché a lavoro ci va con altri mezzi ed usa la bici nei giorni festivi.



## Regressione multipla 
# Il numero di clienti giornalieri può dipendere da diversi fattori come la temperatura, la temperatura percepita, il vento, l'umidità e il numero di clienti occasionali, dunque adesso svilupperemo un modello di regressione lineare multiplo con tali regressori. Verifichiamo se il modello ottenuto usando questi regressori è solido.

summary(lm(dati$cnt ~ dati$temp + dati$atemp + dati$hum + dati$windspeed + dati$casual))

#Possiamo subito notare come le variabili temp e atemp siano entrambe poco significative poiché il loro p-value ci porta ad accettare l'ipotesi che il loro coefficiente sia nullo. Questo però potrebbe essere dovuto al fatto che sono due variabili molto simili tra loro (tempreatura e temperatura percepita) dunque danno praticamente le stesse informazioni. Per ridurre il modello all'essenziale e mantenere soltanto i regressori più significativi usiamo il criterio di Akaike.

summary(step(lm(dati$cnt ~ dati$temp + dati$atemp + dati$hum + dati$windspeed + dati$casual)))

#Qui abbiamo la conferma di quanto detto prima. Usando questo criterio eliminiamo il regressore relativo alla temperatura mantenendo tutti gli altri. E' molto importante notare come elimnando un regressore il coefficiente R-quadro diminuisca ma il coefficiente R-quadro aggiustato aumenti. Questo è dovuto al fatto che in un modello di regressione multiplo il coefficiente R-quadro è sensibile al numero di regressori, dunque, anche dei regressori "inutili" o "doppioni" come la nostra variabile temp ne aumentano il valore. Il coefficiente R-quadro aggiustato non è sensibile al numero di regressori ed è per questo che nel confrontare i due modelli condiserimo solo lui e non il semplice R-quadro. Adesso analizziamo il modello visualizzando i diagrammi di Anscombe.

pdf("Diagrammi regressione multipla.pdf")
par(mfrow = c(2, 2))
plot(lm(dati$cnt ~ dati$atemp + dati$hum + dati$windspeed + dati$casual), which = c(1:4), add.smooth = FALSE)
par(mfrow = c(1, 1))
dev.off()

#Notiamo la presenza di 3 outliers ovvero i giorni 189, 190 e 303 del 2012. Queste sono le osservazioni che si discostano maggiormente dal nostro modello e che impattano maggiormente sul nostro iperpiano di previsione. Procediamo ad eliminare queste osservazioni.

attach(dati)
summary(lm(dati$cnt ~ dati$atemp + dati$hum + dati$windspeed + dati$casual, subset = (1:length(cnt) != 189 & 190 & 303)))

#Eliminando queste osservazioni passiamo da un R-quadro aggiustato pari a 0.6457 ad uno pari a 0.6561, dunque guadagnamo qualcosa in termini di varianza spiegata dal modello.


#Adesso nel nostro modello introduciamo anche una relazione tra variabili, ovvero il prodotto tra la temperatura percepita e il numero di clienti occasionali.

summary(lm(dati$cnt ~ dati$windspeed + dati$hum + dati$atemp * dati$casual))

#Il coefficiente stimato per la variabile di interazione appare significativo e possiamo notare anche un aumento dell'R-quadro aggiustato rispetto all'ultimo modello stimato (dopo l'eliminazione degli outliers). Questo significa che introducendo questa relazione fra la variabile atemp e casual ottengo un modello migliore. In fine eliminiamo anche qui gli outliers ed otteniamo il seguente modello.

summary(lm(dati$cnt ~ dati$windspeed  + dati$hum + dati$atemp * dati$casual, subset = (1:length(cnt) != 189 & 190 & 303)))

#Il modello ottenuto spiega circa il 70% della varianza dei dati. Il modello ottenuto non è di certo uno dei migliori ma comunque abbiamo visto come con qualche modifica siamo riusciti ad ottenere un modello che presenta un R-quadro più alto.


## Modelli additivi generalizzati
# Supponiamo adesso che la nostra variabile di risposta (cnt) dipenda da una somma di trasformazioni non note dei regressori usati nel modello precedente. In questo caso usaremo un modelo additivo generalizzato. 

library(mgcv)
summary(gam(dati$cnt ~ s(dati$atemp) + s(dati$hum) + s(dati$windspeed) + s(dati$casual)))

# In questo modo otteniamo un modello con un R-quadro aggiustato di circa 0.78 ed un modello che spiega circa il 78% della variazione dei dati incomincia ad essere un buon modello. Di seguito vedremo i grafici delle quattro funzioni.
attach(dati)
pdf("Funzioni s.pdf")
par(mfrow = c(2, 2))
plot(gam(cnt ~ s(atemp) + s(hum) + s(windspeed) + s(casual)), pages = 1)
dev.off()

#Di seguito riportiamo gli effetti dei regressori sulla variabile dipendente:
#Variabile atemp: questa variabile per temperature basse ha un impatto negativo sulla variabile dipendente, per temperature miti diventapositivo ed in fine per temperature elevate torna rapidamente ad influire negativamente. Questo lo avevamo gà visto in precedenza, temperature troppo basse o troppo alte influenzano negativamente il numero di persone che noleggiano bici. 
#Variabile hum: Fino a circa il 65% di umidità la variabile ha un impatto positivo sulla variabile di risposta, ma superata questa soglia diventa negativo.
#Variabile Windspeed: All'aumentare dell'intensità del vento il suo andamento è lineare decrescente.
#Variabile Casual: Anche in questo caso abbiamo un andamento lineare ma questa volta crescente, superata la soglia dei 750 clienti occasionali l'impatto di questa variabile sul numero complessivo di clienti è positivo.



#Regressione Logistica Multipla
#In questo elaborato abbiamo già visto due modelli logistici, entrambi con la medesima variabile di risposta dicotomica workingday ed un solo regressore, rispettivamente casual e registered. Adesso considereremo sempre workingday come variabile di risposta ma introdurremo nel modello più regressori, creando così un modello di regressione logistica multiplo. I regressori che inseriremo nel modello sono la temperatura, l'umidità e il numero complessivo di clienti. Sappiamo bene che le prime due variabili sono del tutto ininfluenti nella determinazione della tipologia di giornata (lavorativa o feriale) ma le inseriamo per vedere come il software riconosce le variabili non significative e le elimina dal modello.

dati$workingday <-  ifelse(dati$workingday == "No", 0,
                       ifelse(dati$workingday == "Yes", 1, dati$workingday))

attach(dati)
workingday <- as.numeric(as.character(workingday))

summary(step(glm(workingday ~ temp + hum + cnt, binomial)))

#Come avevamo previsto nel modello resta solo la variabile cnt. Questa variabile come sappiamo è l'unione delle variabili Casual e Registered ed abbiamo visto nei due modelli logistici analizzati in precedenza che, mentre Casual ha un effetto negativo sulla variabile di risposta (all'aumentare dei clienti occasionali diminuisce la probabilità che quel giorno sia lavorativo) Registered ha un effetto positivo (all'aumentare del numero di abbonati che noleggiano una bici aumenta la probabilità che sia un giorno lavorativo). Dall'analisi del modello possiamo vedere che cnt ha un effetto positivo ma esso non è altro che il frutto degli effetti delle due variabili che la compongono. Inoltre dobbiamo dire che la riduzione della devianza risulta veramente minima (si passa da una null deviance di 457.16 ad una residual deviance di 452.07) e questo dimostra che il modello non si adatta molto bene ai dati. Vediamo una rappresentazione grafica.

workingday.h <- workingday[1:366]
cnt.h <- cnt[1:366]
pdf("Regressione Logistica Multipla.pdf")
plot(cnt.h, workingday.h, xlab = "cnt", ylab = "workingday", main = "Scatter plot")
lines(seq(0, 8714, 1), predict(glm(workingday.h ~ cnt.h, binomial), data.frame(cnt.h = seq(0, 8714, 1)), type = "response"))
dev.off()

#Il grafico ci conferma quanto detto prima, il modello non è solido.



##Metodi di smorzamento

#Adesso condurremo un'indagine esplorativa della funzione di densità della variabile windspeed. Tale funzione potrebbe essere stimata in maniera grossolana da un istogramma ma, poiché noi vogliamo ottenere risultati grafici più precisi, stimeremo tale funzione usando i metodi di smorzamento. Un metodo di smorzamento, a differenza dell'istogramma, crea una rappresentazione della funzione di densità concentrata sul nucleo più o meno liscia all'aumentare o al diminuire del parametro di smorzamento h (all'aumentare del parametro la funzione appare più liscia). Dunque scegliamo tre valori diversi di h così da vedere cosa succede alla funzione (h = 5, 1, 0.5).

library(sm)

pdf("Metodo di smorzamento h=5.pdf")
sm.density(dati$windspeed, 5 , yht = 0.075, xlim = c(0, 33), xlab = "Velocità media del vento (in km/h)", ylab="Funzione di densità di probabilità")
title(main = "h = 5")
dev.off()

pdf("Metodo di smorzamento h=1.pdf")
sm.density(dati$windspeed, 1 , yht = 0.13, xlim = c(0, 33), xlab = "Velocità media del vento (in km/h)", ylab="Funzione di densità di probabilità")
title(main = "h = 1")
dev.off()

pdf("Metodo di smorzamento h=0.5.pdf")
sm.density(dati$windspeed, 0.5 , yht = 0.13, xlim = c(0, 33), xlab = "Velocità media del vento (in km/h)", ylab="Funzione di densità di probabilità")
title(main = "h = 0.5")
dev.off()

#Dalle funzioni di densità ottenute possiamo osservare empiricamente come al diminuire del parametro di smorzamento h la funzione di densità della variabile diventi sempre più ruvida avvicinandosi così alla funzione di densità empirica. Come sappiamo più la funzione risulta liscia e maggiori saranno le informaioni che perderemo (aumenta la distorsione dello stimatore di nucleo), dunque è giusto usare dei valori del parametro di smorzamento che rapresentino un buon compromesso tra distorsione dello timatore e smorzamento della funzione. Per fare ciò calcoleremo il parametro h servendoci dei metodi cross-validation e plug-in.

hcv(dati$windspeed)
hsj(dati$windspeed)

#Adesso vediamo graficamente cosa comporta l'utilizzo di un metodo piuttosto che l'altro.

library(sm)
pdf("Smorzamento cv.pdf")
sm.density(dati$windspeed, hcv(dati$windspeed), yht = 0.13, xlim = c(0, 33), xlab = "Velocità media del vento (in km/h)")
title(main = "Kernel density estimation ('CV' h = 1.028668)")
dev.off()

pdf("Smorzamento plug-in.pdf")
sm.density(dati$windspeed, hsj(dati$windspeed), yht = 0.13, xlim = c(0, 33), xlab = "Velocità media del vento (in km/h)")
title(main = "Kernel density estimation ('Plug-in' h = 1.370939)")
dev.off()

#Con entrambi i metodi otteniamo un parametro h relativamente alto, il che ci porta a stimare due funzioni di densità abbastanza lisce.



##Stimatore di nucleo bivariato

#Possiamo utilizzare i metodi di smorzamento anche per stimare la funzione di densità congiunta di due variabili quantitative continue. In questo caso useremo solo il metodo cross-validation e stimeremo la funzione di densità congiunta delle variabili temp e windspeed, ottenendo il seguente grafico tridimensionale.

variabili <- names(dati)
data.frame(Posizione = seq_along(variabili), Variabile = variabili)

library(sm)
pdf("Funzione di densità congiunta.pdf")
sm.density(dati[,c(8,11)], hcv(dati[,c(8,11)]), xlim = c(-5, 35), ylim = c(0, 33), zlim = c(0, 0.006), xlab = "Temperatura media (in °C)", ylab = "Velocità media del vento (in km/h)", zlab="")
title(main = "Kernel density estimation ('CV' h1 = 1.938815, h2 = 1.227147)")
dev.off()

#Come possiamo osservare dal grafico abbiamo due picchi, uno in corrispondenza di vento moderato e temperature basse ed uno in corrispondenza di vento moderato e temperature elevate. Questo è dovuto alla stagionalità dei nostri dati, la città di Washington DC non è soggetta a forti venti dunque i due bicchi rappresentano rispettivamente la bella e la brutta stagione. Per una visione migliore possiamo sezionare a varie altezze la funzione di densità congiunta ottenendo un grafico bidimensionale raffigurante le curve di livello.

pdf("Curve di livello 2.pdf")
plot(dati$temp, dati$windspeed, xlim = c(-3, 33), ylim = c(0, 30), xlab = "Temperatura media (in °C)", ylab = "Velocità media del vento (in km/h)")
sm.density(dati[,c(8,11)], hcv(dati[, c(8,11)]), display = "slice", props = c(75, 50, 25), add = T)
title(main = "Curve di livello")
dev.off()

#Con le curve di livello confermiamo quanto detto prima ed aggiungiamo che nella bella stagione i venti tendono ad essere leggermente meno intensi. Adesso usiamo una rappresentazione grafica per toni di colori.

pdf("Toni di colori.pdf")
sm.density(dati[,c(8,11)], hcv(dati[,c(8,11)]), display = "image", xlim = c(-3, 33), ylim = c(0, 30), xlab = "Temperatura media (in °C)", ylab = "Velocità media del vento (in km/h)")
title(main = "Toni di colori")
dev.off()

#Questo grafico ha un impatto visivo maggiore e unito ai due grafici precedenti ci permette di avere una visione più di insieme di come è fatta la funzione di densità congiunta. 



## Massima Verosimiglianza

#Prendiamo in esame la variabile windspeed. Facciamo finta che i dati che possediamo siano le osservazioni di un campione, campione estratto sotto ipotesi che la variabile windspeed si disponga normalmente. Le stime di massima verosimiglianza sono le seguenti:

mean(dati$windspeed, na.rm = TRUE)
var(dati$windspeed , na.rm = TRUE)

#Adesso verifichiamo la validità del modello controllando i valori dei coefficienti campionari di asimmetria e curtosi (nel nostro caso per avere una distribuzione normale dovremmo ottenere entrambi i coefficienti pari a zero).

library(e1071)
skewness(dati$windspeed, na.rm = TRUE)
kurtosis(dati$windspeed, na.rm = TRUE)

#Abbiamo ottenuto entrambi i coefficienti positivi, dunque la variabile windspeed non si distribuisce normalmente ma la sua distribuzione presenta un'asimmetria positiva ed è leptocurtica (a code pesanti). Adesso vedremo anche una rappresentazione grafica di quanto detto attraverso un diagramma quantile-quantile. Con tale diagramma mettiamo a confronto le osservazioni standardizzate tramite la media e la varianza campionarie con i quantili della distribuzione normale standardizzata. 

pdf("Q-Q.pdf")
qqnorm(dati$windspeed)
qqline(dati$windspeed)
dev.off()

#Avessimo avuto una distribuzione normale i punti si saebbero dovuti distribuire tutti lungo la retta, in questo caso invece vediamo che i punti si discostano da essa. Nella parte superiore possiamo notare come i punti di discostano maggiormente, ciò evidenzia che la coda di destra risulta più pesante e questo conferma l'asimmetria positiva che avevamo anticipato con l'indice di asimmetria. Possiamo notare però come anche nella parte inferiore i punti si discostano, seppur in maniera meno accentuata rispetto la parte superiore, evidenziando code pesanti che sono caratterizzanti di una distribuzione leptocurtica.


















