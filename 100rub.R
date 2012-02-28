#loading libraries
library(rusquant)
library(lmtest)
library(FinTS)
library(fastICA)
library(vars)
library(fUnitRoots)
library(foreach)

setwd('~/rplayground/ifccomovement/')
source('garchoxfit.r')


#retrieving raw data
SP500 <- getSymbols("SANDP-500", from="2002-01-01", to="2012-02-22", auto.assign=F, src="Finam")
FTSE <- getSymbols("FUTSEE-100", from="2002-01-01", to="2012-02-22", auto.assign=F, src="Finam")
MICEX <- getSymbols("MICEX", from="2002-01-01", to="2012-02-22", auto.assign=F, src="Finam")
N225 <- getSymbols("N225JAP", from="2002-01-01", to="2012-02-22", auto.assign=F, src="Finam")
HSENG <- getSymbols("ZHC5", from="2002-01-01", to="2012-02-22", auto.assign=F, src="Finam")
DAX <- getSymbols("DAX", from="2002-01-01", to="2012-02-22", auto.assign=F, src="Finam")
CAC40 <- getSymbols("CAC40", from="2002-01-01", to="2012-02-22", auto.assign=F, src="Finam")
KOREA <- getSymbols("INDEX.KOSPI", from="2002-01-01", to="2012-02-22", auto.assign=F, src="Finam")
SING <- read.csv('table.csv')
SING <- xts(SING[,2:7], order.by = as.Date(SING$Date), unique = FALSE, tzone = "")
SHANGHAI <- read.csv('SH.csv', sep=",", dec=".")
SHANGHAI <- xts(SHANGHAI[,2:7], order.by = as.Date(SHANGHAI$Date, format="%Y-%m-%d"), unique = FALSE, tzone = "")
DUBA <- read.csv('DFM.csv', sep=";", dec=",")
DUBA <- xts(DUBA[,2], order.by = as.Date(DUBA$Date, format="%Y-%m-%d"), unique = FALSE, tzone = "")


#building up the scale for synchronisation
x <- xts(rnorm(4000), Sys.Date()-1:4000)

#removing weekdays
scale <- x[.indexwday(x) %in% 1:5]


#approximating lacunas, synchronising data
RU <- na.approx(aggregate(MICEX[,4], as.Date), xout = time(scale), rule = 2)
EN <- na.approx(aggregate(FTSE[,4], as.Date), xout = time(scale), rule = 2)
HK <- na.approx(aggregate(HSENG[,4], as.Date), xout = time(scale), rule = 2)
JP <- na.approx(aggregate(N225[,4], as.Date), xout = time(scale), rule = 2)
CH <- na.approx(aggregate(SHANGHAI[,6], as.Date), xout = time(scale), rule = 2)
SI <- na.approx(aggregate(SING[,6], as.Date), xout = time(scale), rule = 2)
US <- na.approx(aggregate(SP500[,4], as.Date), xout = time(scale), rule = 2)
DU <- na.approx(aggregate(DUBA[,1], as.Date), xout = time(scale), rule = 2)
GR <- na.approx(aggregate(DAX[,4], as.Date), xout = time(scale), rule = 2)
FR <- na.approx(aggregate(CAC40[,4], as.Date), xout = time(scale), rule = 2)
KO <- na.approx(aggregate(KOREA[,4], as.Date), xout = time(scale), rule = 2)


#merging data into dataset
dataset <- merge(CH, HK, SI, RU, EN, US, JP, DU, GR, FR, KO)
dataset <- dataset[234:2856]

#making diffs of logs
dtdiff <- diff(log(dataset))*100

#cleaning up the data
#WARNING! IF YOU RETRIEVED DATA ON THE DATE OTHER THAN 22/02/2012 YOU HAVE TO RECALCULATE ALL 
#DATA CONSTANTS BY ADDING DIFF IN WORKING DAYS BETWEEN 22/02/2012 AND THE DATE
#dtdiff <- dtdiff[234:2856]
#dtdiff$SI[2406] <- 0
#dtdiff$SI[2407] <- 0

#just checking that everything is OK - can be omitted
#retcris <- dtdiff[1543:1911]
#retpre <- dtdiff[1:1542]
#retpost <- dtdiff[1911:2622]
#plot(retpre)
#plot(retcris)
#plot(retpost)


#fitting FIGARCH - don't forget to change function call to appropriate one
volaCH <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$CH, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

volaHK <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$HK, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

volaSI <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$SI, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

volaRU <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$RU, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

volaEN <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$EN, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

volaUS <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$US, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)


volaJP <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$JP, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

volaGR <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$GR, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

volaFR <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$FR, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

#volaKO <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$KO, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

#volaDU <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$DU, cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

volaDU <- garchOxFit(formula.mean = ~arma(1, 0), formula.var = ~figarch.bbm(1,1), series = dtdiff$DU[1543:2622], cond.dist = "t",include.mean = TRUE, truncation = 100, trace = TRUE, title = NULL, include.var=TRUE)

scale2 <- scale[234:2856]
CH <- volaCH$condvars
HK <- volaHK$condvars
SI <- volaSI$condvars
RU <- volaRU$condvars
EN <- volaEN$condvars
US <- volaUS$condvars
JP <- volaJP$condvars
#KO <- volaKO$condvars
GR <- volaGR$condvars
FR <- volaFR$condvars
DU <- volaDU$condvars

vola <- merge(scale2[2:2623], CH, HK, SI, RU, EN, US, GR, FR, JP)
vola <- vola[,2:10]
vola <- as.zoo(vola)

#cutting data
#dont forget to shift constants
volacris <- vola[1543:1911]
volapre <- vola[1:1542]
DU1 <- DU[1:369]
volacris <- merge(volacris, DU1)
volapost <- vola[1911:2622]
DU <- DU[369:1080]
volapost <- merge(volapost, DU)


#now vola for every period could be easily accessed, i.e. volapost$SI = postcrisis vola in Singapore


symnum( cU <- cor(volapre, method="spearman") )
symnum( cU <- cor(volacris, method="spearman") )
symnum( cU <- cor(volapost, method="spearman") )


#unit root test





#prepare data for grangertest
dfvola <- as.zoo(na.trim(diff(log(vola))))
dfvola.pre <- as.zoo(na.trim(diff(log(volapre))))
dfvola.cris <- as.zoo(na.trim(diff(log(volacris))))
dfvola.post <- as.zoo(na.trim(diff(log(volapost))))

#Lag selection
lags <- VARselect(dfvola, lag.max=25, type="both")
lagspre <- VARselect(dfvola.pre, lag.max=25, type="both")
lagscris <- VARselect(dfvola.cris, lag.max=25, type="both")
lagspost <- VARselect(dfvola.post, lag.max=25, type="both")

lagsmatr <- cbind(lagspre$criteria[1,], lagspre$criteria[2,], lagspre$criteria[3,],
                   lagscris$criteria[1,], lagscris$criteria[2,], lagscris$criteria[3,],
                   lagspost$criteria[1,], lagspost$criteria[2,], lagspost$criteria[3,])
colnames(lagsmatr) <-c("AICpre", "HQpre", "SHpre", "AICcris","HQcris","SHcris", "AICcris","HQcris","SHcris")

matplot(lagsmatr[1:15,], type="l")

#test <- as.matrix(dfvola.pre)
#a <- for(i in 1:10) {
#  for(j in 1:2) { i*j }
#  }
#foreach(i=1:ncol(test), .combine=c) %do%
#  mean(m[,i])
#symnum(grangertest(order=4, data=dfvola.pre, US ~ EN)$'Pr(>F)'[2])
#

for(i in 1:ncol(dfvola.pre)) {
}
  
#US EN
grangertest(order=4, data=dfvola.pre, US ~ EN)
grangertest(order=4, data=dfvola.pre, EN ~ US)
grangertest(order=4, data=dfvola.cris, EN ~ US)
grangertest(order=4, data=dfvola.cris, US ~ EN)
grangertest(order=4, data=dfvola.post, US ~ EN)
grangertest(order=4, data=dfvola.post, EN ~ US)

#SI HK
grangertest(order=4, data=dfvola.pre, SI ~ HK)
grangertest(order=4, data=dfvola.pre, HK ~ SI)
grangertest(order=4, data=dfvola.cris, SI ~ HK)
grangertest(order=4, data=dfvola.cris, HK ~ SI)
grangertest(order=4, data=dfvola.post, SI ~ HK)
grangertest(order=4, data=dfvola.post, HK ~ SI)


#JP ALL
#precris
grangertest(order=4, data=dfvola.pre, JP ~ HK)
grangertest(order=4, data=dfvola.pre, JP ~ SI)
grangertest(order=4, data=dfvola.pre, JP ~ RU)
grangertest(order=4, data=dfvola.pre, JP ~ EN)
grangertest(order=4, data=dfvola.pre, JP ~ US)

grangertest(order=4, data=dfvola.pre, HK ~ JP)
grangertest(order=4, data=dfvola.pre, SI ~ JP)
grangertest(order=4, data=dfvola.pre, RU ~ JP)
grangertest(order=4, data=dfvola.pre, EN ~ JP)
grangertest(order=4, data=dfvola.pre, US ~ JP)

#cris
grangertest(order=4, data=dfvola.cris, JP ~ HK)
grangertest(order=4, data=dfvola.cris, JP ~ SI)
grangertest(order=4, data=dfvola.cris, JP ~ RU)
grangertest(order=4, data=dfvola.cris, JP ~ EN)
grangertest(order=4, data=dfvola.cris, JP ~ US)

grangertest(order=4, data=dfvola.cris, HK ~ JP)
grangertest(order=4, data=dfvola.cris, SI ~ JP)
grangertest(order=4, data=dfvola.cris, RU ~ JP)
grangertest(order=4, data=dfvola.cris, EN ~ JP)
grangertest(order=4, data=dfvola.cris, US ~ JP)

#post-cris
grangertest(order=4, data=dfvola.post, JP ~ HK)
grangertest(order=4, data=dfvola.post, JP ~ SI)
grangertest(order=4, data=dfvola.post, JP ~ RU)
grangertest(order=4, data=dfvola.post, JP ~ EN)
grangertest(order=4, data=dfvola.post, JP ~ US)

grangertest(order=4, data=dfvola.post, HK ~ JP)
grangertest(order=4, data=dfvola.post, SI ~ JP)
grangertest(order=4, data=dfvola.post, RU ~ JP)
grangertest(order=4, data=dfvola.post, EN ~ JP)
grangertest(order=4, data=dfvola.post, US ~ JP)

#Russia Story: independent pre, HSEUJ in cris, C-HSEUJ after
#cris
grangertest(order=4, data=dfvola.cris, RU ~ HK)
grangertest(order=4, data=dfvola.cris, RU ~ SI)
grangertest(order=4, data=dfvola.cris, RU ~ EN)
grangertest(order=4, data=dfvola.cris, RU ~ US)

grangertest(order=4, data=dfvola.cris, HK ~ RU)
grangertest(order=4, data=dfvola.cris, SI ~ RU)
grangertest(order=4, data=dfvola.cris, EN ~ RU)
grangertest(order=4, data=dfvola.cris, US ~ RU)

#post-cris
grangertest(order=4, data=dfvola.post, RU ~ HK)
grangertest(order=4, data=dfvola.post, RU ~ SI)
grangertest(order=4, data=dfvola.post, RU ~ EN)
grangertest(order=4, data=dfvola.post, RU ~ US)

grangertest(order=4, data=dfvola.post, HK ~ RU)
grangertest(order=4, data=dfvola.post, SI ~ RU)
grangertest(order=4, data=dfvola.post, EN ~ RU)
grangertest(order=4, data=dfvola.post, US ~ RU)


#China story: emergings cluster in postcris
grangertest(order=4, data=dfvola.post, HK ~ CH)
grangertest(order=4, data=dfvola.post, CH ~ HK)

grangertest(order=4, data=dfvola.post, SI ~ CH)
grangertest(order=4, data=dfvola.post, CH ~ SI)

grangertest(order=4, data=dfvola.pre, HK ~ CH)
grangertest(order=4, data=dfvola.pre, CH ~ HK)

grangertest(order=4, data=dfvola.post, RU ~ CH)
grangertest(order=4, data=dfvola.post, CH ~ RU)

#main cluster: before, cris, aftermath
#US
grangertest(order=4, data=dfvola.pre, HK ~ US)
grangertest(order=4, data=dfvola.pre, SI ~ US)
grangertest(order=4, data=dfvola.pre, US ~ HK)
grangertest(order=4, data=dfvola.pre, US ~ SI)

grangertest(order=4, data=dfvola.cris, HK ~ US)
grangertest(order=4, data=dfvola.cris, SI ~ US)
grangertest(order=4, data=dfvola.cris, US ~ HK)
grangertest(order=4, data=dfvola.cris, US ~ SI)

grangertest(order=4, data=dfvola.post, HK ~ US)
grangertest(order=4, data=dfvola.post, SI ~ US)
grangertest(order=4, data=dfvola.post, US ~ HK)
grangertest(order=4, data=dfvola.post, US ~ SI)

#EN
grangertest(order=4, data=dfvola.pre, HK ~ EN)
grangertest(order=4, data=dfvola.pre, SI ~ EN)
grangertest(order=4, data=dfvola.pre, EN ~ HK)
grangertest(order=4, data=dfvola.pre, EN ~ SI)

grangertest(order=4, data=dfvola.cris, HK ~ EN)
grangertest(order=4, data=dfvola.cris, SI ~ EN)
grangertest(order=4, data=dfvola.cris, EN ~ HK)
grangertest(order=4, data=dfvola.cris, EN ~ SI)

grangertest(order=4, data=dfvola.post, HK ~ EN)
grangertest(order=4, data=dfvola.post, SI ~ EN)
grangertest(order=4, data=dfvola.post, EN ~ HK)
grangertest(order=4, data=dfvola.post, EN ~ SI)




#COMPONENT ANALYSIS
#preparing data
CHvnorm <- (CH - mean(CH))/sd(CH)
HKvnorm <- (HK - mean(HK))/sd(HK)
SIvnorm <- (SI - mean(SI))/sd(SI)
RUvnorm <- (RU - mean(RU))/sd(RU)
ENvnorm <- (EN - mean(EN))/sd(EN)
USvnorm <- (US - mean(US))/sd(US)
JPvnorm <- (JP - mean(JP))/sd(JP)
DUvnorm <- (DU - mean(DU))/sd(DU)
GRvnorm <- (GR - mean(GR))/sd(GR)
FRvnorm <- (FR - mean(FR))/sd(FR)
KOvnorm <- (KO - mean(KO))/sd(KO)


vnorm <- merge(scale2[2:2623], CHvnorm, HKvnorm, SIvnorm, RUvnorm, ENvnorm, USvnorm, GRvnorm, FRvnorm, JPvnorm, KOvnorm, DUvnorm)
vnorm <- as.zoo(vnorm[,2:12])


#cutting data
vnormcris <- vnorm[1543:1911]
vnormpre <- vnorm[1:1542]
vnormpost <- vnorm[1911:2622]


#generating PCA
prvpre.model <- prcomp(vnormpre)
prvcris.model <- prcomp(vnormcris)
prvpost.model <- prcomp(vnormpost)

#generating ICA 
icvpre.model <- fastICA(vnormpre, 4)
icvcris.model <- fastICA(vnormcris, 4)
icvpost.model <- fastICA(vnormpost, 4)


