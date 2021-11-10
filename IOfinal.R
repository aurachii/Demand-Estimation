#Import Libraries
library(readr)
library(mlogit)
library(gmnl)
library(ivreg)
library(psych)
library(stargazer)
####################################Data Wrangling#################################################################
#Import data
fish<- read_delim("https://peeps.unet.brandeis.edu/~kgraddy/datasets/fishmayreq.txt", 
                  "\t", escape_double = FALSE, trim_ws = TRUE)
#Summary statistics of original sample 
stargazer(as.data.frame(fish),omit.summary.stat = c("p25", "p75") )
#Drop Variables We don't Need  
fish = subset(fish, select = -c(cash,loct,stor,phon,tots,type,size,estb) )
#choice
fish$choice<-0
fish$choice[fish$qual==1]<-'qual1'
fish$choice[fish$qual==2]<-'qual2'
fish$choice[fish$qual==3]<-'qual3'
fish$choice[fish$qual==4]<-'qual4'
fish$choice[fish$qual==5]<-'qual5'

#Average prices per quality and day of the week
#Monday
fishmon <- fish[fish$dayw == 1,]
fishmon<-na.omit(fishmon)
mean(fishmon$pric)
mean(fishmon$pric[fishmon$qual==1])
mean(fishmon$pric[fishmon$qual==2])
mean(fishmon$pric[fishmon$qual==3])
mean(fishmon$pric[fishmon$qual==4])
mean(fishmon$pric[fishmon$qual==5])
#Tuesday
fishtue <- fish[fish$dayw == 2,]
fishtue<-na.omit(fishtue)
mean(fishtue$pric)
mean(fishtue$pric[fishtue$qual==1])
mean(fishtue$pric[fishtue$qual==2])
mean(fishtue$pric[fishtue$qual==3])
mean(fishtue$pric[fishtue$qual==4])
mean(fishtue$pric[fishtue$qual==5])
#Wednesday
fishwed <- fish[fish$dayw == 3,]
fishwed<-na.omit(fishwed)
mean(fishwed$pric)
mean(fishwed$pric[fishwed$qual==1])
mean(fishwed$pric[fishwed$qual==2])
mean(fishwed$pric[fishwed$qual==3])
mean(fishwed$pric[fishwed$qual==4])
mean(fishwed$pric[fishwed$qual==5])
#Thursday
fishthur <- fish[fish$dayw == 4,]
fishthur<-na.omit(fishthur)
mean(fishthur$pric)
mean(fishthur$pric[fishthur$qual==1])
mean(fishthur$pric[fishthur$qual==2])
mean(fishthur$pric[fishthur$qual==3])
mean(fishthur$pric[fishthur$qual==4])
mean(fishthur$pric[fishthur$qual==5])
#Friday
fishfri <- fish[fish$dayw == 5,]
fishfri<-na.omit(fishfri)
mean(fishfri$pric)
mean(fishfri$pric[fishfri$qual==1])
mean(fishfri$pric[fishfri$qual==2])
mean(fishfri$pric[fishfri$qual==3])
mean(fishfri$pric[fishfri$qual==4])
mean(fishfri$pric[fishfri$qual==5])
#Drop NAs 
fish=na.omit(fish)
# Now I'm going to generate price data
fish$price_qual1=NA
fish$price_qual2=NA
fish$price_qual3=NA
fish$price_qual4=NA
fish$price_qual5=NA

for (i in 1:length(fish$qual)) {
  if(fish$qual[i]==1){
    fish$price_qual1[i]=fish$pric[i]
    fish$price_qual2[i]=NA
    fish$price_qual3[i]=NA
    fish$price_qual4[i]=NA
    fish$price_qual5[i]=NA
  }
  else if(fish$qual[i]==2){
    fish$price_qual2[i]=fish$pric[i]
    fish$price_qual1[i]=NA
    fish$price_qual3[i]=NA
    fish$price_qual4[i]=NA
    fish$price_qual5[i]=NA
  }
  else if(fish$qual[i]==3){
    fish$price_qual3[i]=fish$pric[i]
    fish$price_qual1[i]=NA
    fish$price_qual2[i]=NA
    fish$price_qual4[i]=NA
    fish$price_qual5[i]=NA
  }
  else if(fish$qual[i]==4){
    fish$price_qual4[i]=fish$pric[i]
    fish$price_qual1[i]=NA
    fish$price_qual2[i]=NA
    fish$price_qual3[i]=NA
    fish$price_qual5[i]=NA
  }
  else if(fish$qual[i]==5){
  fish$price_qual5[i]=fish$pric[i]
  fish$price_qual1[i]=NA
  fish$price_qual2[i]=NA
  fish$price_qual3[i]=NA
  fish$price_qual4[i]=NA
  } 
  else{
    fish$price_qual1[i]=NA
    fish$price_qual2[i]=NA
    fish$price_qual3[i]=NA
    fish$price_qual4[i]=NA
    fish$price_qual5[i]=NA
  }
}

set.seed(1234)

for (i in 1:length(fish$qual)) {
  if(!is.na(fish$price_qual1[i])){
    fish$price_qual2[i]=fish$price_qual1[i]-round(runif(1, 0, 0.10),2)
    fish$price_qual3[i]=fish$price_qual2[i]-round(runif(1, 0, 0.10),2)
    fish$price_qual4[i]=fish$price_qual3[i]-round(runif(1, 0, 0.10),2)
    fish$price_qual5[i]=fish$price_qual4[i]-round(runif(1, 0, 0.10),2)
  }
}

for (i in 1:length(fish$qual)) {
  if(!is.na(fish$price_qual2[i])){
    fish$price_qual1[i]=fish$price_qual2[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual3[i]=fish$price_qual2[i]-round(runif(1, 0, 0.10),2)
    fish$price_qual4[i]=fish$price_qual3[i]-round(runif(1, 0, 0.10),2)
    fish$price_qual5[i]=fish$price_qual4[i]-round(runif(1, 0, 0.10),2)
  }
}

for (i in 1:length(fish$qual)) {
  if(!is.na(fish$price_qual3[i])){
    fish$price_qual2[i]=fish$price_qual3[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual1[i]=fish$price_qual2[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual4[i]=fish$price_qual3[i]-round(runif(1, 0, 0.10),2)
    fish$price_qual5[i]=fish$price_qual4[i]-round(runif(1, 0, 0.10),2)
  }
}

for (i in 1:length(fish$qual)) {
  if(!is.na(fish$price_qual4[i])){
    fish$price_qual3[i]=fish$price_qual4[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual2[i]=fish$price_qual3[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual1[i]=fish$price_qual2[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual5[i]=fish$price_qual4[i]-round(runif(1, 0, 0.10),2)
  }
}

for (i in 1:length(fish$qual)) {
  if(!is.na(fish$price_qual5[i])){
    fish$price_qual4[i]=fish$price_qual5[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual3[i]=fish$price_qual4[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual2[i]=fish$price_qual3[i]+round(runif(1, 0, 0.10),2)
    fish$price_qual1[i]=fish$price_qual2[i]+round(runif(1, 0, 0.10),2)
  }
}

#Convert our database to type wide 

fish_wide <- mlogit.data(fish, id = "cusn", choice = "choice", varying = c(11:15),
                         shape = "wide", sep = "_")
#####################################Model Estimation############################################################
# Run multinomial logit (base model)
mnl0 <- gmnl(choice~1, data=fish_wide)
ll0=logLik(mnl0)
summary(mnl0)
#Run multinomial logit (full model)
mnl <- gmnl(choice~price, data=fish_wide,model="mnl",method="bhhh")
summary(mnl)
ll1=logLik(mnl)
#Run Nested Logit
nl <- mlogit(choice ~ price, data = fish_wide,
             nests = list(low_quality = c("qual5", "qual4"), medium_quality=c("qual3"),
                          high_quality = c("qual2","qual1")), un.nest.el = TRUE)
summary(nl)
#Run Multinomial Logit without intercepts
mnli<-gmnl(choice~price-1, data=fish_wide,model="mnl",method="bhhh")
summary(mnli)
#Run Multinomial Logit with relative effects
mnlr<-gmnl(choice~1|price, data=fish_wide,model="mnl",method="bhhh")
summary(mnlr)
#######################################Elasticities#############################################################
M<-model.matrix(mFormula(choice~price),data=fish_wide)
coef_names<-colnames(M)
fish_elast<-NULL
for(i in unique(fish_wide$chid)){
  M_i<-M[fish_wide$chid==i,]
  rownames(M_i)<-c("qual1","qual2","qual3","qual4","qual5")
  fish_elast[[i]]<-M_i
}
computeElast<-function(data,betai,coef=5){
  E_chid<-sapply(data,FUN=mnlElast,betai=betai,coef=coef,simplify="array")
  E<-apply(E_chid,c(1,2),mean)
}

mnlProb<-function(x,betai){
  u<-x%*%t(betai)
  num<-exp(u)
  tprobs<-t(num)/colSums(num)
  probs<-t(tprobs)
  shares<-rowMeans(probs)
  res<-list(shares=shares,probs=probs)
  return(res)
}

mnlDeriv <- function(betaxi, probi) {
  nJ <- dim(probi)[1]
  nD <- dim(probi)[2]
  betaxim <- matrix(betaxi, nJ, nD, byrow = TRUE)
  x <- rep(1, nJ)
  eta <- -(1 / x) %*% t(rep(1, nJ)) * tcrossprod(betaxim * probi, probi) / nD
  diag(eta) <- (1 / x) * rowSums(betaxim * probi * (1 - probi)) / nD
  return(eta)
} 

mnlElast <- function(x, betai, coef = 5) {
  nJ <- nrow(x)
  probs <- mnlProb(x, betai)
  shares <- probs$shares
  shares <- matrix(shares, nJ, nJ, byrow = TRUE)
  eta <- x[, coef] / shares * mnlDeriv(betai[, coef], probs$probs)
  return(eta)
}

#Computing elasticities

round(computeElast(fish_elast, t(coef(mnl))), 3)


