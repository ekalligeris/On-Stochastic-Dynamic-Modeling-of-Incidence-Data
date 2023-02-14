rm(list=ls()) #Clear Environment
dev.off() # Clear Plots
cat("\014")  #Clear console

library(readxl)
library(arm)
lib.path1 <- function(file, ...) {
  return(paste('lib/', file, sep=''))
}
goal<-1
itfin<-goal
selected.models.final<-matrix(0,nrow = goal,ncol = 1) 
metritis=1

AIC_1<-matrix(0,nrow = 12,ncol = itfin)
BIC_1<-matrix(0,nrow = 12,ncol = itfin)
AICc_1<-matrix(0,nrow = 12,ncol = itfin)

for (lala in 1:itfin){
  
it<-1

selected.models<-matrix(0,nrow = it,ncol = 1)
m_kall<-matrix(0,nrow = it,ncol = 1)
m_parp<-matrix(0,nrow = it,ncol = 1)
m_serf<-matrix(0,nrow = it,ncol = 1)



for (j in 1:it){
source(lib.path1("runer.R"))
}

selected.models.final[metritis,]<-selected.models[1]
metritis<-metritis+1
}

selected.models<-as.data.frame(selected.models)
MSE<-as.data.frame(cbind(m_kall,m_parp,m_serf))
colnames(MSE) <- c("m_kall", "m_parp","m_serf")
write_xlsx(MSE, "mse_sim.xlsx", col_names=T)
colnames(selected.models)<-c("SELECTED MODEL")
write_xlsx(selected.models, "selected.models.xlsx", col_names=T)
MSE
selected.models.final

library(MuMIn)
AICMIN<-c()
BICMIN<-c()
AICCMIN<-c()

for (i in 1:12){
  AICMIN[i]<-mean(AIC_1[i,])
  BICMIN[i]<-mean(BIC_1[i,])
  AICCMIN[i]<-mean(AICc_1[i,])
}


AICMIN
BICMIN
AICCMIN

write_xlsx(as.data.frame(AIC_1), "AIC_1.xlsx",col_names=T)
write_xlsx(as.data.frame(BIC_1), "BIC_1.xlsx",col_names=T)
write_xlsx(as.data.frame(AICc_1), "AICc_1.xlsx",col_names=T)

write_xlsx(as.data.frame(AICMIN), "AICMIN.xlsx",col_names=T)
write_xlsx(as.data.frame(BICMIN), "BICMIN.xlsx",col_names=T)
write_xlsx(as.data.frame(AICCMIN), "AICCMIN.xlsx",col_names=T)

e_hat_kall<-serie$y-serie$Estimated_valueskall
library(nortest)
library(snpar)

ks.test(e_hat_kall,pnorm)
ad.test(e_hat_kall)
runs.test(e_hat_kall)