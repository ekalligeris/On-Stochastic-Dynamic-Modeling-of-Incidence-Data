#DATA PREPARATION----
# install.packages("readxl")
# install.packages("stats")

library(itsmr)
library(readxl)
library(stats)
library(changepoint)

y<-read_excel("gri.xlsx")
y<-y$gri
descriptives_temps_by_week<-read_excel("descriptives_temps_by_week.xlsx")
min_temp<-descriptives_temps_by_week$min
# plot(ts(y))

n<-length(y)

t<-1:105
t2<-t^2
c1<-cos(2*pi*t/52.179)
s1<-sin(2*pi*t/52.179)
c2<-cos(4*pi*t/52.179)
s2<-sin(4*pi*t/52.179)

options(warn=-1) #WARNINGS OFF

#Number_of_Changepoints IN MEAN
cptmean<-cpt.mean(y,penalty="Manual" ,method = "SegNeigh",pen.value = 0.05,class = TRUE)
cpt1<-cpts.ts(cptmean) #Number_of_Changepoints
cpts.ts(cptmean)

cpt
cptmean@param.es
c
# v1.crops=cpt.var(y,method="PELT",penalty="CROPS",pen.value=c(5,500))
# plot(v1.crops,diagnostic=TRUE)
# 
# dynProg.mean <- function(y, Kmax, Lmin=1) 
# {
#   Nr  <- Kmax - 1
#   n <- length(y)
#   V <- matrix(Inf, nrow = n, ncol = n)
#   for (j1 in (1:(n-Lmin+1))){
#     for (j2 in ((j1+Lmin-1):n)) {
#       yj <- y[j1:j2]
#       nj <- j2-j1+1
#       V[j1,j2] <- sum(yj^2) - (sum(yj)^2)/nj
#     }
#   }
#   
#   Maximum_Likelihood<- vector(length=Kmax)
#   Maximum_Likelihood[1] <- V[1,n]
#   D <- V[,n] 
#   Pos <- matrix(nrow = n, ncol = Nr) 
#   Pos[n,] <- rep(n,Nr)    
#   tau.mat <- matrix(nrow = Nr,ncol = Nr) 
#   for (k in 1:Nr){
#     for (j in 1:(n-1)){
#       dist <- V[j,j:(n-1)] + D[(j+1):n]
#       D[j] <- min(dist)
#       Pos[j,1] <- which.min(dist) + j
#       if (k > 1) { Pos[j,2:k] <- Pos[Pos[j,1],1:(k-1)] }
#     }
#     Maximum_Likelihood[k+1] <- D[1]
#     tau.mat[k,1:k] <- Pos[1,1:k]-1
#   }
#   out <- list(Test=tau.mat, obj=data.frame(Number_of_Changepoints=(1:Kmax),Maximum_Likelihood=Maximum_Likelihood))
#   return(out)
# }
# 
# 
# Kmax <- 10 # maximum number of segments
# Lmin <- 1  # minimum length of a segment
# 
# r <- dynProg.mean(y,Kmax,Lmin)
# pl <- ggplot(data=r$obj) + geom_line(aes(Number_of_Changepoints,Maximum_Likelihood), size=0.5, colour="red")+ 
#   geom_point(aes(Number_of_Changepoints,Maximum_Likelihood), size=3, colour="black")
# print(pl)
# 
# Kopt <- 6
# print(r$Test[(Kopt-1),1:(Kopt-1)])

#me tis antistixes times tous
y1<-y[1:cpt1[1]]
y2<-y[(cpt1[2]+1):cpt1[3]] #Mexri edw einai h periodos 2014/2015
y3<-y[(cpt1[4]+1):n] #Edw einai h periodos 2015/2016
# 
# t.test(y[1:13],y[14:25])
# t.test(y[26:66],y[67:74])

#tis enwnw kai etsi exw thn baseline xronoseira
ynonepid<-c(y1,y2,y3)
plot(ts(ynonepid))

#ARIMA(1,1) SIMULATION ----
mario<-arma(ynonepid, p = 1, q = 1)
sd<-sqrt(mario$sigma2)
phi<-mario$phi
theta<-mario$theta
sdo<-sd(y)
threshold1<-70
threshold2<-60
check1<-0
check2<-0

while (check1<threshold1 | check2<threshold2){
  
  e<-rnorm(n,0,sdo)
  
  ARMA<-abs(arima.sim(n=n,list(order=c(1,0,1),ar=phi,ma=theta),innov = e))
  
  #ysim.nonepidARMA<-  21.789133 - 0.290946*t + 0.001489*t2 - 9.567230*c1 + 12.150802*s1 + 2.991121*c2 - 1.411999*s2 + 0.176566*min_temp + ARMA 
  ysim.nonepidARMA<-  21.789133 - 0.290946*t - 9.567230*c1 + 12.150802*s1 + e
  ysim.nonepidARMA<-as.numeric(unlist(abs(ysim.nonepidARMA)))

#____check1ST----
x<-18:20
selec1<-sample(x,1)


if (selec1==18){
  
  x<-21:24
  ub<-sample(x,1)
  selec2<-13:ub
  
}else if (selec1==19){
  
  x1<-13:15
  x2<-23:25
  lb<-sample(x1,1)
  ub<- sample(x2,1)
  selec2<-lb:ub
  
}else{
  x<-15:18
  lb<-sample(x,1)
  selec2<-lb:26
}

finalsel1st<-selec2

 
#____check2nd----

  x<-68:70
  selec1<-sample(x,1)
  
  if (selec1==68){
    x<-72:74
    ub<-sample(x,1)
    selec2<-65:ub
    
  }else if (selec1==69){
    
    x1<-65:66
    x2<-72:74
    lb<-sample(x1,1)
    ub<- sample(x2,1)
    selec2<-lb:ub
    
  }else{
    
    x<-65:67
    lb<-sample(x,1)
    selec2<-lb:74
    
  }
  
  finalsel2nd<-selec2

  mu1<-mean(y[finalsel1st])
  
  mu2<-mean(y[finalsel2nd])
  
  sd1<-sd(y[finalsel1st])
  sd2<-sd(y[finalsel2nd])
  
  n1<-length(finalsel1st)
  n2<-length(finalsel2nd)
  
  ynew1<-rnorm(n1,mu1,sd1)
  ynew2<-rnorm(n2,mu2,sd2)
  
  ysim.nonepidARMA[finalsel1st[1]:finalsel1st[n1]]<-ynew1
  ysim.nonepidARMA[finalsel2nd[1]:finalsel2nd[n2]]<-ynew2
  
  #Number_of_Changepoints IN MEAN
  
  ysim.nonepidARMA<-ts(ysim.nonepidARMA)
  
  cptmean<-cpt.mean(ysim.nonepidARMA,penalty="AIC",method = "SegNeigh")

  check1<-coef(cptmean)$mean[2]
  check2<-coef(cptmean)$mean[4]
}


#Number_of_Changepoints IN MEAN

cpt1<-cpts.ts(cptmean) #Number_of_Changepoints
# plot(cptmean,type = "l")

finalsel1st[1]:finalsel1st[n1]
finalsel2nd[1]:finalsel2nd[n2]
out1<-finalsel1st[1]:finalsel1st[n1]
out2<-finalsel2nd[1]:finalsel2nd[n2]
library(writexl)
# ysim.nonepidARMA<-as.data.frame(ysim.nonepidARMA)
# ysim.nonepidARMA<-ysim.nonepidARMA$x
write.table(unlist(abs(ysim.nonepidARMA)), "1503553691-599e689b9f9d7",col.names = FALSE,row.names = F, dec = ".",sep = '')

write_xlsx(as.data.frame(abs(ysim.nonepidARMA)), "data.xlsx")

write_xlsx(as.data.frame(ARMA), "difs.xlsx",col_names=T)



