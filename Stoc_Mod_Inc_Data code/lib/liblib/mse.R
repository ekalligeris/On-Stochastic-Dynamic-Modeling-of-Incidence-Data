
model.data = create_model_dataset(serie$x, serie$y, serie$min_temp, serie$arma, serie$keep, nb_temps)#<-----------------
sas<-model.data
y= sas$y
x= 1:length(sas$y)
x=sas$x
x2= sas$x2
x3= sas$x3
x4= sas$x4
c1= sas$c1
s1= sas$s1
c2= sas$c2
s2= sas$s2
c3= sas$c3
s3= sas$s3
min_temp=sas$min_temp
arma=sas$arma

modelparp<-lm(y~x+x2+c1+s1+c2+s2+c3+s3) #23
model<-modelparp
# 
# ### Predictions 
if(nb_predict > 0) {
  d = data.frame( x= seq(max(serie$x) + 1, len=nb_predict) , y=NA, training=F, keep=F)
  if(dataset.params$alert) {
    d$alert.ref = 0
  }
  serie = rbind(serie, d)
  rm(d)
}
lo<-matrix(90,nrow =83,ncol = 1 )

# In retrospective setting : only for the  Training Period (TP)
# In prospective setting : for TP + following year
# We only keep the last nb_app observations in x_new (+ the nb_predict NA if we are in prospective setting)
predict.data = create_model_dataset( serie$x, NA, serie$min_temp, serie$arma, NULL, nb_temps) #<-----------------
pred.plim2 <- predict(model, newdata = predict.data, interval='prediction', level = confidence.limit /100)#<-----------------

# # Baseline level
serie$fit <- pred.plim2[,'fit']
y<-serie$y
pred.plim3 <- predict(model, newdata = predict.data, interval='prediction', level = confidence.limit /100)#<-----------------
y<-serie$y

t1<-c(finalsel1st[1]:finalsel1st[n1])
t2<-c(finalsel2nd[1]:finalsel2nd[n2])

y1<-y[finalsel1st[1]:finalsel1st[n1]]
y2<-y[finalsel2nd[1]:finalsel2nd[n2]]

prof<-y-serie$fit

y1<-prof[finalsel1st[1]:finalsel1st[n1]]
y2<-prof[finalsel2nd[1]:finalsel2nd[n2]]
# #kanw polynomial fit se kathe 1 apo tis 4 sinolika periodous
#
# # rsqr1<-c()
# # rsqr2<-c()
pval1<-matrix(data=0,nrow=5,ncol=6)
pval2<-matrix(data=0,nrow=5,ncol=5)

for (i in 1:5){
  y_1<-lm(y1~poly(t1,i,raw=TRUE))
  #rsqr1[i]<-summary(y_1)$r.squared
  pvalue<-summary(y_1)$coefficients[,4]
  pval1[i,1:(i+1)]<-t(as.matrix(pvalue))
  
}

y_hat_epid1<-c()

if (pval1[1,1]<0.3 & pval1[1,2]<0.3){
  y_1<-lm(y1~poly(t1,1,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]
  }
}else if (pval1[2,1]<0.3 & pval1[2,2]<0.3 & pval1[2,3]<0.3){
  y_1<-lm(y1~poly(t1,2,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2
  }
}else if (pval1[3,1]<0.3 & pval1[3,3]<0.3 & pval1[3,4]<0.3){
  y_1<-lm(y1~poly(t1,3,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2+coef(y_1)[4]*t1[k]^3
  }
}else if (pval1[4,1]<0.3 & pval1[4,2]<0.3 & pval1[4,3]<0.3 & pval1[4,4]<0.3 & pval1[4,5]<0.3){
  y_1<-lm(y1~poly(t1,4,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2+coef(y_1)[4]*t1[k]^3+coef(y_1)[5]*t1[k]^4
  }
}else if (pval1[5,1]<0.3 & pval1[5,2]<0.3 & pval1[5,3]<0.3 & pval1[5,4]<0.3 & pval1[5,5]<0.3 & pval1[5,6]<0.3){
  y_1<-lm(y1~poly(t1,5,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2+coef(y_1)[4]*t1[k]^3+coef(y_1)[5]*t1[k]^4+coef(y_1)[6]*t1[k]^5
  }
}else{
  y_1<-lm(y1~poly(t1,2,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2
  }
}

for (i in 1:4){
  y_2<-lm(y2~poly(t2,i,raw=TRUE))
  #rsqr2[i]<-summary(y_2)$r.squared
  pvalue<-summary(y_2)$coefficients[,4]
  pval2[i,1:(i+1)]<-t(as.matrix(pvalue))
  
}

y_hat_epid2<-c()

if (pval2[1,1]<0.3 & pval2[1,2]<0.3){
  y_2<-lm(y2~poly(t2,1,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]
  }
}else if (pval2[2,1]<0.3 & pval2[2,2]<0.3 & pval2[2,3]<0.3){
  y_2<-lm(y2~poly(t2,2,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]+coef(y_2)[3]*t2[k]^2
  }
}else if (pval2[3,1]<0.3 & pval2[3,2]<0.3 & pval2[3,3]<0.3 & pval2[3,4]<0.3){
  y_2<-lm(y2~poly(t2,3,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]+coef(y_2)[3]*t2[k]^2+coef(y_2)[4]*t2[k]^3
  }
}else if (pval2[4,1]<0.3 & pval2[4,2]<0.3 & pval2[4,3]<0.3 & pval2[4,4]<0.3 & pval2[4,5]<0.3){
  y_2<-lm(y2~poly(t2,4,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]+coef(y_2)[3]*t2[k]^2+coef(y_2)[4]*t2[k]^3+coef(y_2)[5]*t2[k]^4
  }
}else {
  y_2<-lm(y2~poly(t2,2,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]+coef(y_2)[3]*t2[k]^2
  }
}

pred.plim3[(finalsel1st[1]:finalsel1st[n1]),1]<-pred.plim2[(finalsel1st[1]:finalsel1st[n1]),1]+y_hat_epid1[1:n1]

pred.plim3[(finalsel2nd[1]:finalsel2nd[n2]),1]<-pred.plim2[(finalsel2nd[1]:finalsel2nd[n2]),1]+y_hat_epid2[1:n2]

serie$Estimated_valuesparp<-pred.plim3[,'fit']




model.data = create_model_dataset(serie$x, serie$y, serie$min_temp, serie$arma, serie$keep, nb_temps)#<-----------------
sas<-model.data
y= sas$y
x= 1:length(sas$y)
x=sas$x
x2= sas$x2
x3= sas$x3
x4= sas$x4
c1= sas$c1
s1= sas$s1
c2= sas$c2
s2= sas$s2
c3= sas$c3
s3= sas$s3
min_temp=sas$min_temp
arma=sas$arma

modelkall<-lm(y~x+x2+c1+s1+c2+s2+c3+s3+min_temp+arma) #23
model<-modelkall
#
# ### Predictions
if(nb_predict > 0) {
  d = data.frame( x= seq(max(serie$x) + 1, len=nb_predict) , y=NA, training=F, keep=F)
  if(dataset.params$alert) {
    d$alert.ref = 0
  }
  serie = rbind(serie, d)
  rm(d)
}
lo<-matrix(90,nrow =83,ncol = 1 )

# In retrospective setting : only for the  Training Period (TP)
# In prospective setting : for TP + following year
# We only keep the last nb_app observations in x_new (+ the nb_predict NA if we are in prospective setting)
predict.data = create_model_dataset( serie$x, NA, serie$min_temp, serie$arma, NULL, nb_temps) #<-----------------
pred.plim2 <- predict(model, newdata = predict.data, interval='prediction', level = confidence.limit /100)#<-----------------

# Baseline level
serie$fit <- pred.plim2[,'fit']
y<-serie$y
pred.plim3 <- predict(model, newdata = predict.data, interval='prediction', level = confidence.limit /100)#<-----------------
y<-serie$y

t1<-c(finalsel1st[1]:finalsel1st[n1])
t2<-c(finalsel2nd[1]:finalsel2nd[n2])

y1<-y[finalsel1st[1]:finalsel1st[n1]]
y2<-y[finalsel2nd[1]:finalsel2nd[n2]]

prof<-y-serie$fit

y1<-prof[finalsel1st[1]:finalsel1st[n1]]
y2<-prof[finalsel2nd[1]:finalsel2nd[n2]]
#kanw polynomial fit se kathe 1 apo tis 4 sinolika periodous

# rsqr1<-c()
# rsqr2<-c()
pval1<-matrix(data=0,nrow=5,ncol=6)
pval2<-matrix(data=0,nrow=5,ncol=5)

for (i in 1:5){
  y_1<-lm(y1~poly(t1,i,raw=TRUE))
  #rsqr1[i]<-summary(y_1)$r.squared
  pvalue<-summary(y_1)$coefficients[,4]
  pval1[i,1:(i+1)]<-t(as.matrix(pvalue))

}

y_hat_epid1<-c()

if (pval1[1,1]<0.3 & pval1[1,2]<0.3){
  y_1<-lm(y1~poly(t1,1,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]
  }
}else if (pval1[2,1]<0.3 & pval1[2,2]<0.3 & pval1[2,3]<0.3){
  y_1<-lm(y1~poly(t1,2,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2
  }
}else if (pval1[3,1]<0.3 & pval1[3,3]<0.3 & pval1[3,4]<0.3){
  y_1<-lm(y1~poly(t1,3,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2+coef(y_1)[4]*t1[k]^3
  }
}else if (pval1[4,1]<0.3 & pval1[4,2]<0.3 & pval1[4,3]<0.3 & pval1[4,4]<0.3 & pval1[4,5]<0.3){
  y_1<-lm(y1~poly(t1,4,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2+coef(y_1)[4]*t1[k]^3+coef(y_1)[5]*t1[k]^4
  }
}else if (pval1[5,1]<0.3 & pval1[5,2]<0.3 & pval1[5,3]<0.3 & pval1[5,4]<0.3 & pval1[5,5]<0.3 & pval1[5,6]<0.3){
  y_1<-lm(y1~poly(t1,5,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2+coef(y_1)[4]*t1[k]^3+coef(y_1)[5]*t1[k]^4+coef(y_1)[6]*t1[k]^5
  }
}else{
  y_1<-lm(y1~poly(t1,2,raw=TRUE))
  for (k in 1:n1){
    y_hat_epid1[k]<-coef(y_1)[1]+coef(y_1)[2]*t1[k]+coef(y_1)[3]*t1[k]^2
  }
}

for (i in 1:4){
  y_2<-lm(y2~poly(t2,i,raw=TRUE))
  #rsqr2[i]<-summary(y_2)$r.squared
  pvalue<-summary(y_2)$coefficients[,4]
  pval2[i,1:(i+1)]<-t(as.matrix(pvalue))

}

y_hat_epid2<-c()

if (pval2[1,1]<0.3 & pval2[1,2]<0.3){
  y_2<-lm(y2~poly(t2,1,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]
  }
}else if (pval2[2,1]<0.3 & pval2[2,2]<0.3 & pval2[2,3]<0.3){
  y_2<-lm(y2~poly(t2,2,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]+coef(y_2)[3]*t2[k]^2
  }
}else if (pval2[3,1]<0.3 & pval2[3,2]<0.3 & pval2[3,3]<0.3 & pval2[3,4]<0.3){
  y_2<-lm(y2~poly(t2,3,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]+coef(y_2)[3]*t2[k]^2+coef(y_2)[4]*t2[k]^3
  }
}else if (pval2[4,1]<0.3 & pval2[4,2]<0.3 & pval2[4,3]<0.3 & pval2[4,4]<0.3 & pval2[4,5]<0.3){
  y_2<-lm(y2~poly(t2,4,raw=TRUE))
  for (k in 1:n2){
    y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]+coef(y_2)[3]*t2[k]^2+coef(y_2)[4]*t2[k]^3+coef(y_2)[5]*t2[k]^4
  }
}else {
y_2<-lm(y2~poly(t2,2,raw=TRUE))
for (k in 1:n2){
  y_hat_epid2[k]<-coef(y_2)[1]+coef(y_2)[2]*t2[k]+coef(y_2)[3]*t2[k]^2
}
}

pred.plim3[(finalsel1st[1]:finalsel1st[n1]),1]<-pred.plim2[(finalsel1st[1]:finalsel1st[n1]),1]+y_hat_epid1[1:n1]

pred.plim3[(finalsel2nd[1]:finalsel2nd[n2]),1]<-pred.plim2[(finalsel2nd[1]:finalsel2nd[n2]),1]+y_hat_epid2[1:n2]

serie$Estimated_valueskall<-pred.plim3[,'fit']
