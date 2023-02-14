################################################################################
# $Id: model.R 459 2012-05-31 18:03:30Z cturbelin $
# Periodic Regression created by Camille Pelat - Reseau Sentinelles - UMR-S 707 INSERM-UPMC France             
# http://www.u707.jussieu.fr/periodic_regression
# Pelat, C., P. Y. Boelle, et al. (2007). "Online detection and quantification of epidemics".BMC Med Inform Decis Mak, 7: 29.
# This script is the property of INSERM. The use of this script and publication 
# from its result must contains an approriate citation (using one of the line 
# above or contacting authors to get one). Any modification or partial reuse 
# of these scripts must contain this header.                           
# @author Camille Pelat <pelat@u707.jussieu.fr> primary version
# @author Clement Turbelin <turbelin@u707.jussieu.fr> refactoring, coding standards
################################################################################

###############################################################################
# This script runs the model fitting 
# in the retrospective setting it calculates the cumulated excess     
# in the prospective setting it calculates the forecast values for the next year     
# It is called in the script run_modele.R after the setting of the parameters 
###############################################################################

# Structure to contain results
struct.result = list()

# cleanup
cleanup.files('regression*.*')

# nb_app = training set
nb_app = round(nb_app)

# nb_temps: maximum number of observations in 1 year
nb_temps = switch(dataset.params$time_step,  'day'=365.25, 'week'=52.179, 'month'=12, 'quarter'=4)

if(setting == 'prospective') {
  nb_predict = round(nb_temps)
} else {
  nb_predict = 0
} 

# reads the dataset
serie = read.dataset(dataset.params)

pruning.threshold = NULL
# prune.dataset create var pruning.threshold if pruning method allows a threshold value
keep<-c()
for (i in 1:n){
  keep[i]<-"TRUE"
}
keep[finalsel1st[1]:finalsel1st[n1]]<-"FALSE"
keep[finalsel2nd[1]:finalsel2nd[n2]]<-"FALSE"
keep<-unlist(keep)
keep<-as.logical(keep)
serie$keep = keep #prune.dataset( serie, choix_seuil, pruning.value)

### Training Period 
# index of the observations in the training period

# registered models
models.formula = list(
 "M11"="y~x+c1+s1+min_temp+arma",#<-----------------
 "M12"="y~x+c1+s1+c2+s2+min_temp+arma",#<-----------------
 "M13"="y~x+c1+s1+c2+s2+c3+s3+min_temp+arma",#<-----------------
 "M21"="y~x+x2+c1+s1+min_temp+arma",#<-----------------
 "M22"="y~x+x2+c1+s1+c2+s2+min_temp+arma",#<-----------------
 "M23"="y~x+x2+c1+s1+c2+s2+c3+s3+min_temp+arma",#<-----------------
 "M31"="y~x+x2+x3+c1+s1+min_temp+arma",#<-----------------
 "M32"="y~x+x2+x3+c1+s1+c2+s2+min_temp+arma",#<-----------------
 "M33"="y~x+x2+x3+c1+s1+c2+s2+c3+s3+min_temp+arma",#<-----------------
 "M41"="y~x+x2+x3+x4+c1+s1+min_temp+arma",#<-----------------
 "M42"="y~x+x2+x3+x4+c1+s1+c2+s2+min_temp+arma",#<-----------------
 "M43"="y~x+x2+x3+x4+c1+s1+c2+s2+c3+s3+min_temp+arma"#<-----------------
)


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

model11<-lm(y~x+c1+s1+min_temp+arma)
model12<-lm(y~x+c1+s1+c2+s2+min_temp+arma)
model13<-lm(y~x+c1+s1+c2+s2+c3+s3+min_temp+arma)
model21<-lm(y~x+x2+c1+s1+min_temp+arma)
model22<-lm(y~x+x2+c1+s1+c2+s2+min_temp+arma)
model23<-lm(y~x+x2+c1+s1+c2+s2+c3+s3+min_temp+arma)
model31<-lm(y~x+x2+x3+c1+s1+min_temp+arma)
model32<-lm(y~x+x2+x3+c1+s1+c2+s2+min_temp+arma)
model33<-lm(y~x+x2+x3+c1+s1+c2+s2+c3+s3+min_temp+arma)
model41<-lm(y~x+x2+x3+x4+c1+s1+min_temp+arma)
model42<-lm(y~x+x2+x3+x4+c1+s1+c2+s2+min_temp+arma)
model43<-lm(y~x+x2+x3+x4+c1+s1+c2+s2+c3+s3+min_temp+arma)

if( nrow(model.data) < 10 ) {
 write( export.json( list(error_code="too_few_data") ), file="regression-model.json")
 stop("Dataset contains too few data (less than 10 rows)")
}

### Models estimated on the Training Period 
# if (model_choice == 'select')
# {
#   if(setting == 'retrospective') {
#     selection_result = model_select_retrospective(model.data, do.graph = T)
#   } else {
#     # prospective selection
#     selection_result = model_select_prospective(model.data, do.graph = T)
#   }
#   model = selection_result$model
#   struct.result$model =  selection_result$final
# } else {
#   # get the model directly from user choice
#   model = evaluate_model( model_choice, model.data, use.model.name = T)
#   struct.result$model =  model_choice
# }  
# 
# 
# save.selected.model<-model

modelserf<-lm(y~t+c1+s1) #11serfling
model<-modelserf
gc() # run garbage collector to release memory 

### Predictions 
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
} else{
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

serie$Estimated_valuesserf<-pred.plim3[,'fit']


rm(pred.plim2, predict.data, model.data)

# # Upper forecast limit
# u.alpha <- qnorm( ( 100 - ( 100 - confidence.limit ) / 2 ) / 100 )	
# 
# serie$upper <- serie$Estimated_valuesserf + u.alpha * sqrt( var(model$residuals) )  
# serie$upper2 <- serie$fit + u.alpha * sqrt( var(model$residuals) )  

# ### Table 1 (for both retrospective and prospective setting) 
# # baseline et Upper Forecast Limit pour chaque pas de temps 
# nb_digits=3
# 
# serie$fit[serie$fit < 0] = 0
# serie$upper[serie$upper < 0] = 0
# 
# # cols to export and export names (order keeped for compatibilty with older version)
# export.cols = c('x'="index",'date'="date",'week.start'="week.start",'y'="observations",'fit'="predicted_baseline",'upper'="threshold",'Estimated_valuesserf'="Estimated_valuesserf")
# 
# # get dates of the serie
# if (dataset.params$time_step == "week") {
#   d = calc_time_date(serie$x, dataset.params$time_step, dataset.params$date_start, return.monday=T)
#   serie$date = d$dates
#   serie$week.start = d$week.start
# } else {
#   serie$date = calc_time_date(serie$x, dataset.params$time_step, dataset.params$date_start)
#   export.cols = export.cols[ names(export.cols) != "week.start"] # dont keep week.start
# }
# # vector of prediction
# ii = (length(serie$x) - nb_predict - nb_app + 1): length(serie$x)
# 
# export.data(serie[ii,names(export.cols)],file="regression_result",format="csv",rename=export.cols)
# 
# ### Table 2 (retrospective setting only) ###
# # Cumulate excess and dates of start/end
# if( setting=='retrospective') {
#   epid =  serie$y > serie$upper2
#   epidemics = search.epidemic.period( epid, epidemic.time)
#   if( nrow(epidemics) > 0 ) {
#     epidemics = epidemics[ complete.cases(epidemics),] # avoid NA (should not occurs)
#     epidemics$end[1]<-finalsel1st[n1]
#     epidemics$end[2]<-finalsel2nd[n2]
#     epidemics$excess_cases = NA
#     epidemics$expected_cases = NA
#     epidemics$cases = NA
#     for(i in 1:nrow(epidemics) ) {
#       e = epidemics$start[i]:epidemics$end[i]
#       epidemics$excess_cases[i] = sum(serie$y[e] - serie$fit[e],na.rm=T)
#       epidemics$expected_cases[i] = sum(serie$fit[e],na.rm=T)
#       epidemics$cases[i] = sum(serie$y[e],na.rm=T)
#     }
#     
#     epidemics$excess_percentage = round(100 * epidemics$excess_cases / epidemics$expected_cases)
#     
#     export.data(epidemics, file="regression_epids", format="csv")
# 
#     epidemics$delay = epidemics$end - epidemics$start
#     struct.result$epid = list()
#     struct.result$epid$count =  nrow(epidemics) # number of detected epidemic
#     struct.result$epid$delay_max = max(epidemics$delay,na.rm=T)
#     struct.result$epid$delay_min = min(epidemics$delay,na.rm=T)
#     struct.result$epid$delay_med = median(epidemics$delay,na.rm=T)
#   }
# } # end of if( setting=='retrospective')

# ###
# # GRAPHIC 
# ###
# library(ggplot2)
# 
# 
# 
# graph.open(file='regression_curve.png', width=950, height=400)
#   par(las=1, lend=2, tcl=-0.3, mar=c(2, 4, 2.5, .5), mgp=c(3, 0.3, 0), bty='l', xaxs='i', yaxs='r')
#   ylim = range(serie$y, na.rm=T)
#   ymax = ylim[2]
#   ylim[2] = ylim[2] * 1.15
#   plot(range(serie$x), ylim, type = 'n', xaxt='n', xlab = '', ylab = dataset.params$units)
#   title(main=dataset.params$title)  
#   ticks = calc_time_ticks(serie$date, dataset.params$time_step)
#   if(setting == 'retrospective') 
#   {
#     # show epidemics
#     if(nrow(epidemics) > 0) {
#       for(i in 1:nrow(epidemics)) {
#         e = epidemics[i,]
#         rect(e$start - 0.5, ylim[1], e$end + 0.5, ymax, col=color.epid, density=-30, border=NA)
#       }
#     }
#   } else {
#     # show training period
#     rr = range(serie$x[serie$training])
#     y = ymax * 1.05
#     segments(rr[1], y, rr[2], y, col="lightgrey", lwd=1)
#     segments(rr[1], 0, rr[1], y, col="lightgrey", lty=2)
#     segments(rr[2], 0, rr[2], y, col="lightgrey", lty=2)
#   }
#   
#   lines(serie$x, serie$y, type="l", col=color.serie)
#   lines(serie$x, serie$fit, col=color.model, lty=1)
#   lines(serie$x, serie$upper, col=color.Estimated_valuesserf, lty=2) 
#   lines(serie$x, serie$Estimated_valuesserf, col=color.Estimated_valuesserf, lty=1)
# 
#   
#   dd = serie$date[ticks]
#   if( length(unique(dd)) < 3) {
#    # too few ticks
#    ticks = axTicks(1)
#    axis(1, at=ticks, labels=serie$date[ticks]) 
#   } else {
#     ticks2 = sapply(1:length(ticks), function(i) { return( floor( sum(ticks[i:(i + 1)]) / 2)) })
#     dd = dd[ !is.na(ticks2) ]
#     ticks2 = ticks2[ !is.na(ticks2) ]
#     if( dataset.params$time_step == "day") {
#       dd = format(dd, format="%Y")
#     } else {
#       # week and month number
#       dd = floor(dd / 100)
#     }
#     axis(side=1, at=ticks, labels=F, las=1, tick=T)
#     axis(side=1, at=ticks2, labels=dd, las=1, tick=F, mgp=c(3,0.3,0))	
#   }
#   
#   # legend params
#   lg = list(
#   		legend=c('Observations','Estimated_valuesserf','Baseline', paste('Upper bound of PI at ', confidence.limit,'%',sep='')),
#   		col = c(color.serie,color.Estimated_valuesserf, color.model, color.Estimated_valuesserf),
#   		fill = NULL
#   )
#   
#   if(setting == 'retrospective') {
#   	lg$legend = c(lg$legend, 'Epidemic')
#   	lg$col = c(lg$col,NA)
#   	lg$fill = c(NA,NA,NA,NA,color.epid)
#   } else {
#   	lg$legend = c(lg$legend, 'Training period')
#   	lg$col = c(lg$col,"lightgrey")
#   }
#   
#   legend("top", 
#   	   legend = lg$legend,
#   	   fill = lg$fill,
#   	   col=lg$col,
#   	   lty = c(1, 1, 1, 2, 1),
#   	   lwd = c(2, 2, 2, 2, 2),
#   	   border=NA,
#   	   horiz=T,
#   	   cex=0.9,
#   	   bty="n"
#   )
#   
#   box()
# dev.off()
# 
# ###
# #  Metrics
# ###
# if(dataset.params$alert) {
#   serie$alert = serie$y > serie$upper
#   save(serie, epidemic.time, dataset.params, epidemics, file="validation.rda")
#   metrics = calc.metrics(serie$y, serie$upper, serie$alert.ref)
# } else {
#   metrics = list()
# }
# 
# struct.result$metrics = metrics
# 
# # Text file for coefficients and R-squared
# struct.result$coef = list()
# 
# coeff = summary(model)$coefficients
# covar = rownames(coeff)
# covar[covar=="(Intercept)"] <- '_Intercept';
# rownames(coeff)<- covar
# colnames(coeff)<- c('estimate','sd','tvalue','pvalue')
# i = coeff[,"pvalue"] < 0.001
# coeff = format.float(coeff, digits=3)
# coeff[i,"pvalue"] = "< .001"
# 
# #coeff$pvalue[ coeff$pvalue < 0.001 ] = '< .001'
# for(v in covar) {
#  struct.result$coef[[v]] = as.list(coeff[v,])
# }
# struct.result$r_squared = round(summary(model)$r.squared, 5)
# struct.result$adj_r_squared = round(summary(model)$adj.r.squared, 5)
# 
# #write(export.json(struct.result),file="regression-model.json")


