setwd("~/Dropbox/SOS/THIS IS IT")

lib.path <- function(file, ...) {
  return(paste('lib/liblib/', file, sep=''))
}

source(lib.path("simulations.changepoint.R"))
source(lib.path("base.function.R"))
source(lib.path("regression.functions.R"))

# General graphic parameters
graph.params = list(
  cex.main 	= 1, 
  cex.axis 	= 0.9, 
  cex.lab 	= 0.7, 
  cex 		= 0.9, 
  cex.text 	= 0.9)

op = par(no.readonly=T)

par(graph.params[ names(graph.params) %in% names(op)])

### General Parameters for the model

dataset.params = list(
  file= "1503553691-599e689b9f9d7",	#path of the datafile
  training= 105,
  alert=F,
  date_start	= "2014-09-29",	#first date (starting date) of the dataset
  time_step	= "week", 		#time step of the dataset
  units		= "",			#graphic option for plot
  title	= ""	#graphic option for plot
)

nb_app     	= 105  		#length of the training period 

choix_seuil = "percentile"	#purging method chosen

pruning.value  = 15

file_epid="" #name of the epidemic marker file"

running_id	= "1503555288" # Identifier of the current run of the script. Each run should have a different id for the same user

# model parameters
setting		= "retrospective"

model_choice = "select"

confidence.limit = 95 

# export.format = "csv"
# 
# epidemic.time = 2
# 
# color.serie = "black"
# 
# color.model = "green"
# 
# color.Estimated_valuesserf="red"
# #3
# 
# color.epid = "pink"
#FFC9C9
# source(lib.path("glmm.r.squared.R"))#<-------------
#source(lib.path('model.R'))
# source(lib.path("mse.R"))
# 
# mse_kall<-sum((serie$y-serie$Estimated_valueskall)^2)/length(y)
# mse_serf<-sum((serie$y-serie$Estimated_valuesserf)^2)/length(y)
# mse_parp<-sum((serie$y-serie$Estimated_valuesparp)^2)/length(y)
# 
# m_kall[j,]<-mse_kall[1]
# m_parp[j,]<-mse_parp[1]
# m_serf[j,]<-mse_serf[1]
# selected.models[j,]<-as.character(save.selected.model$periodic_model_id)
# 
# AIC<-AIC(model11,model12,model13,model21,model22,model23,model31,model32,model33,model41,model42,model43)
# BIC<-BIC(model11,model12,model13,model21,model22,model23,model31,model32,model33,model41,model42,model43)
# AICc<-AICc(model11,model12,model13,model21,model22,model23,model31,model32,model33,model41,model42,model43)
# 
# AIC_1[,lala]<-AIC$AIC
# BIC_1[,lala]<-BIC$BIC
# AICc_1[,lala]<-AICc$AICc

