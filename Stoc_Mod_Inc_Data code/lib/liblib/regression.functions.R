################################################################################
# $Id: regression.functions.R 459 2012-05-31 18:03:30Z cturbelin $
# Periodic Regression created by Camille Pelat - Reseau Sentinelles - UMR-S 707 INSERM-UPMC France             
# http://www.u707.jussieu.fr/periodic_regression
# Pelat, C., P. Y. Boelle, et al. (2007). "Online detection and quantification of epidemics".BMC Med Inform Decis Mak, 7: 29.
# This script is the property of INSERM. The use of this script and publication 
# from its result must contains an approriate citation (using one of the line 
# above or contacting authors to get one). Any modification or partial reuse 
# of these scripts must contain this header.                           
# @author Camille Pelat <pelat@u707.jussieu.fr>
# @author Clement Turbelin <turbelin@u707.jussieu.fr>
################################################################################

###############################################
# Contains functions used by the model fitting #
###############################################

# evaluate model parameters with a dataset
# formula can be : the model formula or a model id (entry in models.formula)
# if it is a name, we add model name in the model's parameters struct (for debugging purpose)
evaluate_model=function(formula, data = model.data, use.model.name=F) {
  if(use.model.name) {
    model_id <- formula
    formula <- models.formula[[formula]]
  }
  model <- lm(as.formula(formula), data=data)
  if(use.model.name) {
    # mark model with its id to trace it
    model$periodic_model_id <- model_id
  }
  return(model)
}

# create a dataset with models' covariates
# params
# x : index of each observation
# y : observation value
# subset : logical vector. TRUE are obs to keep
# nbtemps : basic period for harmonics
create_model_dataset <- function(x, y, min_temp, arma, subset=NULL, nb_temps) {#<-----------------
  if( !is.null(subset) ) {
    x = x[ subset ]
    y = y[ subset ]
    min_temp=min_temp[ subset ] #<-----------------
    
    arma=arma[subset]

   
  }
  serie <- data.frame(
    x= x,
    y= y,
    min_temp=min_temp, #<-----------------
    arma=arma,
    x2= x**2,
    x3= x**3,
    x4= x**4,#<-----------------
    c1= cos(2*pi*x/nb_temps),
    s1= sin(2*pi*x/nb_temps),
    c2= cos(4*pi*x/nb_temps), 
    s2= sin(4*pi*x/nb_temps),  
    c3= cos(8*pi*x/nb_temps), 
    s3= sin(8*pi*x/nb_temps)
  )
  return(serie)
}


# Compares model AA to model AB in which it is nested and model BA in which it is also nested
# using ANOVA comparisons (significance level 0.5).
# If both AB and BA are "better" than AA, AIC is used to select between them.
# The function returns the final model
# params:
#  model.names : names of the model to test
#  models : models list
#  models.eval : list of anova and AIC of the models
#  alpha : alpha risk value
compare.models <- function(model.names, models, models.eval,  alpha=0.05, verbose=T) {
  modelAA <- model.names[1]
  modelAB <- model.names[2]
  modelBA <- NULL
  
  # return an anova result (alpha value) for a set of model
  get_models_anova=function(m1, m2, model.anova) {
    m = paste(m1,m2,sep='.')
   return(model.anova[[m]])
  }

  if( length(model.names) == 3 ) {
    modelBA <- model.names[3]
  }
  # first comparison AA vs AB
  anovaAB <- get_models_anova( modelAA, modelAB, models.eval$anova)
  
  if (is.na(anovaAB)){
    setwd("~/Dropbox/THIS IS IT/lib")
    go('runer')
  }
  
  if(verbose) {
	   cat("anova(",modelAA," ",modelAB,")=",anovaAB)
  }
  

  if( is.null(modelBA) ) {
	 if( anovaAB > alpha) {
      num_model <-1 
      cas = 11
   } else {
      num_model <- 2 
      cas = 12
   }
  } else {
    # If there are 3 parameters
		anovaBA <- get_models_anova(modelAA, modelBA, models.eval$anova); #anovaBA
		
		if (is.na(anovaBA)){
		  setwd("~/Dropbox/THIS IS IT/lib")
		  go('runer')
		}
		
    if(verbose) {
  	   cat(" anova(",modelAA," ",modelBA,")=",anovaBA)
    }
		
    # if neither of the 2 models is significative ####
		if( anovaAB > alpha & anovaBA > alpha ) {
      num_model <- 1 
      cas = 1
    }
    # if model AB is significative but not BA 
		if( anovaAB <= alpha & anovaBA > alpha ) {
        num_model <- 2 
        cas = 2
    }
    # if model BA is significative but not AB ####
		if( anovaAB > alpha & anovaBA <= alpha ) {
      num_model <- 3
      cas = 3
    }
    #### if both modelq AB and BA are significative ####
		if( anovaAB <= alpha & anovaBA <= alpha )
		{
      AIC_BA <- models.eval$AIC[[ modelBA ]]
		  AIC_AB <- models.eval$AIC[[ modelAB ]]
      if( verbose ) {
        cat(" NS => AIC(",modelBA,")=",AIC_BA," AIC(",modelAB,")")
      }
      if (  AIC_BA < AIC_AB ) {
        num_model <- 3 
        cas <- 4
      } else {
        num_model <- 2 
        cas <- 5
      }
		}
	} 
  if(verbose) {
    cat(" num_model=",num_model,"\n")
  }
  return(list(
    num_model=num_model,
    cas=cas
    )
  )
}

# Generic model selection method using a given model hierarchy
# models = list of models to test (list names = model name)
# hierarchy : hierarchy list of model to test (hierarchical list of model names)
# models.eval : set of pre-calculated anova and AIC for models
# verbose : if true print debug information
select_models <- function(models, hierarchy, models.eval, verbose = T) {

   # Evaluated models
   traversed = c()

   # Recursive function walking on the hierarchy of model rules
   test_model_hierarchy=function( model.name ) {
      traversed <<- c(traversed,model.name)
      sub.models = hierarchy[[ model.name ]]
      if( is.null(sub.models) ) {
        return(model.name)    
      }
      m = c(model.name,sub.models)
      if(verbose) {
        cat("Model ",model.name," test of models : ",paste(m, collapse=','),"\n")
      }
  
      r = compare.models( m, models, models.eval )

      if (is.na(r$cas)){
        setwd("~/Dropbox/THIS IS IT/lib")
        go('runer')
      }
      
      if( verbose ) {
        cat("Selected :",r$num_model," cas : ",r$cas,"\n")
      }
      if( r$num_model == 1) {
        # Return the first model name if selected
        return(model.name)
      } else {
        # If its the 2nd ou 3rd : call model test
        return( test_model_hierarchy( m[ r$num_model ] ))
      }
    }
 # first model to evaluate
 init = names(hierarchy)[1]
 final = test_model_hierarchy(init)
 return(list(final=final, traversed=traversed))
}            

# create list of anova test for a pair of model (given as name)
# models : models list
# model.pairs = list of pair (m1,m2) of model names
calc_models_anova <- function(models, model.pairs) {
 rr = list()
 lapply(model.pairs, function(m) {
   m1 = m[1] ; m2 = m[2]
   a = anova(models[[m1]], models[[m2]])
   rr[paste(m1,m2,sep='.')] <<- a[2,'Pr(>F)']
 })
  return(rr)
}



#
# Retrospective Model selection 
#
model_select_retrospective <- function(model.data, do.graph = T) {

 # get all models names
 model.names <- names(models.formula)

 # get all models
 # estimate models (use sapply to get model names as names attributes in models list)
 models <- sapply(model.names, evaluate_model, simplify=F,USE.NAMES=T, data=model.data, use.model.name=T)

 # models.eval : structure that contains all evaluation of model (anova test for each model pair and AIC for each model)
 # we pre-calculate these vakues because we need them to draw graph
 models.eval <- list()
 
 # hard coded way because we need this order for graph
 models.eval$anova <- calc_models_anova(models, list(
  c('M11','M12'),
  c('M11','M21'),
  c('M12','M13'),
  c('M12','M22'),
  c('M21','M22'),
  c('M21','M31'),
  c('M13','M23'),
  c('M22','M23'),
  c('M22','M32'),
  c('M31','M32'),
  c('M31','M41'),
  c('M23','M33'),
  c('M32','M33'),
  c('M32','M42'),
  c('M41','M42'),
  c('M33','M43'),
  c('M42','M43')
 ))
 

 for (k in 1:length(models.eval$anova)){
   chk<-models.eval$anova [[k]]
   if (is.na(chk)){
     setwd("~/Dropbox/THIS IS IT/lib")
     go('runer')
   }
 }

 models.eval$AIC <- lapply(models, AIC)
 # Model Decision rule hierarchy
  model.hierarchy.retro = list(
    "M11"=c("M12","M21"),
    "M12"=c("M13","M22"),
    "M21"=c("M22","M31"),
    "M13"=c("M23"),
    "M22"=c("M23","M32"),
    "M31"=c("M32","M41"),
    "M23"=c("M33"),
    "M32"=c("M33","M42"),
    "M41"=c("M42"),
    "M33"=c("M43"),
    "M42"=c("M43")
  )
 # run model selection algorithm using this hierarchy
 r <- select_models(models, model.hierarchy.retro, models.eval)
 # create graph
 # if(do.graph) {
 #   model_decision_graph_retrospective( r$traversed, r$final, models.eval )
 # }
 return( list(final=r$final, model=models[[r$final]],traversed=r$traversed))
}
  
# # Graphic decision for restrospective way
# # too specific to write a generic method
# model_decision_graph_retrospective <- function(traversed, final, models.eval) {
#   # get anova results 
#   a <- unlist(models.eval$anova)
#   label_anova <- formatC(a, digits=3, format='f') #digits for pvalue
#   aa = a < 0.001
#   label_anova[ aa ] <- 'p<0.001'
#   label_anova[ !aa ]<- paste('p = ',label_anova[ !aa ], sep='')#<----- "p =" 
# 
#   # order of models in the graph
#   mgraph <- paste('M', c(11, 12, 21, 13, 22, 31, 23, 32, 41, 33, 42, 43), sep='')
# 
#   model_AIC <- unlist(models.eval$AIC[ mgraph ])
#   
#   label_model<-paste( mgraph ,' (AIC=',formatC(model_AIC, digits=2, format='f'),')',sep='')
# 
#   # colouring the models in the selection pathway
#   coul = ifelse( mgraph == final, "red", ifelse( mgraph %in% traversed, "darkorange2", 1))
#   font = ifelse( mgraph == final,    4, ifelse( mgraph %in% traversed, 3, 1) )
# 
#   # Draws an arrow between point (x0,y0) and point (x1,y1)
#   fleche <- function (x0, y0, x1, y1 ,...){
#   		aa = 0.25
#   		if ((x1 - x0) > 0 ) { 
#         a = -aa 
#        } else { 
#         a = aa 
#       }
#   		arrows(x0, y0, x1 + a, y1 + aa, length=0.3,...)
#   } 
# 
#   #### plot ####
#   png(file='regression_decision_anova.png', width=770, height=650) #width=370,height=250
#   par( mar=c(0, 0, 0, 0), cex=graph.params$cex)
# 
#   # position of the model names<----------
#   u = c(0, -0.9, 0.9, -2, 0, 2, -1, 1, 2.8, 0, 2, 1) # - left + right
#   v = c(2.1, 1.1, 1.1, 0.1, 0.1, 0.1, -0.9, -0.9, -0.9, -1.9, -1.9 ,-2.9) # - down + up
#   plot( u, v, type='n', ylim=c(-3, 2.5), xlim=c(-2.6, 3.4), axes=F, ylab='', xlab='') #center the graph
#   
#   # draw arrows <------------
#   fleche(0, 2, 1, 1); fleche(0, 2, -1, 1)  
#   fleche(-1, 1, -2, 0); fleche(-1, 1, 0, 0)
#   fleche(1, 1, 2, 0); fleche(1, 1, 0, 0)
#   fleche(0, 0, -1, -1); fleche(0, 0, 1, -1)
#   fleche(-2,0,-1,-1);fleche(2, 0, 1, -1)
#   fleche(2, 0, 3, -1);fleche( -1, -1, 0, -2)
#   fleche(1, -1, 0, -2);fleche( 1, -1, 2, -2) 
#   fleche(2.9, -1, 2, -2);fleche(0, -2, 1, -3)
#   fleche(2, -2, 1, -3)
#   #fleche(a, b, c, d)
#   #a= (tail: left(-), right(+))
#   #b= (tail: up(+), down(-))
#   #c= (head: left(-), right(+))
#   #d= (head: up(+), down(-))
#  
#   # position of the p-values on the graphic, along the arrows
#   posx <- c(-0.5, 0.5, -1.5, -0.5, 0.5, 1.5, -1.5, -0.5, 0.5, 1.5, 2.5, -0.5, 0.5 , 1.5, 2.5, 0.5 ,1.5)
#   posy <- c(1.5,1.5,0.5,0.5,0.5,0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-1.5, -1.5, -1.5, -1.5, -2.5,-2.5)
#   text(posx, posy, labels=label_anova, pos=2, cex=(graph.params$cex.text - 0.2))
#   #I understand that it works because when i ask for more pvalues than i need (in this case 18 and i need 17) 
#   #the values of pvalues start to recycle from the beggining.
#   
#   text(u, v, labels=label_model, col=coul, font=font, adj=c(0.5, 0) )
#   dev.off()
# } 


# Selection Algorithm for the prospective setting  
# compares the 3 models input as parameters 
# model_select_prospective <- function(model.data, do.graph=T){
#   # models formula to use
#   model.names <- c('M11','M12','M13')
#   
#   # estimate models (use sapply to get model names as names attributes in models list)
#   models <- sapply( model.names, evaluate_model,simplify=F,USE.NAMES=T, data=model.data, use.model.name=T )
#   # create dataset of evaluation (anova, AIC
#   models.eval <- list()
#   models.eval$anova <- calc_models_anova(models, list( c('M11','M12'), c('M12','M13') ))
#   models.eval$AIC <- lapply(models,AIC)
# 
#   # model decision rule hierarchy
  # hierarchy <- list(
  #   'M11'=c('M12'),
  #   'M12'=c('M13')
  # )
#   
#   selected <- select_models(models, hierarchy, models.eval)
#   
#   if(do.graph) {
#     model_decision_graph_prospective(selected$final, selected$traversed, models, models.eval)
#   }
#   
#   return(
#     list(final=selected$final, traversed=selected$traversed, model=models[[ selected$final ]])
#   )
# }

# model_decision_graph_prospective <- function(final, traversed, models, models.eval)  {
#   model.names = names(models)
#   # position of the 3 models names
#   u = c(0, 0, 0)
#   v = c(1, 0, -1)
#   model_AIC <- unlist(models.eval$AIC)
#   label_model<-paste(model.names ,' (AIC=',formatC(model_AIC,digits=2,format='f'),')',sep='')
#   
#   # position of the 2 p-values
#   posx = c(0, 0) + 1
#   posy = c(0, -1)+ 0.5
#   
#   a = unlist(models.eval$anova)
#   aa = a < 0.001
#   label_anova = aa
#   label_anova[ aa ]<- 'p<0.001'
#   label_anova[ !aa ]<- paste('p=',formatC(a[!aa], digits=3, format='f'), sep='')
# 
#   # Draws another type of arrow between point (x0,y0) and point (x1,y1)
#   fleche2<-function (x0,y0,x1,y1,...){
#   		a=0.2
#    		arrows(x0 , y0 - a, x1, y1 + a, length=0.1, ...)
#   } 
# 
#   # colouring the models in the selection pathway
#   coul = ifelse( model.names == final,"red", ifelse( model.names %in% traversed, "darkorange2",1))
#   font = ifelse( model.names == final,    4, ifelse( model.names %in% traversed, 3, 1) )
# 
#   #### plot ####
#   png(file='regression_decision_anova.png',width=300,height=350)
#   par(mar=c(2,2,2,2), cex=graph.params$cex)
#   plot(u,v, ylim=c(-1,1.5), xlim=c(-1,1), type='n', axes=F, ylab='', xlab='')
#   fleche2(0,1,0,0)
#   fleche2(0,0,0,-1)
#   text(posx, posy, labels=label_anova, pos=2, cex=graph.params$cex.text)
#   text(u, v, labels=label_model, col=coul, font=font,adj=c(0.5,0))
#   dev.off()
# }
# 
# # for debugging purpose
# plot.epid=function(epidemics, cur.epid) {
#   e = epidemics[cur.epid,]
#   epid = e$start:e$end
#   ee = (e$start-10):(e$end+10)
#   ss = serie[ee ,]
# 
#   plot(range(ss$x),range(ss$y),type="n")
#   y = min(ss$y,na.rm=T)
#   ym = max(ss$y)
#   rect( e$start, y, e$end, ym, col="lightgrey",border=NA)
#   points(ss$x,ss$y,type="o")
#   points(ss$x, ss$upper,col="red",type="l")
# 
# 
#   ll = which(serie$y[epid] < serie$upper[epid])
#   if(length(ll) > 0) {
#    ll = epid[ll]
#    segments(ll, y - 2, ll, y + 2,col="red")
#   }
# }


