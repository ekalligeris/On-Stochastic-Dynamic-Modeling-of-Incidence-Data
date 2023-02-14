################################################################################
# $Id: functions.R 113 2010-05-06 10:19:44Z cturbelin $
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

.prune.compatibility = T

# Prune data set values
#
prune.dataset = function(serie, pruning.method, pruning.value) {
  if( pruning.method %in% c('percentile','value') ) {
    cat('pruning ', pruning.method,"\n")
    if(pruning.method =='percentile') {
      training = serie$y[ serie$training ]
      training = training[!is.na(training)]
      if(.prune.compatibility) {
       # In old version comparaison quantile is not as simple as it could be
       # estimated quantile is sometimes with value after 14th decimal (and zero before)
       # wich is not the same result when the required quantile is computed alone
       # value are close (to the 14th decimal) but comparison does not give the same result when observed
       # value is equal to the threshold
       # for compatibility
        probs = seq(0, 1 - pruning.value / 100, by=.05)
        seuil <- quantile(training, probs=probs, na.rm=T)[ length(probs) ]
        str(seuil)
        str(training)
        if( (seuil - as.integer(seuil) < 1e-6) ) {
          cat("Quantile compatibility problem\n")
        }
      } else {
        seuil <- quantile(training,probs=(1-pruning.value/100),na.rm=T)
        seuil = signif(seuil, 6)
      }
      keep = serie$y < seuil
    } else { 
      seuil <- pruning.value
      keep = serie$y < seuil
    }
    cat(' threshold ', seuil,"\n")
    pruning.threshold <<- seuil
  } else {
      # Choice == file
      if( !file.exists(file_epid) ) {
        cat(' pruning file doesnt exists ')
      }
      epid <- read.csv(file=file_epid, header=FALSE, col.names='e')
    	epid = epid$e
      epid = as.integer(epid) # get integer value
      
      # Pre-treatment if the length of epid_period is not nb_app
    	nn = sum(serie$training)
      # create an empty vector of the training period size
      seuil = epid[1:nn] == 0# complete or reduce epidemic period to training size
      keep = serie$training # vector of training period (TRUE if training)
      keep[ serie$training ] = seuil # could be keep only in training period
  }
  keep = keep & serie$training
  return(keep)
}
