 ################################################################################
# $Id: base.function.R 425 2011-12-02 19:13:07Z cturbelin $
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

cleanup.files <- function(pattern) {
  f = c(Sys.glob(pattern))
  ff = file.exists(f)
  f = f[ff]
  file.remove(f)
}

# Read dataset contents. Datasource is defined in params list
# params entries:
#  file = file path
#  alert = TRUE if the file contains a second column (alert gold standard)
#  training = size of the training period (from the end of the serie)
# use.training = if TRUE generate training column.
read.dataset <- function(params, use.training=T) {
   if(is.null(params$alert)) {
    params$alert <- F
   }
   cols = c("y")
   if( params$alert ) {
      cols = c(cols, "alert.ref")
   }
   serie <- read.table(file=params$file, header=FALSE, col.names=cols, sep='', blank.lines.skip =F,  na.strings ="")  #time series as a vector : y
   if( is.factor(serie$y) ) {
      serie$y = serie$y #<------------------------------------
   }
   nn = nrow(serie)
   cat("reading ", nn, " lines\n")
   if(use.training) {
     # Training for periodic regression
     serie$training = F
     if(!is.null(params$training) && params$training > 0) {
       tt = ( nn - params$training + 1):nn
       #tt = 1:(params$training-1)
       serie$training[tt] = T
     }
   }
   # index serie
   gri=read_excel("data.xlsx",col_names = T)#<-----------------
   gri<-gri$x
   n<-nrow(gri)
   # serie$y<-unlist(gri)
   serie$x =  1:length(serie$y)
   y_=mean(gri)
   
   descriptives_temps_by_week=read_excel("descriptives_temps_by_week.xlsx")#<-----------------
   difs=read_excel("difs.xlsx")#<-----------------
   serie$min_temp= descriptives_temps_by_week$min #<-----------------
   serie$arma=difs$x
   # cache file if needed
   if( !is.null(params$cache.name) && params$cache.name != "") {
      save(serie, file=params$cache.name)
   }
   return(serie)
}


# export data structure in JSON format (used for web site)
export.json <- function(r) {
  if( is.list(r) ) {
    nn = names(r)
    v =c()
    for(n in nn) {
      value  = export.json(r[[n]])
      v = c(v, paste("\"",n,"\"",":",value,sep=''))
    }
    return(paste('{', paste(v , collapse=","), '}', sep='') ) 
  } 
  if( is.factor(r) ) {
    r = as.character(r)
  }
  if(is.logical(r)) {
    r = tolower(paste(r))
    r[ r == "na" ] = 'null'
  }
  if( is.character(r) ) {
      sep = '","' ; quote = '"'
  } else {
     sep = ',' ; quote = ''
     r = as.character(r)
     r[is.na(r)] = '"NA"'
  }
  if(length(r) == 1) {
    return( paste(quote,r[1],quote,sep='')) 
  } else {
   return( paste('[',quote, paste(r , collapse=sep),quote, ']', sep='') ) 
  }
}

# Format float values
format.float <- function(float, digits=as.numeric(options("digits"))){
  formatC(float,format="f", digits=digits, flag="0")
}

# format integer values
format.int <- function(int){
 formatC(int, format="f", digits=0, width=2, flag="0")
}

# calculate time for serie from index and time type
# for "week" time type, return.monday is used to get week number (ISO8601) and date of monday
calc_time_date <- function(index, time.type, first.date, return.monday = F) {
   d = as.Date(first.date)
   if(time.type == "day") {
      dates = d + index
   } 
   if( time.type == "month") {
     y = as.integer(format(d,"%Y"))
     m = as.integer(format(d,"%m"))
     # index of month
     i = seq(from=m-1, by=1, length.out=length(index))
     im = (i %% 12) + 1
     iy = floor(i / 12) + y
     dates = iy*100 + im
   }
   if(time.type == "week") {
      w = as.integer(format(d,"%w"))
      w = ifelse(w == 0, 7, w) # 1=monday
      d = d - (w - 1) # align date to the monday of the same week
      dates  = d + (index-1)*7
      ww = as.integer(  ISOYearWeek(dates))  # compute week number
      if(return.monday) {
        dates = list(dates=ww, week.start=dates)
      } else {
        dates = ww
      }
   }
   if(time.type == "quarter") {
      dates = d + index
      y = as.integer(format(d,"%Y"))
      m = round(as.integer(format(d,"%m")) / 4) + 1
      y = y*10 + m
      dates = list(dates=y, day.of.year=as.integer(format(d,"%j")))
   }
   return(dates)
}

calc_time_ticks <- function(dates, time.type) {
  if( time.type == "day" ) {
    m = as.integer(format(dates,format="%m%d"))
    m <- m == 101
  } else {
   m <- (as.integer(dates) %% 100) == 1
  }
  return( which(m) )
}

# export files in fixed format (@todo avoid using this format, csv is better )
export.fixed <- function(x, widths, digits=3,file) {
 nn = names(x)
 for(n in nn) {
  w = widths[[n]]
  x[, n] <- format(x[,n] , digits = digits, width=w)
 }
 write.table(x, file=file, quote=F, row.names=F, sep="\t")
}

# Export a data.frame in text file
# handle renaming of fields (using paired list)
export.data <- function(x, file, format, fixed=NULL, rename=NULL, digits=3) {
 if( !is.null(rename) ) {
    n = names(x)
    f = match(n, names(rename))
    n[!is.na(f)] = rename[ f[!is.na(f)] ]
    names(x)<-c(n)
 }
 nn = names(x)
 for(n in nn) {
  if(is.integer(x[,n])) next()
  if( is.double(x[,n]) && (class(x[,n]) == "numeric") ) {
    x[, n] <- formatC(x[,n] , digits = digits, format="fg")
  }
 }
 if(format=="fixed") {
     file = paste(file,"txt",sep=".")
     export.fixed(x, file=file, digits=3, widths=fixed)
 }
 if(format %in% c("csv", "csv2")) {
   file = paste(file, "csv", sep=".")
   if(format == "csv") {
     write.csv(x, file, quote=F, row.names=F)
   } else {
     write.csv2(x, file, quote=F, row.names=F)
   }
 }
}

# cturbelin, d?cembre 2006
# ISO 8601 week number calculation
# weeksbydates(from,to)
# from = Date  YYYY-MM-DD or timestamp
# to = date au format ISO YYYY-MM-DD  ou un timestamp (Sys.time())
# @return dataframe(dates,semaine au format YYYYWW)

if (.Platform$OS.type == "unix") {
  ISOYearWeek <- function(dates) {
   return(format(dates,"%G%V"))
  }
	weeksbydates <- function(from, to) {
		dates = seq(from=as.Date(from),to=as.Date(to),by=1)
		weeks = format(dates,"%G%V")
		return(data.frame("date"=dates,"yw"=weeks))
	}
} else {
  # Week number using ISO 8601 method
	ISOYearWeek <- function(dates) {
		j = as.numeric(format(dates,"%w"))
		j = ifelse(j == 0, 7,j) - 4 # Day of the week (center on thursday)
		d = dates - j # Date of the thursday
		jan.4 = as.Date(paste(format(d,"%Y"), "-01-04", sep=""))
		wd = as.numeric(format(jan.4, "%w")) # what id the day of week of the 4th of Jan.?
		wd = ifelse(wd == 0, 7, wd) # dow monday to sunday
		lundi = jan.4 - (wd - 1) # monday of the week that includes the Jan 4th
		dif = ceiling(as.numeric(d - lundi) / 7) # Number of weeks to this monday
		yw = as.numeric(format(d, "%Y")) * 100 + dif
		return(yw)
	}

	weeksbydates <- function(from, to) {
	 	dates = seq(from=as.Date(from),to=as.Date(to),by=1)
		weeks = ISOYearWeek(dates)
		return(data.frame("date"=dates,"yw"=weeks))
	}
}

# Search epidemic periods (start and end date)
# @param epid logical vector containing TRUE if the timestep has an epidemic alert
# @param epid.time integer, number of consectutive time step needed to declare the epidemic has started
search.epidemic.period <- function(epid, epidemic.time) {
  epidemics = data.frame() # list of epidemices periods
  cur.epid = 0 # current index of epidemics data in data.frame
  is.epid = F # flag (is current state in epidemic period)
  invisible(sapply( 1:length(epid), function(i) {
     if(i < epidemic.time + 1) {
      return()
     }
     # get the vector of n previous obs
     e.start = i - epidemic.time + 1
     e = epid[ e.start:i]
     if( length(e) == 0) {
      return()
     }
     if( any(is.na(e)) | is.na(epid[i]) ) {
      return()
     }
     if( all( e == epid[i] ) ) { # all previous obs are the same for the given length
       # detect change of status
       if( is.epid != epid[i] ) {
          if( epid[i] ) {
            # Start of a new epidemic period
            cur.epid <<- cur.epid + 1
            epidemics <<- rbind(epidemics, data.frame(start=e.start, end=length(epid)))
            # end time is length of epid (max index), because if an epidemic start in last index, it s not ended
            is.epid <<- T
          } else {
            epidemics$end[ cur.epid ] <<- e.start - 1 # last upper is before end of epidemic start
            is.epid <<- F
          }
       }
     }
   }))
   return(epidemics)
}

calc.metrics <- function(y, threshold, reference) {
 metrics = list()
 alert = y > threshold
 size = length(y[ !is.na(y) ])
 if( !is.null(reference) ) {
   metrics$sensitivity = round(sum(alert & reference, na.rm=T) / size, 3)
   metrics$specificity = round(sum(!reference & !reference, na.rm=T)  / size, 3)
 }
 return(metrics)
}

library(Cairo)
graph.open=function(file, width=480, height=480) {
  CairoPNG( file=file, width=width, height=height)
}
