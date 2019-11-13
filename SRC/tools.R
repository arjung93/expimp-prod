library(vars)
library(zoo)
library(tsdb)
library(xtable)
library(strucchange)
library(quantmod)
library(ggplot2)
library(reshape)
library(reshape)
library(tis)

##==== A very general function that is used for almost all the graphs in
## macro-monitoring
##=== Arguments :
## 1) output: data needs to be plotted, could be a single series or an
## object with multiple columns

## 2) ylab stands for label on Y-axis

## 3) cond.Vab: condition for vertical abline if TRUE then function
## will plot abline at "v" (argument in function)

## 4) extension: whether pdf or png

## 5) leg.pos : Postion of legend (will work only if legend is not
## equals to NULL)

## 6) col: colour of lines and legend

## 7) legend : legend needed on plot

## 8) h: horizontol abline (by default value is zero)

## 9) type : whether line graph is needed or bar graph (by default
## line)

## 10) shade : shade=TRUE will shade the graph for nber dates (works
## only for annual and quarterly data)

## 11) bands stands for customised abline band=1 will plot abline at
## median and band=3 will plot quantiles

makegraph <- function(output, ylab, filename, cond.Vab=FALSE, v, extension="pdf", leg.pos="topleft", col=c("black","blue"), legend=NULL, h=0 , type="line", shade=FALSE, bands=0){
  if(extension=="pdf"){
    pdf(paste (filename, ".pdf", sep=""), width=4.9, height=2.9)
  } else {
    png(paste(filename, ".png", sep=""), width=673, height=399)
  }
  par(mai = c(.4, .8, .3, .2))
  if(type=="line") {
    if(frequency(output)==4 && shade==TRUE){
      output <- na.omit(output)
      tmp <- ts(output, freq=frequency(output), start=as.yearqtr(head(index(output), 1)))
      plot.zoo(tmp, type='n', ylab="", xlab="", xaxt="n", yaxt="n", screen="multiple")
      nberShade(col="grey")
      par(new=TRUE)
      plot.zoo(tmp, lwd=2, xlab="", type="l", ylab=ylab, cex.axis=0.55, col= col, screen="multiple")
      if(ncol(tmp)>1){
        timestamp.qrtr(tmp[,1])
      } else {
        timestamp.qrtr(tmp)
      }
    } else if (diff(as.numeric(index(output)))[1] > 200 && shade==TRUE) {
      output <- na.omit(output)
      output <- ts(na.omit(output), freq=frequency(output), start=as.numeric(substr(start(output), 1, 4)))
      plot.zoo(output, type='n', ylab=ylab, screen="multiple", xaxt="n", xlab="")
      nberShade(col="grey")
      if(is.null(ncol(output))==FALSE && ncol(output)>1){
        par(new=TRUE)
        plot.zoo(output, lwd=2, xlab="", ylab="", screen="multiple", col=col, cex.axis=0.55)
        dates <- attr(output, "index")
        output <- zoo(output, order.by=dates)
        timestamp.month(output[,1])
      } else {
        lines(output, lwd=2, xlab="", col=col, cex.axis=0.55)
        dates <- attr(output,"index")
        output <- zoo(output, order.by=dates)
        mtext(paste(paste(tail(na.omit(as.yearmon(index(output))), 1)), round(output[length(output)], 2), sep="; "), 3, at=tail(as.yearmon(index(output)), 1), cex=0.6, adj=1, col="blue")
      }
    } else {
      if (is.null(ncol(output))==FALSE && ncol(output) > 1){
        if(diff(as.numeric(index(output)))[1] == 0.25){
          plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab, cex.axis=0.55, screen="multiple", col = col, lty=rep(1, ncol(output)), xaxt="n",xlim=c(index(output)[1],as.yearqtr(Sys.Date())))
          axis(1, at= as.yearqtr(seq.Date(as.Date(index(output))[1],
                    Sys.Date(), length.out = 6)),
               labels=as.yearqtr(seq.Date(as.Date(index(output))[1],
                 Sys.Date(), length.out = 6)),cex.axis=0.5)
          timestamp.qrtr(output[,1])
        } else if (diff(as.numeric(index(output)))[1] > 300){
          plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab, cex.axis=0.55, screen="multiple", col = col, lty=rep(1, ncol(output)), xaxt="n",xlim=c(index(output)[1],Sys.Date()))
          axis(1, at=seq.Date(as.Date(index(output))[1],
                    Sys.Date(), length.out = 6),
               labels=substr(seq.Date(as.Date(index(output))[1],
                 Sys.Date(), length.out = 6),1,4),cex.axis=0.5)
          timestamp.graph.wv(output[,1])
        } else {
          if(class(index(output))=="Date"){
            plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab, cex.axis=0.55, screen="multiple", col = col, lty=rep(1, ncol(output)), xaxt="n", xlim=c(index(output)[1], Sys.Date()))
            axis(1, at=seq.Date(as.Date(index(output))[1], Sys.Date(),
                       length.out = 6),
                 labels=as.yearmon(seq.Date(as.Date(index(output))[1],
                   Sys.Date(), length.out = 6)), cex.axis=0.5)
          } else{ plot.zoo(output, lwd=2, xlab="", type="l",
                           ylab=ylab, cex.axis=0.55, screen="multiple", col = col,
                           lty=rep(1, ncol(output)), xaxt="n", xlim=c(index(output)[1],
                                                                 as.yearmon(Sys.Date())))
                  axis(1, at=as.yearmon(seq.Date(as.Date(index(output))[1], Sys.Date(),
                             length.out = 6)),
                       labels=as.yearmon(seq.Date(as.Date(index(output))[1],
                         Sys.Date(),  length.out = 6)), cex.axis=0.5)
                }
          ## plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab,
          ## cex.axis=0.55, screen="multiple", col = col, lty=rep(1,
          ## ncol(output)),
          ## xaxt="n",xlim=c(as.yearmon(index(output))[1],
          ## as.yearmon(Sys.Date()))) axis(1,
          ## at=seq.Date(as.Date(index(output))[1], Sys.Date(),
          ## by="months"),
          ## labels=substr(as.yearmon(seq.Date(as.Date(index(output))[1],
          ## Sys.Date(), by="months")),5,8), cex.axis=0.5)
          timestamp.graph.wv(output[,1])
        }
      } else {
        ## plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab,
        ## cex.axis=0.55, screen="multiple")
        if(diff(as.numeric(index(output)))[1] == 0.25){
          plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab, cex.axis=0.55, screen="multiple", col = col, xaxt="n",xlim=c(index(output)[1],as.yearqtr(Sys.Date())))
          axis(1, at= as.yearqtr(seq.Date(as.Date(index(output))[1],
                    Sys.Date(),  length.out = 6)),
               labels=as.yearqtr(seq.Date(as.Date(index(output))[1],
                 Sys.Date(),  length.out = 6)),cex.axis=0.5)
          	timestamp.qrtr(output, value=TRUE)
        } else if (diff(as.numeric(index(output)))[1] > 300){
          ##index(output)  <- as.yearmon(index(output))
          plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab, cex.axis=0.55, screen="multiple", col = col, xaxt="n", xlim=c(index(output)[1],Sys.Date()))
          axis(1, at=seq.Date(as.Date(index(output))[1],
                    Sys.Date(), length.out = 6),
               labels=substr(seq.Date(as.Date(index(output))[1],
                 Sys.Date(), length.out = 6),1,4),cex.axis=0.5)
          timestamp.month(output)
        } else if ( class(index(output))=="Date" && diff(as.numeric(index(output)))[1] > 7 && diff(as.numeric(index(output)))[1] < 40 ) {
          plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab, cex.axis=0.55, screen="multiple", col = col, xaxt="n", xlim=c(index(output)[1], Sys.Date()))
          axis(1, at=seq.Date(as.Date(index(output))[1], Sys.Date(),
                    length.out = 6),
               labels=as.yearmon(seq.Date(as.Date(index(output))[1],
                 Sys.Date(), length.out = 6)), cex.axis=0.5)
          timestamp.graph(output)
        } else if (class(index(output))=="yearmon" && diff(as.numeric(index(output)))[1] < 1){
          plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab,
                   cex.axis=0.55, screen="multiple", col = col,
                   xaxt="n", xlim=c(index(output)[1],
                               as.yearmon(Sys.Date())))
          axis(1, at=as.yearmon(seq.Date(as.Date(index(output))[1], Sys.Date(),
                   length.out = 6)),
               labels=as.yearmon(seq.Date(as.Date(index(output))[1],
                 Sys.Date(),
                length.out = 6)),
               cex.axis=0.5)
          timestamp.graph(output)
          ## plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab,
          ## cex.axis=0.55, screen="multiple", col = col, xaxt="n",
          ## xlim=c(index(output)[1], Sys.Date())) axis(1,
          ## at=seq.Date(as.Date(index(output))[1], Sys.Date(),
          ## by="months"),
          ## labels=as.yearmon(seq.Date(as.Date(index(output))[1],
          ## Sys.Date(), by="months")), cex.axis=0.5) ##
          ## plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab,
          ## cex.axis=0.55, screen="multiple", col = col,
          ## xaxt="n",xlim=c(as.yearmon(index(output))[1],
          ## as.yearmon(Sys.Date()))) ## axis(1,
          ## at=seq.Date(as.Date(index(output))[1], Sys.Date(), ##
          ## by="months"), ##
          ## labels=substr(as.yearmon(seq.Date(as.Date(index(output))[1],
          ## ## Sys.Date(), by="months")),5,8), cex.axis=0.5)
          ## timestamp.graph(output)
        } else {
          plot.zoo(output, lwd=2, xlab="", type="l", ylab=ylab, cex.axis=0.55, screen="multiple", col = col, xlim=c(index(output)[1], Sys.Date()))
          timestamp.graph(output)
        }
      }
      if(cond.Vab==TRUE){
        abline(v=as.Date(v))
      }else{
        abline(h=h, col="black", lwd=2)
      }
      if (bands==1) {
        abline(h=median(as.numeric(output), na.rm=TRUE),
               lty=2, lwd=2, col="blue")
      }
      if (bands==3) {
        abline(h=quantile(as.numeric(output), prob=c(.25,.5,.75), na.rm=TRUE),
               lty=2, lwd=c(1,2,1), col="blue")
      }
    }
    if(is.null(legend)==FALSE){
      legend(x=leg.pos, bty="n", lty=rep(1,ncol(output)), lwd=rep(2,ncol(output)), cex=0.5, col=col, legend=legend) 
    }
  } else{
    barplot(output , xlab="", ylab=ylab, space=0.1, cex.axis=0.55, las=0.1, cex=0.5, col=col, beside = T)
    abline(h=0, col="black", lwd=2)
    timestamp.qrtr(output)
  }
  dev.off()
}

##====Function to generating pdf or png
##== Arguments :
## 1) filename : filename of pdf or png that needs to be generated

## 2) extension: pdf or png (default is pdf), if extension="png" then
## filename.png will be generated else filename.pdf

mypdf.png <- function(filename, extension="pdf") {
  if(extension == "pdf"){
    pdf(paste(filename, "pdf", sep="."), width=4.9, height=2.9)
  }else{
    png(paste(filename, ".png", sep=""), width=673, height=399)
  }
  par(mai=c(.4,.8,.3,.2))
}

##====Function for making 1 and 3 year graphs
##== Arguments:
## 1) series : series that needs to be plotted
## 2) filename : function will paste 1 and 3 at the end of the filename provided by user
## 3) ylab : label on Y-axis
## 4) x: if daily then 365, if monthly then
## 5) start: if daily then Sys.Date(), if monthly then as.yearmon(Sys.Date())
## 6) unit: If data needs to be converted to any other unit
## 7) Keeping band and condition for the time being, will remove it later.  

lookback <- function(series, filename, ylab, x, start, unit, band, condition){
  for (lookback in c(1,3)) {
    p <- window(na.omit(series),start=start-(lookback*x))
    p <- unit*p
    pdf(paste(filename,lookback,".pdf",sep=""), width=4.9, height=2.9)
    par(mai=c(.4,.8,.2,.2))
    ## if(condition=="level"){
    ##   levelplot(p,ylab,band=band)
    ##   if (frequency(p)==1 | frequency(p)==12 )
    ##     {timestamp.graph(p)}else{
    ##       timestamp.qrtr(p)}
    ##   dev.off()
    ## }else{
    plot(p, lwd=2, xlab="", ylab=ylab, cex.axis=0.7)
    if (frequency(p)==1 | frequency(p)==12)
      {timestamp.graph(p)}else{
        timestamp.qrtr(p)}
    abline(h=0)
    dev.off()
  }
}


##==== Function for Recession Dates
##== Argument:
## data: Here data stands for dates of peak and trough. Dates run in
## alternate fashion i.e dates at odd no. places are peaks and dates
## at even no. places are troughs
nberDates <- function(data=c(20001231, 20020131, 20081231, 20090331, 20120331, 20140331))
  {
    matrix(ncol = 2, dimnames = list(character(0), c("Start", "End")), byrow = TRUE, data = data)
  }

nberDates <- function ()
{
    matrix(ncol = 2, dimnames = list(character(0), c("Start",
        "End")), byrow = TRUE, data = c(20010101,20011230, 20030301, 20030901, 20040501, 20041131))
}

## These are the common legend for lot of graphs
## legend.nsaar is used when user plots NSAAR and YoY
## legend.saar is used when user plots SAAR and YoY
legend.nsaar <- c("Non-seasonally adjusted annualised rate (3mma)", "Year-on-year growth")
legend.saar <- c("Seasonally adjusted annualised rate (3mma)", "Year-on-year growth")

##==Note:  Functions for time stamp for different frequencies (need to
## reduce the number of these function to minimum)

##==== Function for time stamp and value for quarterly series
##==Arguments:
## 1)series: Series that needs to be plotted

## 2)value : If TRUE than last value will also be pasted with last
## time stamp
timestamp.qrtr <- function(series, value=FALSE ){
  if(value==TRUE){
    if(substr(as.character(as.yearqtr(index(series))[length(index(series))]), 6, 7) ==  "Q1")
      {mtext(paste(paste("Jan-Mar" , substr(tail(na.omit(as.yearqtr(index(series))), 1), 1, 4)),round(series[length(series)], 2), sep="; "), 3, at=tail(index(series),1), cex=0.6, adj=1, col="blue")
     }else if
    (substr(as.character(as.yearqtr(index(series))[length(index(series))]), 6, 7)== "Q2")
    { mtext(paste(paste("Apr-Jun" , substr(tail(na.omit(as.yearqtr(index(series))), 1), 1, 4)), round(series[length(series)], 2),sep="; "), 3, at=tail(index(series), 1), cex=0.6, adj=1, col="blue")
    }else if
    (substr(as.character(as.yearqtr(index(series))[length(index(series))]), 6, 7) == "Q3")
    { mtext(paste(paste("Jul-Sep" , substr(tail(na.omit(as.yearqtr(index(series))),1), 1, 4)), round(series[length(series)], 2), sep="; "), 3, at=tail(index(series), 1), cex=0.6, adj=1, col="blue")
    }else if(substr(as.character(as.yearqtr(index(series))[length(index(series))]), 6, 7) == "Q4")
      {mtext(paste(paste("Oct-Dec" , substr(tail(na.omit(as.yearqtr(index(series))), 1), 1, 4)),round(series[length(series)], 2), sep="; "), 3, at=tail(index(series), 1), cex=0.6, adj=1, col="blue")
     }else{mtext(paste(paste(tail(as.yearmon(index(series)), 1)), round(series[length(series)], 2), sep="; "), 3, at=tail(index(series), 1), cex=0.6, adj=1, col="blue")
         }
  } else {
    if(substr(as.character(as.yearqtr(index(series))[length(index(series))]), 6, 7) == "Q1")
      {mtext(paste("Jan-Mar" , substr(tail(na.omit(as.yearqtr(index(series))), 1), 1, 4)), 3,at=tail(index(series),1), cex=0.6, adj=1, col="blue")           
     }else if
    (substr(as.character(as.yearqtr(index(series))[length(index(series))]), 6, 7) == "Q2")
    { mtext(paste("Apr-Jun" , substr(tail(na.omit(as.yearqtr(index(series))), 1), 1, 4)), 3, at=tail(index(series), 1), cex=0.6, adj=1, col="blue")
    }else if
    (substr(as.character(as.yearqtr(index(series))[length(index(series))]), 6, 7) == "Q3")
    { mtext(paste("Jul-Sep" , substr(tail(na.omit(as.yearqtr(index(series))), 1), 1, 4)), 3, at=tail(index(series), 1), cex=0.6, adj=1, col="blue")
    }else if(substr(as.character(as.yearqtr(index(series))[length(index(series))]), 6, 7)== "Q4")
      {mtext(paste("Oct-Dec" , substr(tail(na.omit(as.yearqtr(index(series))), 1), 1, 4)), 3, at=tail(index(series), 1), cex=0.6, adj=1, col="blue")}
    else{mtext(paste(tail(as.yearmon(index(series)), 1)), 3, at=tail(index(series), 1), cex=0.6, adj=1, col="blue")
       }
  }
}

timestamp.graph <- function(series){
  if(diff(as.numeric((index(series))))[2] > 27 & diff(as.numeric((index(series))))[2] < 40 ){
    mtext(paste(paste(tail(na.omit(as.yearmon(index(series))),1)),round(series[length(series)],2),sep="; "),3,at=tail(index(series),1), cex=0.6,adj=1,col="blue")
  } else {
    mtext(paste(paste(tail(na.omit(index(series)),1)),round(series[length(series)],2),sep="; "),3,at=tail(index(series),1), cex=0.6,adj=1,col="blue")
  }
}

timestamp.month <- function(series){
  mtext(paste(paste(tail(na.omit(as.yearmon(index(series))),1)),round(series[length(series)],2),sep="; "),3,at=tail(index(series),1), cex=0.6,adj=1,col="blue")
}


timestamp.graph.wv <- function(series){
  if(diff(as.numeric((index(series))))[2] > 27){
    mtext(tail(as.yearmon(index(series)),1),3,at=tail(index(series),1), cex=0.6,adj=1,col="blue")
  }else{
    mtext(tail(index(series),1),3,at=tail(index(series),1), cex=0.6,adj=1,col="blue")
  }
}

##==Function to convert price to return
## Argument:
## series: series for which return needs to be calculated 
prices2returns <- function(series) {
  100*diff(log(series))
}

##==Function for converting daily to weekly
nextfri.Date <- function(x) 7 * ceiling(as.numeric(x - 1)/7) + as.Date(1)

## A function which sniffs for structural breaks in growth rates.
## i.e. structural change in an OLS regression log(y) = a + b time
## It plots the lines on the graph and calculates out nominal & real
## growth rates and places these on the graph.
growthrate.strucchange <- function(x)
    {
	x.bare <- log(as.numeric(x))
	time <- 1:length(x.bare)
	bp <- breakpoints(x.bare ~ time)
	bf <- breakfactor(bp)
	m <- lm(x.bare ~ bf*time)
	lines(index(x), exp(predict(m)), col="red")
	cat("Breaks at ", as.character(as.Date(index(x)[bp$breakpoints])), "\n")
	abline(v=index(x)[bp$breakpoints], lty=2)
	text(x=index(x)[bp$breakpoints], y=.95*max(x), as.character(index(x)[bp$breakpoints]), pos = 4, cex = .6, offset = .05)
	cnt.bp <- length(bp$breakpoints)
	## Construct a list of CPI-IW inflation averages over these periods --
	cpi.iw <- tsdb("in.cpi.iw", freq="monthly")
	dateslist <- as.yearmon(c(index(x)[1], as.yearmon(as.Date(index(x)[bp$breakpoints])), as.yearmon(index(x)[length(x)]))) + 2/12
	cpi.iw.values <- as.numeric(cpi.iw[dateslist])
	timespans <- as.numeric(diff(as.Date(dateslist)))/365
	inflation <- rep(NA, (cnt.bp + 1))
	for (i in 1:(length(dateslist) - 1)) 
		{
		    inflation[i] <- (cpi.iw.values[i+1]/cpi.iw.values[i])^(1/timespans[i])
		}
	inflation <- 100*(inflation-1)
	## Place nominal and real growth rates into the graph --
	for.slopes <- data.frame(m$coef)
	time.pos <- grep("^time$", rownames(for.slopes))
	slopes <- ""
	slopes[1:(time.pos - 1)] <- NA
	slopes[time.pos] <- m$coef["time"]
	for(i in (time.pos + 1):(nrow(for.slopes)))
	    {
        	slopes[i] <- m$coef[time.pos] + m$coef[i]
    	    }
	slopes <- round(as.numeric(slopes[!is.na(slopes)]), 4)
	n.a.g <- round(100*((exp(slopes)^4)-1),2)
	n.a.g.real <- round(100*((exp(slopes)^4)-1) - inflation,2)
	s <- rep(NA, (length(dateslist) - 1))
	for (i in 1:(length(dateslist) - 1))
    		{
		        s[i] <- paste(n.a.g[i], paste("(", n.a.g.real[i], ")", sep=""))
    		}
	pos.gr <- rollapply(c(1, bp$breakpoints, NROW(tmp)), width = 2, mean)
	mtext(s, side = 3, cex = .5, at = index(x)[pos.gr])
    }


##== Note : Function for YoY and SAAR needs to merged to make one
## composite function that will work for all the frequencies

##== Function to calculate YoY of monthly series
## Argument
## series stands for the monthly series for which YoY needs to be
## calculated
yoy.monthly <- function(series) {
  serieslag <- lag(series, -12)
  100*(series-serieslag)/serieslag
}

##==Function to calculate YoY of annual series
## Argument
## series stands for the annual series for which YoY needs to be
## calculated
yoy.annual <- function(series) {
  serieslag <- lag(series, -1)
  100*(series-serieslag)/serieslag
}

##== Function to calculate YoY of quarterly series
## Argument
## series stands for the quarterly series for which YoY needs to be
## calculated
yoy.qtr <- function(series){ 
  serieslag <- lag(series ,-4)
  100*(series-serieslag)/serieslag 
}

##== Function to calculate SAAR of monthly series
## Argument
## series stands for the monthly series for which SAAR needs to be
## calculated
series.saar<-function(x){
  x.saar <- 1200*diff(log(x))
  x.saar.3mma <- rollapply(x.saar, FUN="mean", width=3, align="right")
  x.saar.3mma <- x.saar.3mma
  result<-x.saar.3mma
  return(result)
}
##== Function to calculate SAAR of quarterly series
## Argument
## series stands for the quarterly series for which SAAR needs to be
## calculated
series.saar.qtr <- function(x){
  x.saar <- 400*diff(log(x))
  result <- x.saar
  return(result)
}

series.yoy <- function(series){
  index(series) <- as.Date(index(series), format="%Y-%m-%d")
  series.yoy <- yoy.monthly(series)
  series.yoy <- series.yoy
  return(series.yoy)
}

##==Function to create index out of an object that contains multiple columns
## Arguments
## series : object that contain multiple columns and all of them
## need to be indexed to same base year
index.series <- function(series){
  series.first <- series[1,]
  series[1,] <- 100
  for(i in 2:nrow(series)){
    for (j in 1:2){
      series[i,j] <- (as.numeric(series[i,j])*(100/series.first[1,j]))
    }
  }
  return(series)
}

##==Functionfor to create index of one series
index.one.series <- function(series){
  series.first <- series[1]
  series[1] <- 100
  for(i in 2:length(series)){
      series[i] <- (as.numeric(series[i])*(100/series.first[1]))
    }
  return(series)
}

##==Function for generating gentable
## Arguments:
## 1) x stands for the output of xtabled
## 2) basename: genfile name
## 3) include.rownames if rownames needs to be included in gentable
genxtable <- function(x, basename, include.rownames=FALSE) {
  print(x,
        type="latex",
        file=paste(basename,".gen", sep=""),
        include.rownames=include.rownames,
        table.placement="tp",
        caption.placement="top",
        sanitize.text=function(x)x,
        latex.environments=c("center","footnotesize"), signif.stars = TRUE, floating=TRUE)
}

##==Function for Chainlinking
## Arguments:
## tmp stands for data that contains two columns, First column of the
## object should be old base year series and second column should be
## new base year series
chainlink <- function(tmp) {
  link <- function(old.series,new.series) {
    ## Making one dataset with both series
    tmp1 <- merge(old.series,new.series ,all=TRUE)
    start <- complete.cases(tmp1)
    link.date <- index(tmp1)[start][1]
    cat("The first overlapping period is:", as.character(link.date), "\n")
    cutoff <- window(old.series,end=index(old.series)[as.numeric(which(index(old.series)==link.date))-1])
    link.factor <- new.series[link.date,]/old.series[link.date,]
    cutoff <- cutoff * coredata(link.factor)
    final <- rbind(cutoff,window(new.series ,start=link.date))
    final
  }
  if (NCOL(tmp) > 2) {
    cat("There are more than two series that require base year linking. Proceeding to chain link recursively.\n")
    k <- NULL
    for (i in 1:(ncol(tmp)-1)) {
      cat("Linking:",i,"\n")
      if (i==1) {
        k <- link(old.series=tmp[,i],new.series=tmp[,i+1])
      } else {
        k <- link(old.series=k,new.series=tmp[,i+1])
      }
    }
  } else {
    k <- link(old.series=tmp[,1],new.series=tmp[,2])
  }
  return(k)
}

## To add datapoints to the series in case if TSDB is not updated
addData <- function(series, datapoint, data) {
    series <- window(series, start = index(series)[1])
    if(class(index(series)) == "yearqtr") {
        pseudo <- zoo(NA, order.by = as.yearqtr(seq(as.Date(index(series)[length(series)]), as.Date(index(series)[length(series)] + datapoint/4), by = "3 months")))
            }
    if (class(index(series)) == "yearmon")
        {
            pseudo <- zoo(NA, order.by = as.yearmon(seq(as.Date(index(series)[length(series)]), as.Date(index(series)[length(series)] + datapoint/4), by = "months")))
                   }
    pseudo <- pseudo[-1]
    for(i in 1:length(pseudo)) { pseudo[i] <- data[i] }
    dat <- append(series, pseudo)
    return(dat)
}

### Check for the dates
fullData <- function(tmp)
  {
    if(class(index(tmp)) == "yearmon")
      {
        tmp <- window(tmp)
        today <- as.yearmon(as.Date(as.character(Sys.Date()), "%Y-%m-%d"))
        dates <- as.yearmon(seq(as.Date(index(tmp)[NROW(tmp)]), as.Date(today), by = "months"))
        dates <- dates[-1]
        if(NCOL(tmp) == 1)
          {
            comp.date <- zoo(NA, dates)
            full.data <- append(window(comp.date), tmp)
          }
        if(NCOL(tmp) > 1)
          {
            comp.date <- zoo(matrix(NA, ncol = NCOL(tmp), nrow = length(dates)), dates)
            colnames(comp.date) <- colnames(tmp)
            full.data <- rbind(window(comp.date), tmp)
          }
      }
    if(class(index(tmp)) == "Date")
      {
        tmp <- window(tmp)
        today <- as.Date(as.character(Sys.Date()), "%Y-%m-%d")
        dates <- seq(as.Date(index(tmp)[NROW(tmp)]), as.Date(today), by = "days")
        dates <- dates[-1]
        if(NCOL(tmp) == 1)
          {
            comp.date <- zoo(NA, dates)
            full.data <- append(window(comp.date), tmp)
          }
        if(NCOL(tmp) > 1)
          {
            comp.date <- zoo(matrix(NA, ncol = NCOL(tmp), nrow = length(dates)), dates)
            colnames(comp.date) <- colnames(tmp)
            full.data <- rbind(window(comp.date), tmp)
          }
      }
    if(class(index(tmp)) == "Date" & mean(diff(index(tmp)), na.rm = TRUE) > 364)
      {
        tmp <- window(tmp)
        today <- as.Date(as.character(Sys.Date()), "%Y-%m-%d")
        dates <- seq(as.Date(index(tmp)[NROW(tmp)]), as.Date(today), by = "years")
        dates <- dates[-1]
        comp.date <- zoo(NA, dates)
        if(NCOL(tmp) == 1)
          {
            comp.date <- zoo(NA, dates)
            full.data <- append(window(comp.date), tmp)
          }
        if(NCOL(tmp) > 1)
          {
            comp.date <- zoo(matrix(NA, ncol = NCOL(tmp), nrow = length(dates)), dates)
            colnames(comp.date) <- colnames(tmp)
            full.data <- rbind(window(comp.date), tmp)
          }
      }
    if(class(index(tmp)) == "yearqtr")
      {
        tmp <- window(tmp)
        today <- as.yearqtr(as.Date(as.character(Sys.Date()), "%Y-%m-%d"))
        dates <- as.yearqtr(seq(as.Date(index(tmp)[NROW(tmp)]), as.Date(today), by = "3 months"))
        dates <- dates[-1]
        if(NCOL(tmp) == 1)
          {
            comp.date <- zoo(NA, dates)
            full.data <- append(window(comp.date), tmp)
          }
        if(NCOL(tmp) > 1)
          {
            comp.date <- zoo(matrix(NA, ncol = NCOL(tmp), nrow = length(dates)), dates)
            colnames(comp.date) <- colnames(tmp)
            full.data <- rbind(window(comp.date), tmp)
          }
      }
    return(full.data)
  }


recgraph <- function(tmp, ylab, filename, extension = "pdf", cols = c("orangered4","black"), shade = FALSE, type = "line", typl = 1, leg = FALSE, leg.pos = NULL, legend = NULL, bands = 0, h = NULL, v = NULL)
    {
        tmp <- fullData(tmp)
        object <- tmp
        if(extension=="pdf")
            {
                pdf(paste (filename, ".pdf", sep=""), width=4.9, height=2.9)
            } 
        else 
            {
                png(paste(filename, ".png", sep=""), width=673, height=399)
            }
        if(type == "line")
            {
 
                index(tmp) <- as.Date(index(tmp))
                par(mai = c(.4, .8, .3, .2))
		dates <- as.Date(seq(as.Date("1999-04-01"), as.Date(Sys.Date()), by = "days"), "%Y-%m-%d")
		plot.zoo(tmp, type = "n", xaxt = "n", xlab = "", ylab = ylab, xlim = range(dates), screen = "multiple", cex.axis = .6, cex.lab = .6, las = 1)
		pos <- grep("-04-01", dates)
		ticks <- dates[grep("-04-01", dates)]
		axis(1, at = dates[pos], labels = substr(ticks, 1, 4), cex.axis = .6)
		tdiff <- as.numeric(diff(as.Date(index(tmp)[((NROW(tmp) - 1):NROW(tmp))])))
                    if(NCOL(tmp) > 1)
                    {
                           
                        timestamp.graph.wv(na.omit(tmp))
                    }
                if(NCOL(tmp) == 1)
                    {
                       if(tdiff > 20 & tdiff < 95 )
			{  
                          if(tdiff > 80 & tdiff < 95)
				{
			  	timestamp.qrtr(na.omit(tmp), value = TRUE)
				}
                          if(tdiff > 20 & tdiff < 35)
				{
			  	timestamp.month(na.omit(tmp))
				}
			}
                        else {
                                	mtext.val <- tail(na.omit(tmp), 1)
				lval <- as.numeric(unlist(mtext.val))
				lval.time <- tail(index(na.omit(tmp)), 1)
				time.val <- paste(as.character(lval.time), as.character(lval), sep = ": ")
				mtext(time.val, side = 3, at = index(tmp)[which(index(tmp) %in% lval.time)], cex = .55, col = "blue", adj = 1)	
                             }
		     }
		index(tmp) <- as.Date(index(tmp))
                if(shade == TRUE)
                    {
                        if(class(index(tmp))=="Date")
                            {
                                d1 <- as.Date("2000-12-31")
                                d2 <- as.Date("2002-01-31")
                                d3 <- as.Date("2008-12-31")
                                d4 <- as.Date("2009-03-31")
                                d5 <- as.Date("2012-03-31")
                                d6 <- as.Date(Sys.Date())
                            }
                        rect(d1, par("usr")[3], d2, par("usr")[4], col = "grey 90", border = 0)
                        rect(d3, par("usr")[3], d4, par("usr")[4], col = "grey 90", border = 0)
                        rect(d5, par("usr")[3], d6, par("usr")[4], col = "grey 90", border = 0)
                        box()
                    }
                par(new = TRUE)
	for(i in 1:NCOL(tmp))
		  {
		    lines(tmp[, i], xaxt = "n", lwd = 1.5, xlab = "", type= substr(type, 1, 1), cex.axis = 0.6, cex.lab = .6, col = cols[i], lty = typl, xaxt = "n", las = 1)
                abline(h = 0, col = "black", lwd = 2, lty = 1)
		  }
                if(leg == TRUE)
                    {
                        legend(leg.pos, legend = legend, bty = "n", cex = .55, col = cols, lty = typl)	
                    }
                if (bands==1) 
                    {
                        abline(h=median(as.numeric(tmp), na.rm=TRUE),
                               lty=2, lwd=2, col="blue")
                    }
                if (bands==3) 
                    {
                        abline(h=quantile(as.numeric(tmp), prob=c(.25,.5,.75), na.rm=TRUE),
                               lty=2, lwd=c(1,2,1), col="blue")
                    }
                abline(h = 0, col = "black", lwd = 1.5, lty = 1)
                if(!is.null(h) | !is.null(v))
                    {
                        abline(h = h, v= v, col = "black", lwd = 1.5, lty = 1)
                    }
            }
        if(type == "bar")
            {
		index(tmp) <- as.Date(index(tmp))
		par(mai = c(.4, .8, .3, .2))	
		first.timestamp <- index(tmp)[1]
		last.value.tocreate <- index(tmp)[1] - 90
		tmp1 <- zoo(0, seq(as.Date("1999-04-01"), as.Date(first.timestamp), by = "3 months"))
		tmp1 <- tmp1[-length(tmp1)]
		tmp <- c(tmp1, window(tmp))
		dates <- as.Date(seq(as.Date("1999-04-01"), as.Date(Sys.Date()), by = "days"), "%Y-%m-%d")
		plot(tmp, type = "n", xaxt = "n", xlab = "", ylab = ylab, xlim = range(dates), screen = "multiple", cex.axis = .6, las = 1)
		pos <- grep("-04-01", dates)
		ticks <- dates[grep("-04-01", dates)]
		axis(1, at = dates[pos], labels = substr(ticks, 1, 4), cex.axis = .6)
		tdiff <- as.numeric(diff(as.Date(index(tmp)[((NROW(tmp) - 1):NROW(tmp))])))
		if(NCOL(tmp) == 1)
                    {

                       if(tdiff > 20 & tdiff < 95 )
			{  
                          if(tdiff > 80 & tdiff < 95)
				{
			  	timestamp.qrtr(na.omit(tmp), value = TRUE)
				}
                          if(tdiff > 20 & tdiff < 35)
				{
			  	timestamp.month(na.omit(tmp))
				}
			}
                        else {
			       lval <- as.numeric(unlist(tmp[NROW(tmp)]))
			       lval.time <- tail(index(na.omit(tmp)), 1)
			       time.val <- paste(as.character(lval.time), as.character(lval), sep = ": ")
			       mtext(time.val, side = 3, at = index(tmp)[NROW(tmp)], cex = .55, col = "blue", adj = 1)		
                             }
		     }
                if(shade == TRUE)
                    {
                        if(class(index(tmp))=="Date")
                            {
                                d1 <- as.Date("2000-12-31")
                                d2 <- as.Date("2002-01-31")
                                d3 <- as.Date("2008-12-31")
                                d4 <- as.Date("2009-03-31")
                                d5 <- as.Date("2012-03-31")
                                d6 <- Sys.Date()
                            }
                        rect(d1, par("usr")[3], d2, par("usr")[4], col = "grey 90", border = 0)
                        rect(d3, par("usr")[3], d4, par("usr")[4], col = "grey 90", border = 0)
                        rect(d5, par("usr")[3], d6, par("usr")[4], col = "grey 90", border = 0)
                        box() 
                    }
                par(new = TRUE)
                barplot(t(tmp), xlab = "", las = 1, yaxs = "r", xaxt = "n", yaxt = "n")
                abline(h = 0, col = "black", lwd = 2, lty = 1)
                box()
                if(leg == TRUE)
                    {
                        legend(leg.pos, legend = legend, bty = "n", cex = .55, col = cols, lty = typl)	
                    }
                if (bands==1) 
                    {
                        abline(h=median(as.numeric(tmp), na.rm=TRUE),
                               lty=2, lwd=2, col="blue")
                    }
                if (bands==3) 
                    {
                        abline(h=quantile(as.numeric(tmp), prob=c(.25,.5,.75), na.rm=TRUE),
                               lty=2, lwd=c(1,2,1), col="blue")
                    }
                if(!is.null(h) | !is.null(v))
                    {
                        abline(h = h, v= v, col = "black", lwd = 2, lty = 1)
                    }
            }
        dev.off()
    }

Shade <- function(tmp)
    {
	tmp <- fullData(tmp)
	all <- zoo(NA, as.yearqtr(as.Date("1999-04-01")))
	tmp <- c(all, tmp)
	plot.zoo(tmp, type = "n", xlab="", xaxt="n", yaxt = "n", las = 1, ylab = "", screen="multiple", xlim = c(as.yearqtr(as.Date("1999-04-01")), as.yearqtr(Sys.Date())))
        if(NROW(tmp) <= 7)
            {
                ticks <- index(tmp)
                axis(1, at = ticks, labels = ticks, cex.axis = .6)
            }
        if(class(index(tmp))=="yearqtr")
            {
                d1 <- as.yearqtr(as.Date("2000-12-31"))
                d2 <- as.yearqtr(as.Date("2002-01-31"))
                d3 <- as.yearqtr(as.Date("2008-12-31"))
                d4 <- as.yearqtr(as.Date("2009-03-31"))
                d5 <- as.yearqtr(as.Date("2012-03-31"))
                d6 <- as.yearqtr(Sys.Date())
            }
        if(class(index(tmp))=="Date")
            {
                d1 <- as.Date("2000-12-31")
                d2 <- as.Date("2002-01-31")
                d3 <- as.Date("2008-12-31")
                d4 <- as.Date("2009-03-31")
                d5 <- as.Date("2012-03-31")
                d6 <- Sys.Date()
            }
        if(class(index(tmp))=="yearmon")
            {
                d1 <- as.yearmon(as.Date("2000-12-31"))
                d2 <- as.yearmon(as.Date("2002-01-31"))
                d3 <- as.yearmon(as.Date("2008-12-31"))
                d4 <- as.yearmon(as.Date("2009-03-31"))
                d5 <- as.yearmon(as.Date("2012-03-31"))
                d6 <- as.yearmon(Sys.Date())
            }
        rect(d1, par("usr")[3], d2, par("usr")[4], col = "grey 90", border = 0)
        rect(d3, par("usr")[3], d4, par("usr")[4], col = "grey 90", border = 0)
        rect(d5, par("usr")[3], d6, par("usr")[4], col = "grey 90", border = 0)
    }

slotYear <- function(series)
{
  ### Total No. of unique companies
  unique <- length(unique(series$sa_finance1_cocode))
  cat("There are", unique, "companies in the series \n")
   #### Find March ending companies
  fyend.dat <- series[grep("-03-31", series$sa_finance1_year), ]
  fyend.co <- length(unique(fyend.dat$sa_finance1_cocode))
  cat("There are", fyend.co, "companies records which has FY ending in March \n")
  series$sa_finance1_year <- as.Date(series$sa_finance1_year, "%Y-%m-%d")
  ## Creating maximum annual reference year window series
  year <- as.numeric(sort(unique(substr(series$sa_finance1_year, 1, 4))))
  seq.yr <- as.Date(paste(seq(year[1] - 1, year[length(year)] + 1, by = 1), "03-31", sep = "-"), "%Y-%m-%d")
 ## Splitting the data frame into list by company names
  co <- split(series, series$sa_company_name)
  k <- lapply(co, function(x)
              {
                dates.diff <- new("list")
                for(j in 1:nrow(x)) {
                  dates.diff[[j]] <- x[j, 3] - seq.yr 
                }
                dates.diff <- data.frame(do.call("rbind", dates.diff))
                dates.diff[dates.diff < 0] <- dates.diff[dates.diff < 0] * (-1)
                colnames(dates.diff) <- seq.yr
                rownames(dates.diff) <- x$sa_finance1_year
                slot.dates <- ""
                for(i in 1:nrow(dates.diff))
                  { 
                    if(sum(grep("-09-30", rownames(dates.diff)[i])) == 1)
                      {  
                        slot.dates[i] <- paste(substr(rownames(dates.diff)[i], 1, 4), "03-31", sep = "-")
                      }
                    else
                      {
                        slot.dates[i] <- colnames(dates.diff)[match(min(dates.diff[i,]), dates.diff[i,])] 
                      }
                  }
                x$sa_finance1_year <- as.Date(slot.dates, "%Y-%m-%d")  
                if(sum(duplicated(x$sa_finance1_year)) >= 1)
                  {
                    x <- x[-which(duplicated(x$sa_finance1_year, fromLast=TRUE)),]
                  }
                return(x)
              })
  new.series <- do.call("rbind", k)
  rownames(new.series) <- NULL
#  new.series <-  new.series[-which(new.series$sa_finance1_year == seq.yr[2]),]
#  new.series <- new.series[, -1]
  return(new.series)
#  new.series <- dcast(new.series, sa_company_name ~ sa_finance1_year, value.var = colnames(new.series)[ncol(new.series)])
}



winsorise <- function(x, cutoff=0.01) {
  stopifnot(length(x)>0, cutoff>0)
  osd <-  sd(x)
  values <- quantile(x, p=c(cutoff,1-cutoff), na.rm=TRUE)
  winsorised.left <- x<values[1]
  winsorised.right <- x>values[2]       # From here on, I start writing into x
  x[winsorised.left] <- NA
  x[winsorised.right] <- NA
  list(winsorised=x,
       values=values,
       osd=osd, nsd=sd(x),
       winsorised.left=winsorised.left, winsorised.right=winsorised.right)
} 
