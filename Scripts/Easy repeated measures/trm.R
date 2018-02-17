data(setOne)
data(setTwo)
data(HIkey)  #Group=='H'
data(LIkey)  #Group=='L'

times <- c(-0.5, 0, 24, 48, 72)

candle <- function(x,y1,y2,hw){
	list(x=(x-hw,x+hw,x,x,x+hw,x-hw),
	y=c(rep(y1,3),rep(y2,3)
	))
}

do.set <- function(setFiles, keys = c(High.Intensity=HIkey,Low.Intensity=LIkey),as.baseline.fraction = T, report.by.patient=F, pngOpts=list(height=1024,width=768),times=times,colmap=c(1,2,3)) {
hw<- range(times)/20
    trials <- lapply(setFiles, function(fn) read.csv(fn)[, -1])
    names(trials) <- gsub("data/", "", gsub(".csv", "", setFiles))
    lapply(seq_along(keys),function(i){
    key <- keys[i]
    groupName <- names(keys)[i]
    trials <- lapply(trials, "[", key, T)
    if(report.by.patient==F)
    trails <- lapply(trials,function(trial)matrix(unlist(trial),nrow=1))
    lapply(seq(nrow(trials[[1]])),function(j){
    	trial <- lapply(trials,function(tr)tr[j,])
    	qtrial <- lapply(trial,quantile,names=F)
    	if(report.by.patient==T)
    	trialName <- paste(sep='',groupName,'.Patient.',j)
    	else trialName <- groupName
    	fileName <- paste(trialName,'.png',sep='')
    	pngOpts$file <- fileName
    	plotOpts <- list(xlim=c(min(times)-hw,max(times)+hw),ylim=range(unlist(trial)),xlab = "hours from exercise program", type='b'
    	,main=trialName)
    if (as.baseline.fraction == T) 
        plotOpts$ylab <- "Fraction of baseline" else plotOpts$ylab <- "centimetres"
     trialRanges <- lapply(seq_along(trial),function(k){
     	do.call(candle,as.list(c(times[k],qtrial[[k]][c(1,5)],hw)))
     })
     trialQuantiles <- lapply(seq_along(trial),function(k){
     	do.call(candle,as.list(c(times[k],qtrial[[k]][c(2,4)],hw)))
     })
     trialMean <- list(list(x=times,y=sapply(qtrial,'[',3)))
    	do.call(png,pngOpts)
    	do.call(plot,plotOpts)
    	lapply(trialRanges,lines,col=colmap[3])
    	lapply(trialQuantiles,lines,col=colmap[2])
    	lapply(trialMean,lines,col=colmap[1])
    	legend('bottomright',legend=c('Mean','Quantile','Range'),lty=1,col=colmap)
    dev.off()
    })
    })
}

do.set(setOne)
do.set(setTwo)
do.set(setOne,report.by.patient=T)
do.set(setTwo,report.by.patient=T)
