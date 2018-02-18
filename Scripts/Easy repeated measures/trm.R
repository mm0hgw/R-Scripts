MavGCyclingGroups <- list(High.Intensity = c(1,2,4,7,10), Low.Intensity=c(3,5,6,8,9))
MavGCyclingTimeline <- c(-1/2, 0, 1, 2, 3)
MavGDataFile <- 'ExampleData.txt'
MavGCyclingSetOne <- lapply(seq(5)*3+1,seq,length.out=3)
names(MavGCyclingSetOne)<-c('A','B','C','D','E')
MavGCyclingSetTwo <- lapply(seq(5)*3+16,seq,length.out=3)
names(MavGCyclingSetTwo)<-c('F','G','H','I','J')

debugFlag <- T

debugCat <- function(x, ...) {
    if (debugFlag == T) 
        cat(x, ...)
}

debugPrint <- function(x, ...) {
    if (debugFlag == T) 
        print(x, ...)
    return(x)
}

candle <- function(x, y1, y2, hw) {
    list(x = c(x - hw, x + hw, x, x, x + hw, x - hw), y = c(rep(y1, 3), rep(y2, 3)))
}

do.set <- function(csvFile, times, set, setName, groups, as.baseline.fraction = T, report.by.patient = F, 
    pngOpts = list(height = 1024, width = 768),  colmap = c(1, 2, 
        3)) {
    debugCat(paste(collapse = "\n", c(paste("Starting Repeated Measure Analysis on", 
        setName), setFiles, "")))
    hw <- (max(times) - min(times))/20
    rawTrials <- read.csv(csvFile)
    trials <- lapply(set,function(i)rawTrials[,i])
    rm(rawTrials)
    debugPrint(groups)
    lapply(seq_along(groups), function(i) {
        group <- groups[[i]]
        groupName <- paste(sep = ".", setName, names(groups)[i])
        debugCat(paste("Analysing", groupName, "\n"))
        groupTrials <- lapply(trials, "[", group, T)
        if (report.by.patient == F) 
            groupTrials <- lapply(groupTrials, function(trial) matrix(unlist(trial), 
                nrow = 1))
        lapply(seq(nrow(groupTrials[[1]])), function(j) {
            trial <- lapply(groupTrials, function(tr) tr[j, ])
            if (report.by.patient == T) 
                trialName <- paste(sep = "", groupName, ".Patient.", j) else trialName <- groupName
            debugCat(paste("Processing", trialName, "\n"))
            trial <- lapply(groupTrials, function(tr) as.numeric(tr[j, ]))
            debugPrint(do.call(rbind, trial))
            if (as.baseline.fraction == T) {
                trialBaseline <- mean(trial[[1]])
                trial <- lapply(trial, function(tr) tr/trialBaseline)
            }
            trialMeans <- sapply(trial, mean)
            trialSDs <- sapply(trial, sd)
            debugPrint(rbind(times,trialMeans,trialSDs))
            fileName <- paste(trialName, ".png", sep = "")
            pngOpts$file <- fileName
            plotOpts <- list(x = 0, xlim = c(min(times) - hw, max(times) + hw), ylim = range(unlist(trial)), 
                xlab = "days from exercise program", type = "n", main = trialName, 
                sub = paste("Sample size:", length(trial[[1]])))
            if (as.baseline.fraction == T) 
                plotOpts$ylab <- "Fraction of baseline" else plotOpts$ylab <- "centimetres"
            trialRanges <- lapply(seq_along(trial), function(k) {
                do.call(candle, list(times[k], min(trial[[k]]),max(trial[[k]]), 
                 hw))
            })
            trialSDs <- lapply(seq_along(trial), function(k) {
                do.call(candle, list(times[k], 
                trialMeans[k]-trialSDs[k],
                trialMeans[k]+trialSDs[k],
                 hw))
            })
            trialMean <- list(list(x = times, y = trialMeans))
            do.call(png, pngOpts)
            do.call(plot, plotOpts)
            lapply(trialRanges, lines, col = colmap[3])
            lapply(trialSDs, lines, col = colmap[2])
            lapply(trialMean, lines, col = colmap[1])
            legend("bottomright", legend = c("μ", "σ", "Range"), lty = 1, 
                col = colmap)
            dev.off()
        })
    })
}

logical2combo <- function(x) {
    n <- length(x)
    k <- sum(x)
    i <- revCombnG(seq_along(x)[x], n)
    ultraCombo(i, n, k)
}

do.set(MavGDataFile, MavGCyclingTimeline, setOne, "Set.One", MavGCyclingGroups)
do.set(MavGDataFile, MavGCyclingTimeline, setTwo, "Set.Two", MavGCyclingGroups)
do.set(setTwo, MavGCyclingKeys, "SetTwo")
do.set(setOne, MavGCyclingKeys, "SetOne", report.by.patient = T)
do.set(setTwo, MavGCyclingKeys, "SetTwo", report.by.patient = T)
