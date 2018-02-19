MavGCyclingGroups <- list(High.Intensity = c(1, 2, 4, 7, 10), Low.Intensity = c(3, 
    5, 6, 8, 9))
MavGCyclingTimeline <- c(-1/2, 0, 1, 2, 3)
MavGDataFile <- "ExampleData.txt"
MavGCyclingSetOne <- lapply(seq(5) * 3 + 1, seq, length.out = 3)
names(MavGCyclingSetOne) <- c("A", "B", "C", "D", "E")
MavGCyclingSetTwo <- lapply(seq(5) * 3 + 16, seq, length.out = 3)
names(MavGCyclingSetTwo) <- c("F", "G", "H", "I", "J")

logFlag <- T
logFile <- "trm.log"

logCat <- function(x, ...) {
    if (logFlag == T) 
        cat(x, file = logFile, append = T, ...)
}

logPrint <- function(x, ...) {
    if (logFlag == T) 
        cat(capture.output(print(x, ...)), file = logFile, append = T)
    return(x)
}

candle <- function(x, y1, y2, hw) {
    list(x = c(x - hw, x + hw, x, x, x + hw, x - hw), y = c(rep(y1, 3), rep(y2, 3)))
}

do.set <- function(rawTrials, times, set, setName, groups, as.baseline.fraction = T, 
    report.by.patient = F) {
    # Report start
    logCat(paste("Starting Repeated Measure Analysis on", setName, "\nat", format(Sys.time(), 
        "%a %b %d %X %Y %Z\n")))
    logPrint(times)
    logPrint(set)
    logPrint(groups)
    
    # set candle width
    hw <- (max(times) - min(times))/20
    
    # subset and name trials
    trials <- lapply(set, function(i) rawTrials[, i])
    names(trials) <- names(set)
    
    # lapply across patient groups
    out <- lapply(seq_along(groups), function(i) {
        
        # pull group and name and log
        group <- groups[[i]]
        groupName <- paste(sep = ".", setName, names(groups)[i])
        logCat(paste("Analysing", groupName, "\n"))
        
        # subset trials by patient group
        groupTrials <- lapply(trials, "[", group, T)
        
        
        # flatten results, maybe
        if (report.by.patient != T) 
            groupTrials <- lapply(groupTrials, function(trial) matrix(unlist(trial), 
                nrow = 1))
        
        # lapply across patients (or group)
        patients <- lapply(seq(nrow(groupTrials[[1]])), function(j) {
            
            # name results and report to user
            if (report.by.patient == T) 
                trialName <- paste(sep = "", groupName, ".Patient.", j) else trialName <- groupName
            logCat(paste("Processing", trialName, "\n"))
            
            # subset trials by patient
            trial <- lapply(groupTrials, function(tr) as.numeric(tr[j, ]))
            
            # log to demostrate actual subset
            logPrint(do.call(rbind, trial))
            
            # normalise to baseline and log, maybe
            if (as.baseline.fraction == T) {
                trialBaseline <- mean(trial[[1]])
                trial <- lapply(trial, function(tr) tr/trialBaseline)
                logPrint(do.call(rbind, trial))
            }
            
            # finally got sample! calculate mean and sd and log to demonstrate actual results
            trialMeans <- sapply(trial, mean)
            trialSDs <- sapply(trial, sd)
            logPrint(cbind(times, trialMeans, trialSDs))
            
            # calculate graph lines for mean, sd and range
            trialRanges <- lapply(seq_along(trial), function(k) {
                candle(times[k], min(trial[[k]]), max(trial[[k]]), hw)
            })
            trialSDs <- lapply(seq_along(trial), function(k) {
                candle(times[k], trialMeans[k] - trialSDs[k], trialMeans[k] + trialSDs[k], 
                  hw)
            })
            trialMean <- list(list(x = times, y = trialMeans))
            
            # package, name and return results
            out <- list(trialMean, trialSDs, trialRanges)
            names(out) <- paste(trialName, c("μ", "σ", "Range"), sep = ".")
            out
        })
        do.call(c, patients)
    })
    
    # label output report completion and return results
    names(out) <- paste(sep = ".", setName, names(groups))
    logCat("Processing of", setName, "successfully completed at", format(Sys.time(), 
        "%a %b %d %X %Y %Z\n\n"))
    out
}

system(paste("rm", logFile))

rawTrials <- read.csv(MavGDataFile)
setOne <- do.set(rawTrials, MavGCyclingTimeline, MavGCyclingSetOne, "Set.One", MavGCyclingGroups)
setTwo <- do.set(rawTrials, MavGCyclingTimeline, MavGCyclingSetTwo, "Set.Two", MavGCyclingGroups)
graphlines <- c(setOne, setTwo)
graphCols <- seq_along(graphlines)
dput(graphlines, "graphlines.dput")
# png('trm.png')
plotOpts <- list(x = 0, type = "n", xlim = range(do.call(c, sapply(do.call(c, do.call(c, 
    graphlines)), "[", "x"))), ylim = range(do.call(c, sapply(do.call(c, do.call(c, 
    graphlines)), "[", "y"))), main = "Cycling study", ylab = "fraction of baseline", 
    xlab = "days after exercise program")
legendOpts <- list(x = "bottomleft", legend = names(graphlines), pch = 10, col = graphCols)

png("trm.png", height = 768, width = 1024)
do.call(plot, plotOpts)
do.call(legend, legendOpts)
lapply(seq_along(graphlines), function(i) {
    g <- graphlines[[i]]
    graphCol <- graphCols[i]
    lapply(g[[1]], lines, col = graphCol, type = "b", pch = 10)
    lapply(g[[2]], lines, col = graphCol)
})
dev.off()
