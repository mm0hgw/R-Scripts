
logFlag <- T
logFile <- "trm.log"

logCat <- function(x, ...) {
    if (logFlag == T) 
        cat(x, file = logFile, append = T, ...)
}

logPrint <- function(x, ...) {
    if (logFlag == T) 
        capture.output(print(x, ...), file = logFile, append = T)
    return(x)
}

candle <- function(x, y1, y2, hw) {
    list(x = c(x - hw, x + hw, x, x, x + hw, x - hw), y = c(rep(y1, 3), rep(y2, 3)))
}

do.setList <- function(rawTrials, times, setList, groups, as.baseline.fraction = T, 
    report.by.patient = F) {
    setIndex <- seq_along(setList)
    names(setIndex) <- names(setList)
    lapply(setIndex, function(i) {
        set <- setList[[i]]
        setName <- names(setList)[i]
        # Report start
        logCat(paste("Starting Repeated Measure Analysis on", setName, "\nat", format(Sys.time(), 
            "%a %b %d %X %Y %Z\n")))
        logPrint(times)
        logPrint(lapply(set, function(x) colnames(rawTrials)[x]))
        logPrint(lapply(groups, function(x) rownames(rawTrials)[x]))
        
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
                
                # calculate graph lines for mean, sd
                trialSDs <- lapply(seq_along(trial), function(k) {
                  candle(times[k], trialMeans[k] - trialSDs[k], trialMeans[k] + trialSDs[k], 
                    hw)
                })
                trialMean <- list(list(x = times, y = trialMeans))
                
                # package, name and return results
                out <- list(trialMean, trialSDs)
                names(out) <- paste(trialName, c("μ", "σ"), sep = ".")
                out
            })
            do.call(c, patients)
        })
        
        # label output report completion and return results
        names(out) <- paste(sep = ".", setName, names(groups))
        logCat("Processing of", setName, "successfully completed at", format(Sys.time(), 
            "%a %b %d %X %Y %Z\n\n"))
        out
    })
}

deGreek <- function(x) {
    gsub("Α", "A", gsub("Β", "B", x))
}

mungColnames <- function(rawColnames) {
    function(variable) {
        x <- sprintf("^%s%%s[[:digit:]]+$", variable)
        function(event) {
            grep(sprintf(x, event), rawColnames)
        }
    }
}


system(paste("rm", logFile))
MavGCyclingGroups <- list(High.Intensity = c(1, 2, 4, 7, 10), Low.Intensity = c(3, 
    5, 6, 8, 9))
MavGCyclingTimeline <- c(-1/2, 0, 1, 2, 3)
MavGDataFile <- "ExampleData.txt"
rawTrials <- read.csv(MavGDataFile)
rownames(rawTrials) <- paste(seq(nrow(rawTrials)), rawTrials$Group, sep = ".")
colnames(rawTrials) <- deGreek(colnames(rawTrials))
setNameList <- list(Set.One = c(A = "A", B = "B", C = "C", D = "D", E = "E"), Set.Two = c(F = "F", 
    G = "G", H = "H", I = "I", J = "J"))
mungVar <- mungColnames(colnames(rawTrials))
mungEvent <- mungVar("SquatH")
setList <- lapply(setNameList, function(setNames) lapply(setNames, mungEvent))
graphlines <- do.call(c, do.setList(rawTrials, MavGCyclingTimeline, setList, MavGCyclingGroups))
graphCols <- seq_along(graphlines)
dput(graphlines, "graphlines.dput")
lineList <- do.call(c, do.call(c, graphlines))
plotOpts <- list(x = 0, type = "n", xlim = range(do.call(c, sapply(lineList, "[", 
    "x"))), ylim = range(do.call(c, sapply(lineList, "[", "y"))), main = "Cycling study", 
    ylab = "fraction of baseline", xlab = "days after exercise program")
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

MavGDataFile2 <- "~/MyData.csv"
rawTrials2 <- read.csv(MavGDataFile2)
colnames(rawTrials2) <- deGreek(colnames(rawTrials2))

mungVar2 <- mungColnames(colnames(rawTrials2))
mungEvent2 <- mungVar2("SquatH")
setList2 <- lapply(setNameList, function(setNames) lapply(setNames, mungEvent2))
groupAll <- list(All.Patients = T)
graphlines2 <- do.call(c, do.setList(rawTrials2, MavGCyclingTimeline, setList2, groupAll))
graphCols2 <- seq_along(graphlines2)
dput(graphlines2, "graphlines2.dput")
lineList2 <- do.call(c, do.call(c, graphlines2))
plotOpts2 <- list(x = 0, type = "n", xlim = range(do.call(c, sapply(lineList2, "[", 
    "x"))), ylim = range(do.call(c, sapply(lineList2, "[", "y"))), main = "Cycling study", 
    ylab = "fraction of baseline", xlab = "days after exercise program")
legendOpts2 <- list(x = "bottomleft", legend = names(graphlines2), pch = 10, col = graphCols2)

png("trm2.png", height = 768, width = 1024)
do.call(plot, plotOpts2)
do.call(legend, legendOpts2)
lapply(seq_along(graphlines2), function(i) {
    g <- graphlines2[[i]]
    graphCol <- graphCols2[i]
    lapply(g[[1]], lines, col = graphCol, type = "b", pch = 10)
    lapply(g[[2]], lines, col = graphCol)
})
dev.off()

mungEvent3 <- mungVar2("Torque70")
setList3 <- lapply(setNameList, function(setNames) lapply(setNames, mungEvent3))
graphlines3 <- do.call(c, do.setList(rawTrials2, MavGCyclingTimeline, setList3, groupAll))
graphCols3 <- seq_along(graphlines3)
dput(graphlines3, "graphlines3.dput")
lineList3 <- do.call(c, do.call(c, graphlines3))
plotOpts3 <- list(x = 0, type = "n", xlim = range(do.call(c, sapply(lineList3, "[", 
    "x"))), ylim = range(do.call(c, sapply(lineList3, "[", "y"))), main = "Torque70", 
    ylab = "fraction of baseline", xlab = "days after exercise program")
legendOpts3 <- list(x = "bottomleft", legend = names(graphlines3), pch = 10, col = graphCols3)

png("trm3.png", height = 768, width = 1024)
do.call(plot, plotOpts3)
do.call(legend, legendOpts3)
lapply(seq_along(graphlines3), function(i) {
    g <- graphlines3[[i]]
    graphCol <- graphCols3[i]
    lapply(g[[1]], lines, col = graphCol, type = "b", pch = 10)
    lapply(g[[2]], lines, col = graphCol)
})
dev.off()
