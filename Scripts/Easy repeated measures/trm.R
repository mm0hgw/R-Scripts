MavGCyclingGroups <- list(High.Intensity = c(1, 2, 4, 7, 10), Low.Intensity = c(3, 
    5, 6, 8, 9))
MavGCyclingTimeline <- c(-1/2, 0, 1, 2, 3)
MavGDataFile <- "ExampleData.txt"
MavGCyclingSetOne <- lapply(seq(5) * 3 + 1, seq, length.out = 3)
names(MavGCyclingSetOne) <- c("A", "B", "C", "D", "E")
MavGCyclingSetTwo <- lapply(seq(5) * 3 + 16, seq, length.out = 3)
names(MavGCyclingSetTwo) <- c("F", "G", "H", "I", "J")

debugFlag <- T
logFile <- "trm.log"

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

do.set <- function(csvFile, times, set, setName, groups, as.baseline.fraction = T, 
    report.by.patient = F, do.ranges = F) {
    debugCat(paste("Starting Repeated Measure Analysis on", setName, "from", csvFile, 
        "\nat", format(Sys.time(), "%a %b %d %X %Y %Z\n")))
    hw <- (max(times) - min(times))/20
    rawTrials <- read.csv(csvFile)
    trials <- lapply(set, function(i) rawTrials[, i])
    rm(rawTrials)
    names(trials) <- names(set)
    debugPrint(groups)
    out <- lapply(seq_along(groups), function(i) {
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
                debugPrint(do.call(rbind, trial))
            }
            trialMeans <- sapply(trial, mean)
            trialSDs <- sapply(trial, sd)
            debugPrint(cbind(times, trialMeans, trialSDs))
            fileName <- paste(trialName, ".png", sep = "")
            plotOpts <- list(x = 0, xlim = c(min(times) - hw, max(times) + hw), ylim = range(unlist(trial)), 
                xlab = "days from exercise program", type = "n", main = trialName, 
                sub = paste("Sample size:", length(trial[[1]])))
            if (as.baseline.fraction == T) 
                plotOpts$ylab <- "Fraction of baseline" else plotOpts$ylab <- "centimetres"
            trialRanges <- lapply(seq_along(trial), function(k) {
                do.call(candle, list(times[k], min(trial[[k]]), max(trial[[k]]), 
                  hw))
            })
            trialSDs <- lapply(seq_along(trial), function(k) {
                do.call(candle, list(times[k], trialMeans[k] - trialSDs[k], trialMeans[k] + 
                  trialSDs[k], hw))
            })
            trialMean <- list(list(x = times, y = trialMeans))
            out <- list(trialMean, trialSDs, trialRanges)
            names(out) <- paste(trialName, c("μ", "σ", "Range"), sep = ".")
            if (do.ranges != T) 
                out <- out[-3]
            out
        })
    })
    names(out) <- paste(sep = ".", setName, names(groups))
    debugCat("Processing of", setName, "successfully completed at", format(Sys.time(), 
        "%a %b %d %X %Y %Z\n\n"))
    out
}

system(paste("rm", logFile))

capture.output(a <- do.set(MavGDataFile, MavGCyclingTimeline, MavGCyclingSetOne, 
    "Set.One", MavGCyclingGroups), file = logFile, append = T)
capture.output(b <- do.set(MavGDataFile, MavGCyclingTimeline, MavGCyclingSetTwo, 
    "Set.Two", MavGCyclingGroups), file = logFile, append = T)
# capture.output(do.set(MavGDataFile, MavGCyclingTimeline, MavGCyclingSetOne,
# 'Set.One', MavGCyclingGroups, report.by.patient = T), file = logFile, append =
# T) capture.output(do.set(MavGDataFile, MavGCyclingTimeline, MavGCyclingSetTwo,
# 'Set.Two', MavGCyclingGroups, report.by.patient = T), file = logFile, append =
# T)
graphlines <- c(a, b)
graphCols <- seq_along(graphlines)
dput(graphlines, "graphlines.dput")
# png('trm.png')
plotOpts <- list(x = 0, type = "n", xlim = range(do.call(c, sapply(do.call(c, do.call(c, 
    do.call(c, graphlines))), "[", "x"))), ylim = range(do.call(c, sapply(do.call(c, 
    do.call(c, do.call(c, graphlines))), "[", "y"))), main = "Cycling study", ylab = "fraction of baseline", 
    xlab = "days after exercise program")
legendOpts <- list(x = "bottomleft", legend = names(graphlines), pch = 10, col = graphCols)

png("trm.png", height = 768, width = 1024)
do.call(plot, plotOpts)
do.call(legend, legendOpts)
lapply(seq_along(graphlines), function(i) {
    g <- graphlines[[i]][[1]]
    graphCol <- graphCols[i]
    lapply(g[[1]], lines, col = graphCol, type = "b", pch = 10)
    lapply(g[[2]], lines, col = graphCol)
})
dev.off()
