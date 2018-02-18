MavGCyclingKeys <- list(High.Intensity = ultraCombo(33, 10, 5), Low.Intensity = ultraCombo(220, 
    10, 5))

setOne <- c("data/A.csv", "data/B.csv", "data/C.csv", "data/D.csv", "data/E.csv")
setTwo <- c("data/F.csv", "data/G.csv", "data/H.csv", "data/I.csv", "data/J.csv")

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

timesx <- c(-1/2, 0, 1, 2, 3)

candle <- function(x, y1, y2, hw) {
    list(x = c(x - hw, x + hw, x, x, x + hw, x - hw), y = c(rep(y1, 3), rep(y2, 3)))
}

do.set <- function(setFiles, keys, setName, as.baseline.fraction = T, report.by.patient = F, 
    pngOpts = list(height = 1024, width = 768), times = timesx, colmap = c(1, 2, 
        3)) {
    debugCat(paste(collapse = "\n", c(paste("Starting Repeated Measure Analysis on", 
        setName), setFiles, "")))
    hw <- (max(times) - min(times))/20
    trials <- lapply(setFiles, function(fn) read.csv(fn)[, -1])
    names(trials) <- gsub("data/", "", gsub(".csv", "", setFiles))
    debugPrint(keys)
    lapply(seq_along(keys), function(i) {
        key <- keys[[i]]
        groupName <- paste(sep = ".", setName, names(keys)[i])
        debugCat(paste("Analysing", groupName, "\n"))
        groupTrials <- lapply(trials, "[", key$Gen(1), T)
        if (report.by.patient == F) 
            groupTrials <- lapply(groupTrials, function(trial) matrix(unlist(trial), 
                nrow = 1))
        lapply(seq(nrow(groupTrials[[1]])), function(j) {
            if (report.by.patient == T) 
                trialName <- paste(sep = "", groupName, ".Patient.", j) else trialName <- groupName
            debugCat(paste("Processing", trialName, "\n"))
            trial <- lapply(groupTrials, function(tr) as.numeric(tr[j, ]))
            debugPrint(do.call(rbind, trial))
            if (as.baseline.fraction == T) {
                trialBaseline <- mean(trial[[1]])
                trial <- lapply(trial, function(tr) tr/trialBaseline)
            }
            qtrial <- lapply(trial, quantile, names = F)
            fileName <- paste(trialName, ".png", sep = "")
            pngOpts$file <- fileName
            plotOpts <- list(x = 0, xlim = c(min(times) - hw, max(times) + hw), ylim = range(unlist(trial)), 
                xlab = "days from exercise program", type = "n", main = trialName, 
                sub = paste("Sample size:", length(trial[[1]])))
            if (as.baseline.fraction == T) 
                plotOpts$ylab <- "Fraction of baseline" else plotOpts$ylab <- "centimetres"
            trialRanges <- lapply(seq_along(trial), function(k) {
                do.call(candle, list(times[k], qtrial[[k]][1], qtrial[[k]][5], hw))
            })
            trialQuantiles <- lapply(seq_along(trial), function(k) {
                do.call(candle, list(times[k], qtrial[[k]][2], qtrial[[k]][4], hw))
            })
            trialMean <- list(list(x = times, y = sapply(qtrial, "[", 3)))
            do.call(png, pngOpts)
            do.call(plot, plotOpts)
            lapply(trialRanges, lines, col = colmap[3])
            lapply(trialQuantiles, lines, col = colmap[2])
            lapply(trialMean, lines, col = colmap[1], pch = 10, type = "b")
            legend("bottomright", legend = c("Mean", "Quantile", "Range"), lty = 1, 
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

do.set(setOne, MavGCyclingKeys, "SetOne")
do.set(setTwo, MavGCyclingKeys, "SetTwo")
do.set(setOne, MavGCyclingKeys, "SetOne", report.by.patient = T)
do.set(setTwo, MavGCyclingKeys, "SetTwo", report.by.patient = T)
