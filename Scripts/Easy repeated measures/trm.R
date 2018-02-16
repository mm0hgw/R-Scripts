data(setOne)
data(setTwo)
data(HIkey)  #Group=='H'
data(LIkey)  #Group=='L'

times <- c(-0.5, 0, 24, 48, 72)

do.trials <- function(trials, normalise = T) {
    out <- sapply(trials, function(t) mean(unlist(t)))
    if (normalise == T) 
        out <- out/out[1]
    out
}

do.set <- function(setFiles, normalise = T, ...) {
    trials <- lapply(setFiles, function(fn) read.csv(fn)[, -1])
    names(trials) <- gsub("data/", "", gsub(".csv", "", setFiles))
    HItrials <- lapply(trials, "[", HIkey, T)
    LItrials <- lapply(trials, "[", LIkey, T)
    HImeans <- do.trials(HItrials, normalise)
    LImeans <- do.trials(LItrials, normalise)
    ylim <- range(HImeans, LImeans)
    xlim <- range(times)
    if (normalise == T) 
        ylab <- "Fraction of baseline" else ylab <- "Raw measurement (cm)"
    plot(0, type = "n", xlim = xlim, ylim = ylim, xlab = "hours from exercise program", 
        ylab = ylab, ...)
    lines(x = times, y = HImeans, type = "b", col = 2)
    lines(x = times, y = LImeans, type = "b", col = 3)
}

png("setOne.png")
do.set(setOne, main = "Set One Normalised")
dev.off()
png("setTwo.png")
do.set(setTwo, main = "Set Two Normalised")
dev.off()
png("setOneRaw.png")
do.set(setOne, F, main = "Set One Raw")
dev.off()
png("setTwoRaw.png")
do.set(setTwo, F, main = "Set Two Raw")
dev.off()
