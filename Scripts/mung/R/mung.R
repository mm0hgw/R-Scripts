#'mung
#'@description
#' MUNG UNTIL NO GOOD
#'@param x a data.frame to mung.
#'@param mungfuns a vector of function names to mung with.
#'@param key a 'vector' mapping which columns mung together
#'@return a 'matrix' containing the munged results.
#'@export
mung <- function(x, mungfuns = c("max", "min", "mean"), key = gsub("[\\.0-9]", "", 
    colnames(x))) {
    if ("data.frame" != class(x)) 
        x <- as.data.frame(x)
    stopifnot('data.frame'==class(x))
    FUNS <- lapply(mungfuns, get)
    stopifnot(all("function" == sapply(FUNS, class)))
    out <- do.call(cbind, lapply(unique(key), function(y) {
        key2 <- grep(paste(sep = "", "^", y, "$"), key)
        out <- do.call(cbind, lapply(FUNS, function(MUNGFUN) {
            sapply(seq(nrow(x)), function(z) {
                MUNGFUN(as.numeric(x[z, key2]))
            })
        }))
        if (length(mungfuns) == 1) {
            colnames(out) <- y
        } else {
            colnames(out) <- paste(y, mungfuns, sep = ".")
        }
        out
    }))
    as.data.frame(out)
}

