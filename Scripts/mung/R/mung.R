#'mung
#'@description
#' MUNG UNTIL NO GOOD
#'@param x a data.frame, matrix or vector to mung.
#'@param mungfuns a vector of function names to mung with.
#'@param key a 'vector' mapping which columns mung together
#'@return a 'matrix' containing the munged results.
#'@export
mung <- function(x, mungfuns = c("max", "min", "mean"), key = gsub("[\\.0-9]", "", 
    colnames(x))) {
    FUNS <- lapply(mungfuns, get)
    stopifnot(all("function" == sapply(FUNS, class)))
    out <- do.call(cbind, lapply(unique(key), function(y) {
        key2 <- grep(y, key)
        out <- do.call(cbind, lapply(FUNS, function(MUNGFUN) {
            sapply(seq(nrow(x)), function(z) {
                MUNGFUN(as.numeric(x[z, key2]))
            })
        }))
        colnames(out) <- paste(y, mungfuns, sep = ".")
        out
    }))
    as.data.frame(out)
}
