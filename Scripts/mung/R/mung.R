#'mung
#'@description
#' MUNG UNTIL NO GOOD
#'@param x a data.frame, matrix or vector to mung.
#'@param MUNGFUN a function to mung with.
#'@param key a 'vector' mapping which columns mung together
#'@return a 'matrix' containing the munged results.
#'@export
mung <- function(x, MUNGFUN = max, key = gsub("[\\.0-9]", "", colnames(x))) {
    out <- do.call(cbind, lapply(unique(key), function(y) {
        key2 <- y == key
        sapply(seq(nrow(x)), function(z) {
            MUNGFUN(as.numeric(x[z, key2]))
        })
    }))
    colnames(out) <- unique(key)
    out
}
