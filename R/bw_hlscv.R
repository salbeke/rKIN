#' Least-squares cross-validation bandwidth matrix selector for multivariate data.
#'
#' A simple wrapper for the ks::Hlscv function.
#'
#' @param x 2d matrix of data values.
#' @return A numeric vector of estimated x and y bandwidths. Must subset your data if you wish to obtain group specific bandwidths.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming
#' @export
#' @examples
#' data("rodents")
#' # Subset the data for a single species
#' spec1<- rodents[rodents$Species == "Species1", ]
#' # Calculate the bandwidth
#' bw_hlscv(as.matrix(spec1[, c("Ave_C", "Ave_N")]))


bw_hlscv<- function(x){
  if(!inherits(x, "matrix"))
    stop("x must be a 2-d numeric matrix")
  if(!is.numeric(x))
    stop("x must be a 2-d numeric matrix")
  if(dim(x)[2] != 2)
    stop("x must be a 2-d numeric matrix")
  return(ks::Hlscv(x)[c(1, 4)])
}
