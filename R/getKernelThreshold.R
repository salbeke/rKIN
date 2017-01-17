#' Miscellaneous functions to complete kernel 2D estimates: Get contour threshold values
#'
#' Obtains the quantile threshold levels for a vector of probabilities from a
#' kernel density estimate.
#'
#' @param x Numeric vector of probabilities from a kernel density estimate
#' @param levels Numeric vector of desired percent levels (e.g. c(10, 50, 90). Should not be less than 1 or greater than 99)
#' @return A list of threshold values for each percent.
#' @author Shannon E. Albeke, Wyoming Geographic Information Science Center, University of Wyoming

getKernelThreshold<- function(x, levels = c(50, 75, 95)){
  prob<- NULL
  for(i in levels){
    thresh <- function(z) {
      abs(i/100 - sum(x[x >= z]) / sum(x))
    }# close thresh
    prob<- c(prob, stats::optimize(thresh, c(0, max(x)), tol = .Machine$double.eps)$minimum)
  }#close levels
  out.val<- vector("list", 2)
  names(out.val)<- c("Percent", "Threshold")
  out.val[[1]]<- paste(levels, "%", sep = "")
  out.val[[2]] <- prob
  return(out.val)
}
