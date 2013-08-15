#' Generates a matrix with a minimum and maximum value for xvect and yvect - helper function for heatmap plot
#'
#' @param xvect a numerical vector
#' @param yvect a numerical vector
#' @export
#' 
MinMax = function(xvect, yvect) {
  MinMaxMatrix = base::matrix(0,nrow=2,ncol=2)
  MinMaxMatrix = base::as.data.frame(MinMaxMatrix)
  names(MinMaxMatrix) = base::c('xvect', 'yvect')
  row.names(MinMaxMatrix) = base::c('min', 'max')
  MinMaxMatrix['min', 'xvect'] = base::min(xvect, na.rm=T)
  MinMaxMatrix['max', 'xvect'] = base::max(xvect, na.rm=T)
  MinMaxMatrix['min', 'yvect'] = base::min(yvect, na.rm=T)
  MinMaxMatrix['max', 'yvect'] = base::max(yvect, na.rm=T)
  
#   MinMaxMatrix['min', 'xvect'] = stats::runif(1, 1, 1.1) * MinMaxMatrix['min', 'xvect']
#   MinMaxMatrix['max', 'xvect'] = stats::runif(1, 1, 1.1) * MinMaxMatrix['max', 'xvect'] # * runif(1, 1, 1.1)
#   MinMaxMatrix['min', 'yvect'] = stats::runif(1, 1, 1.1) * MinMaxMatrix['min', 'yvect'] # * runif(1, 1, 1.1)
#   MinMaxMatrix['max', 'yvect'] = stats::runif(1, 1, 1.1) * MinMaxMatrix['max', 'yvect'] # * runif(1, 1, 1.1)
  base::return(MinMaxMatrix)  
}