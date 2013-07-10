#' Generates a density grid with a priori defined limits
#'
#' @param xvect a numerical vector
#' @param yvect a numerical vector
#' @param x.min a minimum value for the x axis of the grid density object
#' @param x.max a maximum value for the x axis of the grid density object
#' @param y.min a minimum value for the y axis of the grid density object
#' @param y.max a maximum value for the y axis of the grid density object
#' @export
#' 
grid.density.limits <- function(xvect,yvect, x.min, x.max, y.min, y.max)
{
  xvect.save<-xvect
  yvect.save<-yvect
  
  xvect.not.missing<-1-(base::is.na(xvect)*1)
  yvect.not.missing<-1-(base::is.na(yvect)*1)
  
  x.and.y.present<-xvect.not.missing*yvect.not.missing
  
  xvect<-xvect.save[x.and.y.present==1]
  yvect<-yvect.save[x.and.y.present==1]
  
  
  
#   #par(mfrow=c(1,1))
#   y.min<-base::min(yvect)
#   x.min<-base::min(xvect)
#   y.max<-base::max(yvect)
#   x.max<-base::max(xvect)
  
  
#   y.range<-base::max(yvect)-base::min(yvect)
#   x.range<-base::max(xvect)-base::min(xvect)
  
  y.range<-y.max-y.min
  x.range<-x.max-x.min
  
  numints<-20
  y.interval<-y.range/numints
  x.interval<-x.range/numints
  
  y.cuts<-base::seq(from=y.min,to=y.max,by=y.interval)
  y.mids<-base::seq(from=(y.min+y.interval/2),to=(y.max-y.interval/2),by=y.interval)
  y.cuts[numints+1]<-y.cuts[numints+1]*1.001
  
  x.cuts<-base::seq(from=x.min,to=x.max,by=x.interval)
  x.mids<-base::seq(from=(x.min+x.interval/2),to=(x.max-x.interval/2),by=x.interval)
  x.cuts[numints+1]<-x.cuts[numints+1]*1.001
  
  
  grid.density<-base::matrix(0,nrow=numints,ncol=numints)
  
  for(j in 1:numints)
  {
    for(k in 1:numints)
    {
      grid.density[j,k]<-base::sum(1*(yvect>=y.cuts[j] & yvect<y.cuts[j+1] & xvect >=x.cuts[k] & xvect<x.cuts[k+1]), na.rm=TRUE)
    }
  }
  base::print(base::length(x.mids))
  base::print(base::length(y.mids))
  
  #grid.density.obj<-data.frame(base::matrix(grid.density),as.vector(x.mids),as.vector(y.mids))
  grid.density.obj<-base::cbind(grid.density,x.mids,y.mids)
  
  # return(grid.density.obj)
}
