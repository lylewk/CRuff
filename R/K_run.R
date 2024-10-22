#' @title Konigsberg & Jantz (2018) stature regressions
#' @description Does plot of estimated statures against modified Fully statures
#' for Ruff's European data.  The analyzed data can be from statures estimated from
#' skeletal height and "partial" skeletal height, just skeletal height, or skeletal
#' and "partial" skeletal height for just Iberian females.
#' @param i.which Character indicating which long bone to use.  Possible values are
#' "Fem" for maximum femur length, "Tib" for maximum tibia length, "Hum" for maximum
#' humerus length, or "Rad" for maximum radius length.  Default: "Fem"
#' @param which.dat Integer indicating which data to use. Possible values are 1 for
#' "Europe" data, 2 for "short" data, or any other integer for "tall". Default=1
#' @param area Decimal value giving the proportion for the prediction interval.  Value
#' must be greater than 0.0 and less than 1.0. Default = 0.5
#' @details Estimates statures using \link{Europe} data with modified Fully statures
#' from skeletal height or partial skeletal height, \link{short} data set from
#' short individuals, or \link{tall} for tall individuals.
#' The regression equations are from summary statistics in Konigsberg and Jantz (2018).
#' @return Does a plot of statures estimated from regressions using summary statistics
#' from Konigsberg and Jantz (2018) against the (modified) Fully statures.  The
#' equations are calculated using the (large sample) classical calibration method
#' given in Konigsberg et al. (1998).  Also returns the overall bias and the number
#' of total cases that are below the lower bounds of the prediction intervals, within
#' the prediction intervals, and above the upper bound for the prediction intervals.
#' These measures (bias and classification relative to prediction interval bounds)
#' are also given for the lower quartile of the data (by modified Fully stature),
#' upper quartile, and middle 50 percent of the data.
#' @references
#' Konigsberg, L. W., & Meadows Jantz, L. (2018). Multivariate regression
#'  methods for the analysis of stature. In K. Latham, E. Bartelink,
#'  & M. Finnegan (Eds.), \emph{New Perspectives in Forensic Human Skeletal
#'  Identification} (pp. 87-104). Cambridge, MA: Academic Press.
#'
#'  Konigsberg, L. W., Hens, S. M., Jantz, L. M., & Jungers, W. L. (1998).
#'  Stature estimation and calibration: Bayesian and maximum likelihood perspectives
#'  in physical anthropology. \emph{Yearbook of Physical Anthropology}, 41, 65-92.
#'
#' @examples
#' # Default using femur, European data, and area = 0.5
#'   K_run()
#' # Using tibia, "short" data, and default area = 0.5
#'   K_run(i.which='Tib',which.dat=2)
#' @export
K_run=function(i.which='Fem',which.dat=1,area=0.5){
  if(which.dat==1) data(Europe)
  else if(which.dat==2) data(short)
  else(data(tall))
  bones=c('Fem','Tib','Hum','Rad')
  Row = which(i.which==bones)
  parms=K_reg[Row,]
  if(which.dat==1) dat = Europe[,c(7,7+Row)]
  else if(which.dat==2) (dat=short[,c(1,1+Row)])
  else(dat=tall[,c(1,1+Row)])
  run_it(parms,dat,i.which,'Konigsberg & Jantz (2018),',area)
}
