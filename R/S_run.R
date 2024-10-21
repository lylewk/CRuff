#' @title Sjøvold (1990) stature estimation
#' @description Does plot of estimated statures against modified Fully statures
#' for Ruff's European data.  The analyzed data can be from statures estimated from
#' skeletal height and "partial" skeletal height, just skeletal height, or skeletal
#' and "partial" skeletal height for just Iberian females.
#' @param i.which Character indicating which long bone to use.  Possible values are
#' "Fem" for maximum femur length, "Tib" for maximum tibia length, "Hum" for maximum
#' humerus length, or "Rad" for maximum radius length.  Default: "Fem"
#' @param which.dat Integer indicating which data to use. Possible values are 1 for
#' "Europe" data, 2 for "Europe2" data, or any other integer for "Iberia". Default=1
#' @param area Decimal value giving the proportion for the prediction interval.  Value
#' must be greater than 0.0 and less than 1.0. Default = 0.5
#' @param which.parms Integer indicating whether to use Table 1 or Table 2 from
#' Sjøvold (1990).  Value of 1 uses Table 1 while any other integer uses Table 2.
#' Table 1 is "Caucasians independent of sex" while Table 2 is "all ethnic groups
#' independent of sex."  Default value: 1.
#' @details Estimates statures using \link{Europe} data with modified Fully statures
#' from skeletal height or partial skeletal height, \link{Europe2} data set from
#' skeletal height only, or \link{Iberia} subset of "Europe" that only includes Iberian females.
#' The regression equations are from Sjøvold (1990) Table 1 or 2.
#' @return Does a plot of statures estimated from the  (2016) regression
#' equations against the (modified) Fully statures.  Also returns the overall bias and
#' the number of total cases that are below the lower bounds of the prediction intervals,
#' within the prediction intervals, and above the upper bound for the prediction
#' intervals.  These measures (bias and classification relative to prediction interval
#' bounds) are also given for the lower quartile of the data (by modified Fully stature),
#' upper quartile, and middle 50 percent of the data.
#' @references
#' Sjøvold, T. (1990). Estimation of stature from long bones utilizing
#' the line of organic correlation. \emph{Human Evolution}, 5, 431-447.
#'
#' @examples
#' # Default using femur, European data, and area = 0.5, and Table 1
#' # "Caucasians independent of sex"
#'   S_run()
#' # "all ethnic groups independent of sex"
#'   S_run(which.parms=2)
#' # Using tibia, Iberian female data, and default area = 0.5
#'   A_run(i.which='Tib',which.dat=3)
#' @export

S_run=function(i.which='Fem',which.dat=1,area=0.5,which.parms=1){
  if(which.dat==1) data(Europe)
  else if(which.dat==2){data(Europe2)}
  else(data(Iberia))
  bones=c('Fem','Tib','Hum','Rad')
  Row = which(i.which==bones)
  if(which.parms==1) parms=S_reg[Row,]
  else{parms=S2_reg[Row,]}
  if(which.dat==1) dat = Europe[,c(7,7+Row)]
  else if(which.dat==2){dat = Europe2[,c(7,7+Row)]}
  else {dat=Iberia[,c(1,1+Row)]}
  run_it(parms,dat,i.which,'Sjovold (1990),',area)
}
