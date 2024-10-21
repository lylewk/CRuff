#' @title Utility function to make classical calibration equations
#' @description This function takes the vector of means \link{mu} and the
#'   variance- covariance matrix \link{V.2015} from Konigsberg and Jantz (2018,
#'   Table 9.2, first sample) and calculates the intercepts, slopes (equation
#'   2), and standard error of estimates (SEEs) (square root of IMSE from Table
#'   3).
#' @return A four row by three column matrix, where the rows are for femur,
#'   tibia, humerus, and radius, and the columns are for slope, intercept, and
#'   SEE.
#' @references Konigsberg, L. W., & Meadows Jantz, L. (2018). Multivariate
#' regression methods for the analysis of stature. In K. Latham, E. Bartelink,
#'  & M. Finnegan (Eds.), \emph{New Perspectives in Forensic Human Skeletal
#'  Identification} (pp. 87-104). Cambridge, MA: Academic Press.
#'
#' Konigsberg, L. W., Hens, S. M., Jantz, L. M., & Jungers, W. L. (1998).
#' Stature estimation and calibration: Bayesian and maximum likelihood
#' perspectives in physical anthropology. \emph{Yearbook of Physical
#' Anthropology}, 41, 65-92.
#'
#' @examples
#' \dontrun{
#' # In package, output is stored in K_reg
#' make_reg()
#' }
#' @export
make_reg= function(){
  regs=matrix(NA,nr=4,nc=3)
  regs[,1]=V.2015[1,1]/V.2015[1,2:5]
  regs[,2]=mu[1]-mu[2:5]*regs[,1]
  r=vector()
  for(i in 1:4){
    r[i]=V.2015[1,i+1]/sqrt(V.2015[1,1]*V.2015[i+1,i+1])
  }
  see=vector()
  see=sqrt(V.2015[1,1]*(r^(-2)-1))
  regs[,3]=see
  return(regs)
}
