#' Konigsberg & Jantz (2018) regression data for stature estimation
#'
#' These regression parameters are obtained by classical calibration
#' using the multivariate summary statistics from Konigsberg and Jantz (2018).
#'
#' @docType data
#' @keywords datasets
#' @name K_reg
#' @usage data(K_reg)
#' @format A data frame with 4 rows and 3 columns where
#' the rows are for maximum femur, tibia, humerus, and radius lengths
#' and the columns are the regression coefficient, the regression
#' constant, and the standard error of estimate. The bone measurements
#' should be in mm. and the returned stature estimates are in mm.
#' @references
#' Konigsberg, L. W., & Meadows Jantz, L. (2018). Multivariate regression
#'  methods for the analysis of stature. In K. Latham, E. Bartelink,
#'  & M. Finnegan (Eds.), \emph{New Prespectives in Forensic Human Skeletal
#'  Identification} (pp. 87-104). Cambridge, MA: Academic Press.
"K_reg"
#' Albanese et al. (2016) regression data
#'
#' Contains the bivariate regression data for stature on long bones from
#' Albanese et al. (2016) Table 3.
#'
#' @docType data
#' @keywords datasets
#' @name A_reg
#' @usage data(A_reg)
#' @format A data frame with 4 rows and 3 columns where
#' the rows are for maximum femur, tibia, humerus, and radius lengths
#' and the columns are the regression coefficient, the regression
#' constant, and the standard error of estimate. The bone measurements
#' should be in mm. and the returned stature estimates are in mm.
#' @references
#' Albanese, J., Tuck, A., Gomes, J., & Cardoso, H. F. (2016). An alternative
#' approach for estimating stature from long bones that is not population-or
#' group-specific. \emph{Forensic Science International}, 259, 59-68.
"A_reg"
#' Sjøvold (1990) regression data
#'
#' Contains the bivariate regression (RMA) data for stature on long bones
#' from Sjøvold (1990) Table 1.  Note: This is for "Caucasians" independent
#' of sex
#'
#' @docType data
#' @keywords datasets
#' @name S_reg
#' @usage data(S_reg)
#' @format A data frame with 4 rows and 3 columns where
#' the rows are for maximum femur, tibia, humerus, and radius lengths
#' and the columns are the regression coefficient, the regression
#' constant, and the standard error of estimate. The bone measurements
#' should be in mm. and the returned stature estimates are in mm.
#' @references
#' Sjøvold, T. (1990). Estimation of stature from long bones utilizing
#' the line of organic correlation. \emph{Human Evolution}, 5, 431-447.
"S_reg"
#' Sjøvold (1990) regression data
#'
#' Contains the bivariate regression (RMA) data for stature on long bones
#' from Sjøvold (1990) Table 2.  Note: This is for "all ethnic groups independent
#' of sex
#'
#' @docType data
#' @keywords datasets
#' @name S2_reg
#' @usage data(S2_reg)
#' @format A data frame with 4 rows and 3 columns where
#' the rows are for maximum femur, tibia, humerus, and radius lengths
#' and the columns are the regression coefficient, the regression
#' constant, and the standard error of estimate. The bone measurements
#' should be in mm. and the returned stature estimates are in mm.
#' @references
#' Sjøvold, T. (1990). Estimation of stature from long bones utilizing
#' the line of organic correlation. \emph{Human Evolution}, 5, 431-447.
"S2_reg"
#' Europe data
#'
#' A dataset of Europeans.  The data come from Ruff (2018).  Statures were
#' estimated using modifications of the Fully method (Niskanen & Ruff 2018; Raxter et
#' al. 2006, 2007).
#'
#' @details Data as follows:
#' \itemize{
#'  \item{Region:}{  Geographic region}
#'  \item{Subregion:}{  Geographic subregion}
#'  \item{Site:}{  Site}
#'  \item{ID:}{  Unique identifier}
#'  \item{Museum:}{  Museum}
#'  \item{Sex:}{  Sex as a factor, either "Female" or "Male"}
#'  \item{Stat:}{  Stature in mm.}
#'  \item{Fem:}{  Maximum femur length in mm.}
#'  \item{Tib:}{  Maximum tibia length in mm.}
#'  \item{Hum:}{  Maximum humerus length in mm.}
#'  \item{Rad:}{  Maximum radius length in mm.}
#' }
#' @docType data
#' @keywords datasets
#' @name Europe
#' @usage data(Europe)
#' @format A data frame with 1062 rows and 11 variables
#' @source{
#' See "European data set: Excel" and "Notes" under "Special Projects"
#' \url{https://fae.johnshopkins.edu/chris-ruff/}
#' }
#' @references
#' Niskanen, M. and Ruff, C.B. (2018) Body size and shape reconstruction. In Ruff CB, editor.
#' \emph{Skeletal Variation and Adaptation in Europeans: Upper Paleolithic to the Twentieth Century}.
#' Hoboken: Wiley-Blackwell, pp. 15-37.
#'
#' Raxter, M.H., Auerbach, B.M. and Ruff, C.B. (2006) A revision of the Fully technique
#' for estimating statures. \emph{Am. J. Phys. Anthropol}. 130, 374-384.
#'
#' Raxter, M.H., Ruff, C.B. and Auerbach, B.M. (2007) Technical note: Revised Fully
#' stature estimation technique. \emph{Am. J. Phys. Anthropol}. 133, 817-818.
#'
#' Ruff, C.B. (ed.) (2018) \emph{Skeletal variation and adaptation in Europeans:
#' Upper Paleolithic to the Twentieth Century}. Hoboken: Wiley Blackwell.
"Europe"
#' Variance-covariance matrix
#'
#' This data is the variance-covariance matrix among stature and maximum femur,
#' tibia, humerus, and radius length (all in mm.) from Konigsberg and Jantz (2018).
#'
#' @docType data
#' @keywords datasets
#' @name V.2015
#' @usage data(V.2015)
#' @format A 5 by 5 matrix
#' @references
#' Konigsberg, L. W., & Meadows Jantz, L. (2018). Multivariate regression
#'  methods for the analysis of stature. In K. Latham, E. Bartelink,
#'  & M. Finnegan (Eds.), \emph{New Perspectives in Forensic Human Skeletal
#'  Identification} (pp. 87-104). Cambridge, MA: Academic Press.
"V.2015"
#' Vector of means
#'
#' This data is a row vector of means for stature, maximum femur,
#' tibia, humerus, and radius length (all in mm.) from Konigsberg and Jantz (2018).
#'
#' @docType data
#' @keywords datasets
#' @name mu
#' @usage data(mu)
#' @format A 1 by 5 row vector
#' @references
#' Konigsberg, L. W., & Meadows Jantz, L. (2018). Multivariate regression
#'  methods for the analysis of stature. In K. Latham, E. Bartelink,
#'  & M. Finnegan (Eds.), \emph{New Perspectives in Forensic Human Skeletal
#'  Identification} (pp. 87-104). Cambridge, MA: Academic Press.
"mu"
#' short
#'
#' @details This is a subset of the "Europe" data frame that includes only
#' Iberian, French, and Italian females.
#' Data is as follows:
#' \itemize{
#'  \item{Stat:}{  Stature in mm.}
#'  \item{Fem:}{  Maximum femur length in mm.}
#'  \item{Tib:}{  Maximum tibia length in mm.}
#'  \item{Hum:}{  Maximum humerus length in mm.}
#'  \item{Rad:}{  Maximum radius length in mm.}
#' }
#' @docType data
#' @keywords datasets
#' @name short
#' @usage data(short)
#' @format A 5 column by 139 row data frame.
"tall"
#' tall
#'
#' @details This is a subset of the "Europe" data frame that includes only males
#' from the Balkans, Scandinavia, or Finland.
#' Data is as follows:
#' \itemize{
#'  \item{Stat:}{  Stature in mm.}
#'  \item{Fem:}{  Maximum femur length in mm.}
#'  \item{Tib:}{  Maximum tibia length in mm.}
#'  \item{Hum:}{  Maximum humerus length in mm.}
#'  \item{Rad:}{  Maximum radius length in mm.}
#' }
#' @docType data
#' @keywords datasets
#' @name tall
#' @usage data(tall)
#' @format A 5 column by 179 row data frame.
"tall"
