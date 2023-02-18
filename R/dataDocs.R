#' Experimental data from the job training program first studied by LaLonde (1986)
#'
#' A dataset of units in an experimental evaluation of a job training
#' program. Subset to those units with two years of pre-treatment income data.
#' \itemize{
#'   \item \code{age} - age in years.
#'   \item \code{education} - number of years of schooling.
#'   \item \code{black} - 1 if black, 0 otherwise.
#'   \item \code{hispanic} - 1 if Hispanic, 0 otherwise.
#'   \item \code{married} - 1 if married, 0 otherwise.
#'   \item \code{nodegree} - 1 if no high school degree, 0 otherwise.
#'   \item \code{re74} - earnings ($) in 1974.
#'   \item \code{re75} - earnings ($) in 1975.
#'   \item \code{re78} - earnings ($) in 1978.
#'   \item \code{u74} - 1 if unemployed in 1974, 0 otherwise.
#'   \item \code{u75} - 1 if unemployed in 1975, 0 otherwise.
#'   \item \code{treat} - 1 if treated, 0 otherwise.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name lalonde.exp
#' @usage data(lalonde.exp)
#' @format A data frame with 445 rows and 12 variables
#' @references LaLonde, Robert J. (1986). Evaluating the Econometric
#' Evaluations of Training Programs with Experimental Data. The
#' American Economic Review, 76(4), 604--620.
NULL
######################################################################
#' Non-experimental data from Lalonde (1986)
#'
#' A dataset of experimental treated units and non-experimental
#' control units from the Panel Study of Income Dynamics (PSID).
#'
#' \itemize{
#'   \item \code{age} - age in years.
#'   \item \code{education} - number of years of schooling.
#'   \item \code{black} - 1 if black, 0 otherwise.
#'   \item \code{hispanic} - 1 if Hispanic, 0 otherwise.
#'   \item \code{married} - 1 if married, 0 otherwise.
#'   \item \code{nodegree} - 1 if no high school degree, 0 otherwise.
#'   \item \code{re74} - earnings ($) in 1974.
#'   \item \code{re75} - earnings ($) in 1975.
#'   \item \code{re78} - earnings ($) in 1978.
#'   \item \code{u74} - 1 if unemployed in 1974, 0 otherwise.
#'   \item \code{u75} - 1 if unemployed in 1975, 0 otherwise.
#'   \item \code{treat} - 1 if treated, 0 otherwise.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name lalonde.psid
#' @usage data(lalonde.psid)
#' @format A data frame with 2675 rows and 12 variables
#' @references LaLonde, Robert J. (1986). Evaluating the Econometric
#' Evaluations of Training Programs with Experimental Data. The
#' American Economic Review, 76(4), 604--620.
NULL
######################################################################
#' Public use wage, education data from 1980 census.
#' Used in Mostly Harmless for Mincer Regression.
#'
#' \itemize{
#'   \item \code{lwklywge} - Log Wage
#'   \item \code{educ} - number of years of schooling.
#'   \item \code{yob} - year of birth
#'   \item \code{qob} - quarter of birth
#'   \item \code{pob} - place of birth (state)
#'   \item \code{age} - age in 1980
#' }
#'
#' @docType data
#' @name pums
#' @usage data(pums)
#' @format A data frame with 329,509 rows and 6 variables
NULL
######################################################################
#' California proposition 99
#'
#' A dataset containing per-capita cigarette consumption (in packs).
#' In year 1989 California imposed a Tobacco tax. The column `treated` is 1 from then on for California.
#'
#' @docType data
#' @name california_prop99
#' @format A data frame with 1209 rows and 4 variables:
#' \describe{
#'   \item{State}{US state name, character string}
#'   \item{Year}{Year, integer}
#'   \item{PacksPerCapita}{per-capita cigarette consumption, numeric}
#'   \item{treated}{the treatmed indicator 0: control, 1: treated, numeric}
#' }
#' @source Abadie, Alberto, Alexis Diamond, and Jens Hainmueller.
#'  "Synthetic control methods for comparative case studies: Estimating the effect of California’s tobacco control program."
#'   Journal of the American statistical Association 105, no. 490 (2010): 493-505.
#' @usage data(california_prop99)
NULL
