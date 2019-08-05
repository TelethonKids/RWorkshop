#' tki_demo
#'
#' Demonstration data set for the Telethon Kids Institute Introduction to R 
#' Workshop.
#'
#' @docType data
#'
#' @usage data(tki_demo)
#'
#' @format A data table with 100 rows and 8 columns:
#' \describe{
#'   \item{\code{id}}{integer unique participant identifier}
#'   \item{\code{dob}}{dat date of birth}
#'   \item{\code{male}}{logical T - male; F - female}
#'   \item{\code{smoker}}{logical T - smoker; F - non-smoker}
#'   \item{\code{intervention}}{factor one of: Placebo, Drug 1, or Drug 2}
#'   \item{\code{day1}}{numeric measurement on day 1}
#'   \item{\code{day2}}{numeric measurement on day 2}
#'   \item{\code{day3}}{numeric measurement on day 3}
#' }
"tki_demo"

#' tki_demo_complications
#'
#' Demonstration data set for the Telethon Kids Institute Introduction to R 
#' Workshop. This second data frame is to illustrate joining data.
#'
#' @docType data
#'
#' @usage data(tki_demo_complications)
#'
#' @format A data table with 20 rows and 2 columns:
#' \describe{
#'   \item{\code{id}}{integer unique participant identifier}
#'   \item{\code{complications}}{text health complications that were experienced by the participant}
#' }
"tki_demo_complications"
