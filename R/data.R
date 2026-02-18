#' Processed Foster Care & Crime Data
#'
#' A processed subset of data derived from the study "Crime during the transition to
#' adulthood" (Courtney & Cusick, 2010). This dataset is used to demonstrate
#' the Marginalized Zero-Inflated Model (MZIM) and has undergone specific
#' preprocessing steps including variable selection and listwise deletion.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{ID}{Unique participant identifier.}
#'   \item{Gender}{Participant gender (Recoded from original: 0/1).}
#'   \item{School}{School enrollment status at Wave 1 (Recoded from original `INSCHOOL_W1`).}
#'   \item{ABUSESUM}{Count of abuse incidents (Response Variable; derived from `ABUSESUM_W1`).}
#' }
#'
#' @details
#' This dataset was created by subsetting the original data to focus on specific demographic
#' and outcome variables. Preprocessing steps included:
#' \itemize{
#'   \item Selection of relevant variables (`GENDER`, `INSCHOOL_W1`, `ABUSESUM_W1`).
#'   \item Recoding of missing values (e.g., -9 codes converted to NA).
#'   \item Listwise deletion of observations with missing data in the selected variables.
#'   \item Renaming columns for clarity in model demonstration.
#' }
#'
#' @source
#' Courtney, M. E., & Cusick, G. R. (2010). Crime during the transition to adulthood:
#' How youth fare as they leave out-of-home care in Illinois, Iowa, and Wisconsin, 2002–2007.
#' ICPSR25981-v1. Ann Arbor, MI: Inter-university Consortium for Political and Social Research.
#'
#' @references
#' Courtney, M. E., & Cusick, G. R. (2010). Crime during the transition to adulthood:
#' How youth fare as they leave out-of-home care in Illinois, Iowa, and Wisconsin, 2002–2007.
#'
"abuse"
