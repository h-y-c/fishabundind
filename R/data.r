#' Hypothetical fish survey data for species Q
#'
#' The data are assumed collected from a fishery-independent survey following 
#'  a stratified random sampling design with regions within the sampling area and 
#'  strata within a given region. The survey was conducted over week for each year. 
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{yr}{Year of data collection.}
#'  \item{wk}{Week of data collection.}
#'  \item{TowID}{ID of a tow.}
#'  \item{regname}{Names of regions.}
#'  \item{stra}{Strata code.}
#'  \item{stravol}{Volume of the strata of sample collection.}
#'  \item{regvol}{Volume of the region of sample collection.}
#'  \item{volsamp}{Volume of water sampled of a tow.}
#'  \item{nind}{Number of individuals (fish) sampled of a tow.}
#'  \item{spe}{Species name.}
#' 
#' @examples
#' \dontrun{
#'  fishQ
#' }
"fishQ"

#' Hypothetical fish survey data for species X
#'
#' @examples
#' \dontrun{
#'  fishX
#' }
"fishX"
