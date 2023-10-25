#' plotfishai function
#' Plot annual abundance index#'
#'
#' @name plotfishai
#'
#' @param df A dataset contains a variable of year and a variable of abundance indices.
#' @param ... arguments passed to other methods
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
plotfishai <- function(df, ...) {
  UseMethod("plotfishai")
}

#' @param Year A variable of Year of data collection.
#' @param AbundIndex A variable of annual abundance indices.
#'
#' @return This function returns a graph of the abundance indices time series.
#'
#' @rdname plotfishai
#' @export
#'
#' @examples
#' \dontrun{
#'  aiQ<-fishai(dataset=fishQ,Year="yr",
#'             Week="wk",
#'             RegionName="regname",
#'             Strata="stra",
#'             StrataVolume="stravol",
#'             RegionVolume="regvol",
#'             VolumeSample="volsamp",
#'             N_individual="nind",
#'             SeasonEst=TRUE,first_thr=0.05,durationEst=7)
#'  plotfishai(aiQ)
#'
#'  aiX<-fishai(dataset=fishX,Year="yr",
#'             Week="wk",
#'             RegionName="regname",
#'             Strata="stra",
#'             StrataVolume="stravol",
#'             RegionVolume="regvol",
#'             VolumeSample="volsamp",
#'             N_individual="nind",
#'             SeasonEst=FALSE)
#'  plotfishai(aiX,Year=Year,AbundIndex=AbundIndex)
#'
#' }
#'
plotfishai.default<-function(df, ... ,Year=NULL,AbundIndex=NULL){
  fishdf<-df%>%dplyr::rename(Year=Year,AbundIndex=AbundIndex)
  pai<-ggplot(data=fishdf, aes(x=Year, y=AbundIndex, group=1)) +
    geom_line()+
    geom_point()+
    theme_bw()  +
    ylab("Abundance Index") +
    xlab("Year")
  class(pai)<-"plotfishai"
  return(pai)
}




