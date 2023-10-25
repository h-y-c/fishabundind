#' plot.fishai function
#' Plot annual abundance index
#'
#' @param df A dataset contains a variable of year and a variable of abundanc indices.
#' @param ... further arguments passed to or from other methods.
#' @param Year A variable of Year of data collection.
#' @param AbundIndex A variable of annual abundance indices.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return This function returns a graph of the annual abundance indices time series.
#'
#' @rdname plot.fishai
#' @export
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
#'  plot(aiQ,Year=Year,AbundIndex=AbundIndex)
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
#'  plot(aiX,Year=Year,AbundIndex=AbundIndex)
#'
#' }
#'

# plot.fishai.default<-function(df, ... ,Year=NULL,AbundIndex=NULL){
#   fishdf<-df%>%dplyr::rename(Year=Year,AbundIndex=AbundIndex)
#   ggplot(data=fishdf, aes(x=Year, y=AbundIndex, group=1)) +
#     geom_line()+
#     geom_point()+
#     theme_bw()  +
#     ylab("Abundance Index") +
#     xlab("Year")
# }

plot.fishai<-function(df, ... ,Year=NULL,AbundIndex=NULL){
  fishdf<-df%>%dplyr::rename(Year=Year,AbundIndex=AbundIndex)
  ggplot(data=fishdf, aes(x=Year, y=AbundIndex, group=1)) +
    geom_line()+
    geom_point()+
    theme_bw()  +
    ylab("Abundance Index") +
    xlab("Year")
}

