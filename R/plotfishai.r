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
#' @param Year A variable of Year of data collection.
#' @param AbundIndex A variable of annual abundance indices.
#' @param se Logical. Standard error will be plotted out if TRUE.
#'   the default is TRUE.
#'
#' @return This function returns a graph of the abundance indices time series.
#'
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
plotfishai<-function(df, ... ,Year=NULL,AbundIndex=NULL,se=TRUE){
  if(!is.logical(se)){stop("se should be logical (TRUE or FALSE)",call.=FALSE)}
  if(se){
    fishdf<-df%>%dplyr::rename(Year=Year,AbundIndex=AbundIndex)%>%
      mutate(serr=sqrt(var))
    pai<-
    ggplot(data=fishdf, aes(x=Year, y=AbundIndex, group=1)) +
      geom_line()+
      geom_point()+
      geom_ribbon(aes(ymin = AbundIndex-serr, ymax = AbundIndex+serr), alpha = 0.3) +
    theme_bw()  +
      ylab("Abundance Index") +
      xlab("Year")
    return(pai)
  }else{
    fishdf<-df%>%dplyr::rename(Year=Year,AbundIndex=AbundIndex)
    pai<-
    ggplot(data=fishdf, aes(x=Year, y=AbundIndex, group=1)) +
      geom_line()+
      geom_point()+
      theme_bw()  +
      ylab("Abundance Index") +
      xlab("Year")
    return(pai)
  }

}








