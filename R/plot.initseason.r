#' plot.initseason function
#' Plot initial weeks
#' 
#' @param df A dataset contains a variable of year and a variable of initial week.
#' @param Year A variable of Year of data collection.
#' @param first_wk A variable of estimated first week of the season.
#'   
#' @import dplyr
#' @import ggplot2
#'  
#' @return This function returns a graph of the initial week time series.
#'  
#' @export
#' @examples
#' \dontrun{
#'  seasonQ<-fishinitseasonEst(dataset=fishQ,Year="yr",
#'                                 Week="wk",
#'                                 RegionName="regname",
#'                                 Strata="stra",
#'                                 StrataVolume="stravol",
#'                                 RegionVolume="regvol",
#'                                 VolumeSample="volsamp",
#'                                 N_individual="nind",
#'                                 first_thr=0.05,durationEst=7
#'  )
#'  plot.initseason(seasonQ)
#'  
#'  }

plot.initseason<-function(df,Year=NULL,first_wk=NULL){
  fishinitseasondf<-df%>%dplyr::rename(Year=Year,first_wk=first_wk)
  ggplot(data=fishinitseasondf, aes(x=Year, y=first_wk, group=1)) +
    geom_line()+
    geom_point()+
    theme_bw()  +
    ylab("Initial Week") +
    xlab("Year")
}


