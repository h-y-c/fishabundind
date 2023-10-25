#' plotinitseason function
#' Plot initial weeks
#'
#' @name plotinitseason
#'
#' @param df A dataset contains a variable of year and a variable of initial week.
#' @param ... arguments passed to other methods
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
plotinitseason <- function(df, ...) {
  UseMethod("plotinitseason")
}

#' @param Year A variable of Year of data collection.
#' @param first_wk A variable of estimated first week of the season.
#'
#' @return This function returns a graph of the initial week time series.
#'
#' @rdname plotinitseason
#' @export
#'
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
#'  plotinitseason(seasonQ,Year=Year,first_wk=first_wk)
#'
#'  }

plotinitseason.default<-function(df, ... ,Year=NULL,first_wk=NULL){
  fishinitseasondf<-df%>%dplyr::rename(Year=Year,first_wk=first_wk)
  pinit<-ggplot(data=fishinitseasondf, aes(x=Year, y=first_wk, group=1)) +
    geom_line()+
    geom_point()+
    theme_bw()  +
    ylab("Initial Week") +
    xlab("Year")
  class(pinit)<-"plotinitseason"
  return(pinit)
}












