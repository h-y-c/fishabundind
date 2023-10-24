#' fishinitseasonEst function
#' Initial week of the proposed season
#' 
#' @inheritParams fishai 
#'   
#' @import dplyr
#'  
#' @return This function returns a dataframe which includes a variable of year and
#'  a variable of estimated initial week of the proposed season
#' 
#' @details The data were assumed collected from a fishery-independent survey following 
#'  a stratified random sampling design with i regions within the sampling area and 
#'  s strata within a given region. The survey was conducted over week for each year. 
#'  The initial week of the proposed season is estimated as the first week of the year when  
#'  the cumulative weekly density estimates surpass \code{first_thr}*100% of 
#'  the total densities observed across all weeks of sampling.
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
#'  seasonQ
#'  
#' }
#'  

fishinitseasonEst<-function(dataset,
                 Year,Week,RegionName,Strata,
                 StrataVolume,RegionVolume,VolumeSample,N_individual,
                 first_thr=0.05,durationEst=8){
  
  if(!is.data.frame(dataset)){stop("dataset is not a data.frame object",call.=FALSE)}
  
  first_thr <- first_thr
  durationEst<-durationEst
  
  if(!is.numeric(first_thr)|first_thr<0|first_thr>1|!is.numeric(durationEst)|durationEst<=0){
    stop("incorrect first_thr or durationEst")
  }
  
  
  suppressWarnings(
    suppressMessages(
      
      fishdat<-dataset%>%
        dplyr::rename(Year=Year,
                      Week=Week,
                      RegionName=RegionName,
                      Strata=Strata,
                      StrataVolume=StrataVolume,
                      RegionVolume=RegionVolume,
                      VolumeSample=VolumeSample,
                      N_individual=N_individual
        )

    )# suppressMessages
  )#suppressWarnings
  

  suppressWarnings(
    suppressMessages(

      first_wk_df<-fishdat%>%
        group_by(Year,Week)%>%
        summarise(
          wk_n=sum(N_individual,na.rm=TRUE),
          wk_volume=sum(VolumeSample,na.rm=TRUE))%>%
        arrange(Year,Week)%>%
        mutate(
          den=wk_n/wk_volume,
          den_cumsum=cumsum(wk_n/wk_volume)
        )%>%
        group_by(Year)%>%
        summarise(
          wk_sum_den=sum(den,na.rm=TRUE),
          Week=Week,
          wk_n=wk_n,
          wk_volume=wk_volume,
          den=den,
          den_cumsum=den_cumsum
        )%>%
        mutate(
          thr_sum=wk_sum_den*first_thr
        )%>%
        dplyr::select(-wk_sum_den)%>%
        group_by(Year)%>%
        summarise(
          first_wk=min(Week[which(den_cumsum>=thr_sum)])
        )%>%ungroup()%>%as.data.frame()
      
    )# suppressMessages
  )#suppressWarnings
  
  
  return(first_wk_df)
  
}# end of funnction

