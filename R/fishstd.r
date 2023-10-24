#' fishstd function
#' Averaged weekly density for each region and each year
#' 
#' @inheritParams fishai 
#'   
#' @import dplyr
#'  
#' @return This function returns a dataframe which includes averaged weekly density
#'  for each region and each year
#' 
#' @details The data were assumed collected from a fishery-independent survey following 
#'  a stratified random sampling design with i regions within the sampling area and 
#'  s strata within a given region. The survey was conducted over week for each year. 
#'  The averaged weekly density for each region and each year is estimated using the average density, 
#'  with the stratum densities weighted by the proportion of the regional river volume found in the stratum:
#'  \code{density} = \deqn{\mean{\mean{\frac{\code{N_individual}}{\code{VolumeSample}}*\frac{\code{StrataVolume}}{\code{RegionVolume}}}}}
#'  
#'  
#' @export
#' @examples
#' \dontrun{
#'  stdQ<-fishstd(dataset=fishQ,Year="yr",
#'               Week="wk",
#'               RegionName="regname",
#'               Strata="stra",
#'               StrataVolume="stravol",
#'               RegionVolume="regvol",
#'               VolumeSample="volsamp",
#'               N_individual="nind",
#'               SeasonEst=TRUE,first_thr=0.05,durationEst=7
#'  )
#'  stdQ
#'  
#'  stdX<-fishstd(dataset=fishX,Year="yr",
#'               Week="wk",
#'               RegionName="regname",
#'               Strata="stra",
#'               StrataVolume="stravol",
#'               RegionVolume="regvol",
#'               VolumeSample="volsamp",
#'               N_individual="nind",
#'               SeasonEst=FALSE
#'  )
#'  stdX
#'  
#' }
#'  

fishstd<-function(dataset,
                 Year,Week,RegionName,Strata,
                 StrataVolume,RegionVolume,VolumeSample,N_individual,
                 SeasonEst=TRUE,first_thr=NULL,durationEst=NULL){
  
  if(!is.data.frame(dataset)){stop("dataset is not a data.frame object",call.=FALSE)}
  
  SeasonEst<-SeasonEst
  first_thr<-first_thr
  durationEst<-durationEst

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
  
  
  if(!is.logical(SeasonEst)){stop("SeasonEst should be logical (TRUE or FALSE)",call.=FALSE)}
  if(SeasonEst){
    if(!is.numeric(first_thr)|first_thr<0|first_thr>1|!is.numeric(durationEst)|durationEst<=0){
      stop("incorrect first_thr or durationEst")
    }
    if(!is.numeric(fishdat$Week)){
      stop("incorrect week data")
    }
  }else{
    warning("No input for first_thr and durationEst. All data are used",call.=TRUE)
  }
  
  suppressWarnings(
    suppressMessages(
      
      vol_wk_sum<-fishdat%>%
        dplyr::select(Year, 
                      RegionName,
                      Week,
                      RegionVolume)%>%
        unique()%>%
        group_by(Year, 
                 RegionName, 
                 Week)%>%
        summarise(VolSum=sum(RegionVolume,na.rm=TRUE))
      
    )# suppressMessages
  )#suppressWarnings
  

  if(SeasonEst){
    
    suppressWarnings(
      suppressMessages(
        
        first_wk_df<-fishdat%>%
          group_by(Year,Week)%>%
          summarise(
            wk_n=sum(N_individual,na.rm=TRUE),
            wk_volume=sum(VolumeSample,na.rm=TRUE))%>%
          arrange(Year,Week)%>%
          mutate (
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
          ) %>% 
          dplyr::select(-wk_sum_den)%>%
          group_by(Year)%>%
          summarise(
            first_wk=min(Week[which(den_cumsum>=thr_sum)])
          )
        
      )# suppressMessages
    )#suppressWarnings
    
    foo<-wks_df<-list()
    for(i in 1:length(first_wk_df$Year)){
      foo[[i]]<-fishdat[which(fishdat$Year%in%first_wk_df$Year[i]),]
      wks_df[[i]]<-foo[[i]][which(foo[[i]]$Week%in%c(first_wk_df$first_wk[i]:(first_wk_df$first_wk[i]+durationEst))),]
    }
    foo_wks<-do.call(rbind,wks_df)
  }else{
    foo_wks<-fishdat
  }

  
  suppressWarnings(
    suppressMessages(
      
      yr_rg_df<-foo_wks%>% 
        mutate(den_ls_i=ifelse((N_individual/VolumeSample)%in%Inf,NA,(N_individual/VolumeSample)))%>% 
        group_by(Year, 
                 RegionName, 
                 Week, 
                 Strata)%>% 
        summarise(StrataVolume=StrataVolume,
                  wk_region_strata_ls_den=mean(den_ls_i,na.rm=TRUE))%>%
        unique()%>%
        left_join(vol_wk_sum)%>%
        group_by(Year, 
                 RegionName, 
                 Week) %>%
        summarise(
          wk_region_ls_den=sum((wk_region_strata_ls_den*StrataVolume)/mean(unique(VolSum),na.rm=TRUE)))%>%
        group_by(Year, 
                 RegionName)%>%
        summarise(region_ls_den=mean(wk_region_ls_den,na.rm=TRUE)) %>%
        left_join(
          data.frame(Year=rep(c(min(fishdat$Year,na.rm=TRUE):max(fishdat$Year,na.rm=TRUE)),each=length(unique(fishdat$RegionName))),
                     RegionName=rep(unique(fishdat$RegionName),length(c(min(fishdat$Year,na.rm=TRUE):max(fishdat$Year,na.rm=TRUE)))))
        )%>%
        mutate(Year=as.character(Year))%>%
        reshape2::dcast(RegionName~Year)%>%
        # slice(match(rev(RegionName),RegionName))%>%
        `rownames<-`(.[,1])%>%select(-RegionName) 
      
    )# suppressMessages
  )#suppressWarnings
  
  return(yr_rg_df)
  
}# end of funnction

