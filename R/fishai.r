#' fishai function
#' Estimate annual abundance index
#'
#' @param dataset A dataset contains the input data.
#' This dataset must include the following variables.
#' @param Year A variable of Year of data collection.
#' @param Week A variable of calender week of data collection.
#' @param RegionName A variable of names of regions where data were collected.
#' @param Strata A variable of strata within the corresponding \code{RegionName} where data were collected.
#' @param StrataVolume A variable of volume of the corresponding \code{Strata} where data were collected.
#'  Depending on the sampling design, it could be area of a strata.
#' @param RegionVolume A variable of volume of the corresponding region where data were collected.
#'  Depending on the sampling design, it could be area of a region.
#' @param VolumeSample A variable of volume of water sampled.
#'  Depending on the sampling design, it could be time of tow duration or area swept.
#' @param N_individual A variable of number of individual sampled.
#' @param SeasonEst Logical. Should seasonality be considered? Some speices or some life stages
#'  would have prominent peak season, especially for eggs and larvae.
#'  If \code{TRUE}, \code{first_thr} and \code{durationEst} must be specified.
#'  If \code{FALSE}, all data will be used.
#'  The default value is \code{TRUE}.
#' @param first_thr A numeric value in between 0 and 1.
#'  It is used to determine the first week of the year in which
#'  the cumulative weekly density estimates surpass \code{first_thr}*100% of
#'  the total densities observed across all weeks of sampling.
#' @param durationEst Integer (>0).
#'  The estimated duration of weeks the season will last.
#'  For example, if the season is estimated to end 8 weeks after
#'  the estimated first week (see \code{first_thr}), then the input should be 8.
#'
#' @import dplyr
#'
#' @return This function returns a dataframe which includes a variable of year,
#'  a variable of annual abundance indeices, and a variable of variance
#'
#' @details The data are assumed collected from a fishery-independent survey following
#'  a stratified random sampling design with i regions within the sampling area and
#'  s strata within a given region. The survey was conducted over week for each year.
#'  The design-based annual abundance index for each year is estimated using the average density:
#'  \code{AbundInd} = \deqn{\sum{\frac{\sum{\sum{\code{StrataVolume}*\frac{\sum{\code{N_individual}}}{\sum{\code{VolumeSample}}}}}}{\sum{\code{RegionVolume}}}}}
#'
#' @references Chang, H.-Y., Sun, M., Rokosz, K., and Chen, Y.. (2023)
#'  Evaluating effects of changing sampling protocol for a long-term Ichthyoplankton monitoring program.
#'  Frontiers in Marine Science. DOI: 10.3389/fmars.2023.1237549
#'
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
#'  aiQ
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
#'  aiX
#'
#' }
#'


fishai<-function(dataset,
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

      vol_sum<-fishdat%>%
        dplyr::select(Year,
                      RegionName,
                      RegionVolume)%>%
        unique()%>%
        group_by(Year)%>%
        summarise(VolSum=sum(RegionVolume,na.rm=TRUE))%>%
        ungroup()%>%as.data.frame()


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


    foo<-wks_df<-list()
    for(i in 1:length(first_wk_df$Year)){
      foo[[i]]<-fishdat[which(fishdat$Year%in%first_wk_df$Year[i]), ]
      wks_df[[i]]<-foo[[i]][which(foo[[i]]$Week%in%c(first_wk_df$first_wk[i]:(first_wk_df$first_wk[i]+durationEst))),]
    }
    foo_wks<-do.call(rbind,wks_df)
  }else{
    foo_wks<-fishdat
  }


  suppressWarnings(
    suppressMessages(

      var_df<-foo_wks%>%
        group_by(Year,
                 RegionName,
                 Week,
                 Strata)%>%
        summarise(mean_ls=mean(N_individual,na.rm=TRUE),
                  n_obs=n(),
                  wk_volume=sum(VolumeSample,na.rm=TRUE)
        )%>%dplyr::filter(wk_volume>0)%>%
        right_join(foo_wks)%>%
        group_by(Year,
                 RegionName,
                 Week,
                 Strata)%>%
        summarise(
          sse=((n_obs*sum(((N_individual-mean_ls)^2)/(n_obs-1),na.rm=TRUE))/(wk_volume)^2),
          StrataVolume=StrataVolume,
          RegionVolume=RegionVolume
        )%>%unique()%>%
        group_by(Year,
                 Week) %>%
        summarise(
          temp=sum(((StrataVolume^2)*sse)/(sum(unique(RegionVolume),na.rm=TRUE)^2),na.rm=TRUE)
        )%>%group_by(Year)%>%
        summarise(var=sum(temp,na.rm=TRUE))%>%
        arrange(Year)%>%ungroup()%>%as.data.frame()

    )# suppressMessages
  )#suppressWarnings



  suppressWarnings(
    suppressMessages(

      yr_abund_df<-foo_wks%>%
        group_by(Year,
                 RegionName,
                 Week,
                 Strata)  %>%
        summarise(
          StrataVolume=StrataVolume,
          RegionVolume=RegionVolume,
          wk_n_ls=sum(N_individual,na.rm=TRUE),
          wk_volume=sum(VolumeSample,na.rm=TRUE)) %>%
        mutate(wk_volume=ifelse(wk_volume%in%0,NA,wk_volume))%>%
        unique()%>%
        mutate(ls_den=ifelse((wk_n_ls/wk_volume)%in%Inf,NA,wk_n_ls/wk_volume)) %>%
        left_join(vol_sum)%>%
        group_by(Year)%>%
        summarise(
          AbundIndex=sum((ls_den*StrataVolume)/mean(unique(VolSum),na.rm=TRUE),na.rm=TRUE)
        )%>%
        arrange(Year)%>%ungroup()%>%as.data.frame()%>%
        full_join(var_df)

    )# suppressMessages
  )#suppressWarnings

  return(yr_abund_df)

}# end of funnction



