#' plot.fishstd function
#' Plot spatiotemporal distribution for each region and each year
#'
#' @param df A dataset contains density for each region and each year.
#'  Each row is a region, and each column is a year.
#' @param legend.title Title for the color key.
#' @param plot.title Title for the graph.
#'
#' @import gplots
#' @import colorRamps
#'
#' @return This function returns a heatmap showing spatiotemporal distribution over regions and years.
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
#'  plot.fishstd(df=stdQ,legend.title="Standardized Density",plot.title="SpeciesQ")
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
#'  plot.fishstd(df=stdX,legend.title="Standardized Density",plot.title="SpeciesX")
#'
#' }
#'

plot.fishstd<-function(df,legend.title=NULL,plot.title=NULL){
  heatmap.2(as.matrix(scale(df)), dendrogram='none',
            Rowv=FALSE, Colv=FALSE, trace = "none",density.info="none",
            keysize=1,
            key = TRUE,
            key.title = legend.title,
            key.xlab=plot.title,
            cexRow = (1.2) ,
            cexCol = (1.2) ,
            key.par=list(mar=c(3.5,0,3,0)),
            lmat=rbind(c(5, 4, 2), c(6, 1, 3)),
            lhei=c(1, 3.5), lwid=c(1, 10, 1),
            col = matlab.like(30))
}


