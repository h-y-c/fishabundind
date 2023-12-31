% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fishai.r
\name{fishai}
\alias{fishai}
\title{fishai function
Estimate annual abundance index}
\usage{
fishai(
  dataset,
  Year,
  Week,
  RegionName,
  Strata,
  StrataVolume,
  RegionVolume,
  VolumeSample,
  N_individual,
  SeasonEst = TRUE,
  first_thr = NULL,
  durationEst = NULL
)
}
\arguments{
\item{dataset}{A dataset contains the input data.
This dataset must include the following variables.}

\item{Year}{A variable of Year of data collection.}

\item{Week}{A variable of calender week of data collection.}

\item{RegionName}{A variable of names of regions where data were collected.}

\item{Strata}{A variable of strata within the corresponding \code{RegionName} where data were collected.}

\item{StrataVolume}{A variable of volume of the corresponding \code{Strata} where data were collected.
Depending on the sampling design, it could be area of a strata.}

\item{RegionVolume}{A variable of volume of the corresponding region where data were collected.
Depending on the sampling design, it could be area of a region.}

\item{VolumeSample}{A variable of volume of water sampled.
Depending on the sampling design, it could be time of tow duration or area swept.}

\item{N_individual}{A variable of number of individual sampled.}

\item{SeasonEst}{Logical. Should seasonality be considered? Some speices or some life stages
would have prominent peak season, especially for eggs and larvae.
If \code{TRUE}, \code{first_thr} and \code{durationEst} must be specified.
If \code{FALSE}, all data will be used.
The default value is \code{TRUE}.}

\item{first_thr}{A numeric value in between 0 and 1.
It is used to determine the first week of the year in which
the cumulative weekly density estimates surpass \code{first_thr}*100\% of
the total densities observed across all weeks of sampling.}

\item{durationEst}{Integer (>0).
The estimated duration of weeks the season will last.
For example, if the season is estimated to end 8 weeks after
the estimated first week (see \code{first_thr}), then the input should be 8.}
}
\value{
This function returns a dataframe which includes a variable of year,
a variable of annual abundance indeices, and a variable of variance
}
\description{
fishai function
Estimate annual abundance index
}
\details{
The data are assumed collected from a fishery-independent survey following
a stratified random sampling design with i regions within the sampling area and
s strata within a given region. The survey was conducted over week for each year.
The design-based annual abundance index for each year is estimated using the average density:
\code{AbundInd} = \deqn{\sum{\frac{\sum{\sum{\code{StrataVolume}*\frac{\sum{\code{N_individual}}}{\sum{\code{VolumeSample}}}}}}{\sum{\code{RegionVolume}}}}}
}
\examples{
\dontrun{
 aiQ<-fishai(dataset=fishQ,Year="yr",
            Week="wk",
            RegionName="regname",
            Strata="stra",
            StrataVolume="stravol",
            RegionVolume="regvol",
            VolumeSample="volsamp",
            N_individual="nind",
            SeasonEst=TRUE,first_thr=0.05,durationEst=7)
 aiQ

 aiX<-fishai(dataset=fishX,Year="yr",
            Week="wk",
            RegionName="regname",
            Strata="stra",
            StrataVolume="stravol",
            RegionVolume="regvol",
            VolumeSample="volsamp",
            N_individual="nind",
            SeasonEst=FALSE)
 aiX

}

}
\references{
Chang, H.-Y., Sun, M., Rokosz, K., and Chen, Y.. (2023)
Evaluating effects of changing sampling protocol for a long-term Ichthyoplankton monitoring program.
Frontiers in Marine Science. DOI: 10.3389/fmars.2023.1237549
}
