

library(fishabundind)

context("Test plot.fishstd()")

test_that("plot.fishstd() is performing correctly", {
  
  expect_silent( 
    aiQ<-fishstd(fishQ, Year="yr",
                Week="wk",
                RegionName="regname",
                Strata="stra",
                StrataVolume="stravol",
                RegionVolume="regvol",
                VolumeSample="volsamp",
                N_individual="nind",
                SeasonEst=TRUE,first_thr=0.05,durationEst=7)
  )
  graphics.off(); par("mar"); par(mar=c(1,1,1,1))
  expect_silent(plot.fishstd(aiQ))
  
  expect_warning(
    aiX<-fishstd(fishX, Year="yr",
                Week="wk",
                RegionName="regname",
                Strata="stra",
                StrataVolume="stravol",
                RegionVolume="regvol",
                VolumeSample="volsamp",
                N_individual="nind",
                SeasonEst=FALSE)
  )
  graphics.off(); par("mar"); par(mar=c(1,1,1,1))
  expect_silent(plot.fishstd(aiX))
  
})
