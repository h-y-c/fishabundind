

library(fishabundind)

context("Test plot.initseason()")

test_that("plot.initseason() is performing correctly", {
  
  expect_silent( 
    seasonQ<-fishinitseasonEst(fishQ, Year="yr",
                 Week="wk",
                 RegionName="regname",
                 Strata="stra",
                 StrataVolume="stravol",
                 RegionVolume="regvol",
                 VolumeSample="volsamp",
                 N_individual="nind",
                 first_thr=0.05,durationEst=7)
  )
  
  expect_silent(plot.initseason(seasonQ))
  
  
})
