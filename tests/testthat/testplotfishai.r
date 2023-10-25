



library(fishabundind)

context("Test plot.fishai()")

test_that("plot.fishai() is performing correctly", {
  
  expect_silent( 
    aiQ<-fishai(fishQ, Year="yr",
                Week="wk",
                RegionName="regname",
                Strata="stra",
                StrataVolume="stravol",
                RegionVolume="regvol",
                VolumeSample="volsamp",
                N_individual="nind",
                SeasonEst=TRUE,first_thr=0.05,durationEst=7)
  )
  
  expect_silent( plot.fishai(aiQ) )
  
  expect_warning(
      aiX<-fishai(fishX, Year="yr",
              Week="wk",
              RegionName="regname",
              Strata="stra",
              StrataVolume="stravol",
              RegionVolume="regvol",
              VolumeSample="volsamp",
              N_individual="nind",
              SeasonEst=FALSE)
  )

  expect_silent(plot.fishai(aiX))

})




