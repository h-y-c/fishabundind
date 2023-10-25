

context("Test plotfishstd()")

test_that("plotfishstd() is performing correctly", {
  
  expect_silent( 
    stdQ<-fishstd(fishQ, Year="yr",
                Week="wk",
                RegionName="regname",
                Strata="stra",
                StrataVolume="stravol",
                RegionVolume="regvol",
                VolumeSample="volsamp",
                N_individual="nind",
                SeasonEst=TRUE,first_thr=0.05,durationEst=7)
  )
 
  expect_error(plotfishstd(stdQ)) # this can return "figure margins too large" error

  expect_warning(
    stdX<-fishstd(fishX, Year="yr",
                Week="wk",
                RegionName="regname",
                Strata="stra",
                StrataVolume="stravol",
                RegionVolume="regvol",
                VolumeSample="volsamp",
                N_individual="nind",
                SeasonEst=FALSE)
  )

  expect_error(plotfishstd(stdX)) # this can return "figure margins too large" error
  
})
