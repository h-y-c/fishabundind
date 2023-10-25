


library(fishabundind)

context("Test fishstd()")

test_that("fishstd() is performing correctly", {
  
  expect_silent(fishstd(fishQ, Year="yr",
                       Week="wk",
                       RegionName="regname",
                       Strata="stra",
                       StrataVolume="stravol",
                       RegionVolume="regvol",
                       VolumeSample="volsamp",
                       N_individual="nind",
                       SeasonEst=TRUE,first_thr=0.05,durationEst=7))
  
  
  expect_warning(fishstd(fishX, Year="yr",
                        Week="wk",
                        RegionName="regname",
                        Strata="stra",
                        StrataVolume="stravol",
                        RegionVolume="regvol",
                        VolumeSample="volsamp",
                        N_individual="nind",
                        SeasonEst=FALSE)
                 
  )
  
})


test_that("fishstd() throws appropriate errors", {
  
  expect_error(fishstd(fishQ, Year="yr",
                      Week="wk",
                      RegionName="regname",
                      Strata="stra",
                      StrataVolume="stravol",
                      RegionVolume="regvol",
                      VolumeSample="volsamp",
                      N_individual="nind",
                      SeasonEst=TRUE,first_thr=1.05,durationEst=7))
  
  
  expect_error(fishstd(fishQ, Year="yr",
                      Week="wk",
                      RegionName="regname",
                      Strata="stra",
                      StrataVolume="stravol",
                      RegionVolume="regvol",
                      VolumeSample="volsamp",
                      N_individual="nind",
                      SeasonEst=TRUE,first_thr=0.05,durationEst="x"))
  
  
})


