

context("Test fishai()")

test_that("fishai() is performing correctly", {
  
  expect_silent(fishai(fishQ, Year="yr",
                       Week="wk",
                       RegionName="regname",
                       Strata="stra",
                       StrataVolume="stravol",
                       RegionVolume="regvol",
                       VolumeSample="volsamp",
                       N_individual="nind",
                       SeasonEst=TRUE,first_thr=0.05,durationEst=7))
  
  
  expect_warning(fishai(fishX, Year="yr",
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
  
  
test_that("fishai() throws appropriate errors", {
  
  expect_error(fishai(fishQ, Year="yr",
                       Week="wk",
                       RegionName="regname",
                       Strata="stra",
                       StrataVolume="stravol",
                       RegionVolume="regvol",
                       VolumeSample="volsamp",
                       N_individual="nind",
                       SeasonEst=TRUE,first_thr=1.05,durationEst=7))
  
  
  expect_error(fishai(fishQ, Year="yr",
                      Week="wk",
                      RegionName="regname",
                      Strata="stra",
                      StrataVolume="stravol",
                      RegionVolume="regvol",
                      VolumeSample="volsamp",
                      N_individual="nind",
                      SeasonEst=TRUE,first_thr=1.05,durationEst="x"))
                 
  
})


