


library(fishabundind)

context("Test fishinitseasonEst()")

test_that("fishinitseasonEst() is performing correctly", {
  
  expect_silent(fishinitseasonEst(fishQ, Year="yr",
                                  Week="wk",
                                  RegionName="regname",
                                  Strata="stra",
                                  StrataVolume="stravol",
                                  RegionVolume="regvol",
                                  VolumeSample="volsamp",
                                  N_individual="nind",
                                  first_thr=0.05,durationEst=7))
  
  
})


test_that("fishinitseasonEst() throws appropriate errors", {
  
  expect_error(fishinitseasonEst(fishQ, Year="yr",
                                 Week="wk",
                                 RegionName="regname",
                                 Strata="stra",
                                 StrataVolume="stravol",
                                 RegionVolume="regvol",
                                 VolumeSample="volsamp",
                                 N_individual="nind",
                                 first_thr=1.05,durationEst=7))
  
  expect_error(fishinitseasonEst(fishQ, Year="yr",
                                 Week="wk",
                                 RegionName="regname",
                                 Strata="stra",
                                 StrataVolume="stravol",
                                 RegionVolume="regvol",
                                 VolumeSample="volsamp",
                                 N_individual="nind",
                                 first_thr=1.05,durationEst="x"))
  
  
  expect_error(fishinitseasonEst(as.matrix(fishQ), Year="yr",
                                 Week="wk",
                                 RegionName="regname",
                                 Strata="stra",
                                 StrataVolume="stravol",
                                 RegionVolume="regvol",
                                 VolumeSample="volsamp",
                                 N_individual="nind",
                                 first_thr=0.05,durationEst=7))
  
  
})


