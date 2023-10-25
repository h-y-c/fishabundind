

context("Test plotinitseason()")

test_that("plotinitseason() is performing correctly", {
  
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
  
  expect_silent(plotinitseason(seasonQ))
  
  
})
