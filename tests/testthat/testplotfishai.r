

context("Test plotfishai()")

test_that("plotfishai() is performing correctly", {

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

  expect_silent(plotfishai(aiQ))

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

  expect_silent(plotfishai(aiX))

})




