#' Description
#' This testthat tests the new indices added in climdex.pcic
#' 1. Test the eca.input function which can be used for station files from ECA&D
#' 2. Test the climdexInput.raw for the added parameters, added frequencies (halfyear, seasons), and new quantiles
#' 3. Test new indices for precipitation and temperature
#' 4. test new indices for new parameters: cloud, sun, snow, wind.
#' 5. We don't test yet the sun_rel and the snow_new (related indices)

#rm(list = ls())
library(climind)
library(testthat)

####################################
### Precipitation indices
####################################
context("Precipitation indices")

test_that("Precipitation indices annual", {
  prec <- eca.input('regressionInput/RR_STAID000162.txt', 'RR', 'DATE')

  expect_equal_to_reference(prec, "regressionOutput/rr_deBilt.rds")

  ## Here we test the additional frequencies (halfyear & seasons)
  ci <- climdexInput.raw(prec=prec$RR, prec.dates = prec$DATE, base.range=c(1961,1990))
  expect_equal_to_reference(ci, "regressionOutput/ci_rr_deBilt.rds")

  ## Test new indices for precipitation
  ci_spi3   <- climdex.spi3(ci, freq=c("monthly"), scale=3)
  ci_spi6   <- climdex.spi6(ci, freq=c("monthly"), scale=6)
  ci_r99p <- climdex.r99p(ci, freq=c("monthly"))
  ci_r99ptot <- climdex.r99ptot(ci, freq=c("monthly"))

  expect_equal_to_reference(ci_spi3, "regressionOutput/spi3_deBilt.rds")
  expect_equal_to_reference(ci_spi6, "regressionOutput/spi6_deBilt.rds")
  expect_equal_to_reference(ci_r99p, "regressionOutput/r99p_deBilt.rds")
  expect_equal_to_reference(ci_r99ptot, "regressionOutput/r99ptot_deBilt.rds")


})


####################################
### Temperature indices
####################################

context("tmax indices")

test_that("tmax indices annual", {
  tg <- eca.input('regressionInput/TG_STAID000162.txt', 'TG', 'DATE')
  tx <- eca.input('regressionInput/TX_STAID000162.txt', 'TX', 'DATE')
  tn <- eca.input('regressionInput/TN_STAID000162.txt', 'TN', 'DATE')
  prec <- eca.input('regressionInput/RR_STAID000162.txt', 'RR', 'DATE')

  expect_equal_to_reference(tg, "regressionOutput/tg_deBilt.rds")
  expect_equal_to_reference(tx, "regressionOutput/tx_deBilt.rds")
  expect_equal_to_reference(tn, "regressionOutput/tn_deBilt.rds")
  expect_equal_to_reference(prec, "regressionOutput/rr_deBilt.rds")


  ## Here we test the additional frequencies (halfyear & seasons) and the additional quantiles for temperature (q25, q75 )
  ci_temp <- climdexInput.raw(tmax= tx$TX, tmin=tn$TN, tavg=tg$TG, tmax.dates = tx$DATE, tmin.dates = tn$DATE,
                              prec=prec$RR, prec.dates = prec$DATE,
                              tavg.dates = tg$DATE,base.range=c(1961, 1991))

  expect_equal_to_reference(ci_temp, "regressionOutput/ci_temp_deBilt.rds")

  ## tmax
  ci_csu <- climdex.csu(ci_temp, freq=c("annual"))
  ci_su <- climdex.su(ci_temp, freq=c("seasonal"))
  #ci_txndaymin <- climdex.txndaymin(ci_temp, freq=c("annual"))
  #ci_txndaymax <- climdex.txndaymin(ci_temp, freq=c("annual"))

  expect_equal_to_reference(ci_csu, "regressionOutput/csu_deBilt.rds")
  expect_equal_to_reference(ci_su, "regressionOutput/su_deBilt.rds")
  #expect_equal_to_reference(ci_txndaymin, "regressionOutput/txndaymin_deBilt.rds")
  #expect_equal_to_reference(ci_txndaymax, "regressionOutput/txndaymax_deBilt.rds")
})

context("tmin indices")

test_that("tmin indices annual & monthly", {
  tn <- eca.input('regressionInput/TN_STAID000162.txt', 'TN', 'DATE')

  ci_temp <- climdexInput.raw(tmin=tn$TN, tmin.dates = tn$DATE,base.range=c(1961, 1991))

  ci_cfd   <- climdex.cfd(ci_temp, freq=c("monthly"))
  ci_fd    <- climdex.fd(ci_temp, freq=c("halfyear"))
  #ci_tnndaymin <- climdex.tnndaymin(ci_temp, freq=c("monthly"))
  #ci_tnndaymax <- climdex.tnndaymax(ci_temp, freq=c("annual"))

  expect_equal_to_reference(ci_cfd, "regressionOutput/cfd_deBilt.rds")
  expect_equal_to_reference(ci_fd, "regressionOutput/fd_deBilt.rds")
  #expect_equal_to_reference(ci_tnndaymin, "regressionOutput/tnndaymin_deBilt.rds")
  #expect_equal_to_reference(ci_tnndaymax, "regressionOutput/tnndaymax_deBilt.rds")
})

context("tavg indices")

test_that("tavg indices annual & monthly", {
  tn <- eca.input('regressionInput/TN_STAID000162.txt', 'TN', 'DATE')
  tx <- eca.input('regressionInput/TX_STAID000162.txt', 'TX', 'DATE')
  tg <- eca.input('regressionInput/TG_STAID000162.txt', 'TG', 'DATE')
  prec <- eca.input('regressionInput/RR_STAID000162.txt', 'RR', 'DATE')

  # ci_temp <- climdexInput.raw(tmin = tn$TN, tavg = tg$TG, prec = prec$RR,
  #                             tmin.dates = tn$DATE, prec.dates = prec$DATE, tavg.dates = tg$DATE,
  #                             base.range=c(1961, 1991))
  
  ci_temp <- climdexInput.raw(tmin = tn$TN, tmax = tx$TX, prec = prec$RR, tavg = tg$TG,
                              tmin.dates = tn$DATE, prec.dates = prec$DATE, tmax.dates = tx$DATE,tavg.dates = tg$DATE,
                              base.range=c(1961, 1991))

  ##tavg
  ci_hd17        <- climdex.hd17(ci_temp, freq=c("annual"))
  #ci_tmndaymin   <- climdex.tmndaymin(ci_temp, freq=c("monthly"))
  #ci_tmndaymax   <- climdex.tmndaymax(ci_temp, freq=c("monthly"))
   ci_cd        <- climdex.cd(ci_temp, freq=c("annual"))
   ci_cw        <- climdex.cw(ci_temp, freq=c("monthly"))
   ci_wd        <- climdex.wd(ci_temp, freq=c("monthly"))
   ci_ww        <- climdex.ww(ci_temp, freq=c("monthly"))

  expect_equal_to_reference(ci_hd17, "regressionOutput/hd17_deBilt.rds")
  # expect_equal_to_reference(ci_tmndaymin, "regressionOutput/tmndaymin_deBilt.rds")
  # expect_equal_to_reference(ci_tmndaymax, "regressionOutput/tmndaymax_deBilt.rds")
   expect_equal_to_reference(ci_cd, "regressionOutput/cd_deBilt.rds")
   expect_equal_to_reference(ci_cw, "regressionOutput/cw_deBilt.rds")
   expect_equal_to_reference(ci_wd, "regressionOutput/wd_deBilt.rds")
   expect_equal_to_reference(ci_ww, "regressionOutput/ww_deBilt.rds")

})

####################################
### Wind indices
####################################
# 
# context("Wind speed")
# 
# test_that("Wind speed annual & monthly", {
#   fg <- eca.input('regressionInput/FG_STAID000162.txt', 'FG', 'DATE')
#   fx <- eca.input('regressionInput/FX_STAID000162.txt', 'FX', 'DATE')
#   dd <- eca.input('regressionInput/DD_STAID000162.txt', 'DD', 'DATE')
# 
#   expect_equal_to_reference(fg, "regressionOutput/fg_deBilt.rds")
#   expect_equal_to_reference(fx, "regressionOutput/fx_deBilt.rds")
#   expect_equal_to_reference(dd, "regressionOutput/dd_deBilt.rds")
# 
# 
#   ## Here we test the additional frequencies (halfyear & seasons) and the additional quantiles for temperature (q25, q75 )
#   ci_wind <- climdexInput.raw(wind= fg$FG, wind_gust=fx$FX, wind_dir=dd$DD, wind.dates = fg$DATE, wind_gust.dates = fx$DATE,
#                               wind_dir.dates = dd$DATE, base.range=c(1961, 1991))
# 
#   expect_equal_to_reference(ci_wind, "regressionOutput/ci_wind_deBilt.rds")
# 
#   ## wind speed
#   ci_fg   <- climdex.fg(ci_wind, freq=c("monthly"))
#   ci_fgcalm <- climdex.fgcalm(ci_wind, freq=c("monthly"))
#   ci_fg6bft <- climdex.fg6bft(ci_wind, freq=c("annual"))
# 
#   expect_equal_to_reference(ci_fg, "regressionOutput/fg_ci_deBilt.rds")
#   expect_equal_to_reference(ci_fgcalm, "regressionOutput/fgcalm_deBilt.rds")
#   expect_equal_to_reference(ci_fg6bft, "regressionOutput/fg6bft_deBilt.rds")
# 
# })
# #
# context("Wind gust")
#
# test_that("Wind gust monthly", {
#   fx <- eca.input('regressionInput/FX_STAID000162.txt', 'FX', 'DATE')
#
#   ci_wind <- climdexInput.raw(wind_gust=fx$FX, wind_gust.dates = fx$DATE, base.range=c(1961, 1991))
#
#   ## wind gust
#   ci_fxstorm   <- climdex.fxstorm(ci_wind, freq=c("monthly"))
#   ci_fxx <- climdex.fxx(ci_wind, freq=c("monthly"))
#
#   expect_equal_to_reference(ci_fxstorm, "regressionOutput/fxstorm_deBilt.rds")
#   expect_equal_to_reference(ci_fxx, "regressionOutput/fxx_deBilt.rds")
# })
#
# context("Wind direction")
#
# test_that("Wind direction annual and monthly", {
#   dd <- eca.input('regressionInput/DD_STAID000162.txt', 'DD', 'DATE')
#
#   ci_wind <- climdexInput.raw(wind_dir=dd$DD, wind_dir.dates = dd$DATE, base.range=c(1961, 1991))
#
#   ## wind direction
#   ci_ddnorth   <- climdex.ddnorth(ci_wind, freq=c("monthly"))
#   ci_ddeast <- climdex.ddeast(ci_wind, freq=c("monthly"))
#   ci_ddsouth <- climdex.ddsouth(ci_wind, freq=c("annual"))
#   ci_ddwest <- climdex.ddwest(ci_wind, freq=c("annual"))
#
#   expect_equal_to_reference(ci_ddnorth, "regressionOutput/ddnorth_deBilt.rds")
#   expect_equal_to_reference(ci_ddeast, "regressionOutput/ddeast_deBilt.rds")
#   expect_equal_to_reference(ci_ddsouth, "regressionOutput/ddsouth_deBilt.rds")
#   expect_equal_to_reference(ci_ddwest, "regressionOutput/ddwest_deBilt.rds")
#
# })

####################################
### Snow indices
####################################
#
# context("Snow depth")
#
# test_that("Snow annual & monthly", {
#   snowD <- eca.input('regressionInput/SD_STAID000242.txt', 'SD', 'DATE')
#
#   expect_equal_to_reference(snowD, "regressionOutput/snowD_deBilt.rds")
#
#   ci_snow <- climdexInput.raw(snow = snowD$SD, snow.dates = snowD$DATE, base.range=c(1961, 1991))
#
#   expect_equal_to_reference(ci_snow, "regressionOutput/ci_snow_Lugano.rds")
#
#   ## snow depth
#   ci_sdd   <- climdex.sdd(ci_snow, freq=c("monthly"))
#   ci_sdx <- climdex.sdx(ci_snow, freq=c("monthly"))
#   ci_sd <- climdex.sd(ci_snow, freq=c("annual"))
#
#   expect_equal_to_reference(ci_sdd, "regressionOutput/sdd_deBilt.rds")
#   expect_equal_to_reference(ci_sdx, "regressionOutput/sdx_deBilt.rds")
#   expect_equal_to_reference(ci_sd, "regressionOutput/sd_deBilt.rds")
#
# })

####################################
### Cloud indices
####################################
# 
# context("Cloud indices")
# 
# test_that("Cloud indices annual & monthly", {
#   cloud <- eca.input('regressionInput/CC_STAID000162.txt', 'CC', 'DATE')
# 
#   expect_equal_to_reference(cloud, "regressionOutput/cloud_deBilt.rds")
# 
#   ci_cloud <- climdexInput.raw(cloud = cloud$CC, cloud.dates = cloud$DATE, base.range=c(1961, 1991))
# 
#   expect_equal_to_reference(ci_cloud, "regressionOutput/ci_cloud_deBilt.rds")
# 
#   ## snow depth
#   ci_cc   <- climdex.cc(ci_cloud, freq=c("monthly"))
#   ci_cc6 <- climdex.cc6(ci_cloud, freq=c("monthly"))
#   ci_cc2 <- climdex.cc2(ci_cloud, freq=c("annual"))
# 
#   expect_equal_to_reference(ci_cc, "regressionOutput/cc_deBilt.rds")
#   expect_equal_to_reference(ci_cc6, "regressionOutput/cc6_deBilt.rds")
#   expect_equal_to_reference(ci_cc2, "regressionOutput/cc2_deBilt.rds")
# 
# })

####################################
### Sun indices
####################################
# 
# context("Sun indices")
# 
# test_that("Sun index monthly", {
#   sunshine <- eca.input('regressionInput/SS_STAID000162.txt', 'SS', 'DATE')
# 
#   expect_equal_to_reference(sunshine, "regressionOutput/sunshine_deBilt.rds")
# 
#   ci_sun <- climdexInput.raw(sun = sunshine$SS, sun.dates = sunshine$DATE, base.range=c(1961, 1991))
# 
#   expect_equal_to_reference(ci_sun, "regressionOutput/ci_sun_deBilt.rds")
# 
#   ## snow depth
#   ci_ss  <- climdex.ss(ci_sun, freq=c("monthly"))
# 
#   expect_equal_to_reference(ci_ss, "regressionOutput/ss_deBilt.rds")
# 
# })


