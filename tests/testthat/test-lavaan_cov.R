suppressWarnings(library(lavaan))

x <- paste0("x", 1:9)
latent <- list(
  visual = x[1:3],
  textual = x[4:6],
  speed = x[7:9]
)

regression <- list(
  ageyr = c("visual", "textual", "speed"),
  grade = c("visual", "textual", "speed")
)

HS.model <- write_lavaan(latent = latent, regression = regression)

fit <- sem(HS.model, data = HolzingerSwineford1939)


#   ____________________________________________________________________________
#   Tests                                                                   ####


test_that("nice_fit regular", {
  expect_s3_class(
    lavaan_cov(fit),
    c("lavaan.data.frame", "data.frame")
  )
})

test_that("nice_fit as nice_table", {
  skip_if_not_installed("rempsyc")
  expect_s3_class(
    lavaan_cov(fit, nice_table = TRUE),
    c("nice_table", "flextable")
  )
})
