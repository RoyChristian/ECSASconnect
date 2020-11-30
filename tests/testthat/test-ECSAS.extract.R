

test_that("species input errors are detected",{
  expect_error(ECSAS.extract(species = 1), "Variable 'species.+Must be of type 'string'")
  expect_error(ECSAS.extract(species = "ABCDc"), "Variable 'species'.+String must be of length 4")
  expect_error(ECSAS.extract(species = NA), "Variable 'species'.+May not be NA")
  expect_error(ECSAS.extract(species = NaN), "Variable 'species'.+May not be NA")
})


# do i need to check every possible type of input error for every param? seems excessive

# Now do tests where it succeeds and use expect_snapshot_value() to compare
# dataframes returned from ECSAS.extract()
# uses a stripped down database with only one cruise in it for speed.
test_that("DOVE 2020 extraction is correct", {
  expect_snapshot_value(ECSAS.extract(species = "DOVE", years = 2020,
                                      ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")),
                        style = "json2")
})