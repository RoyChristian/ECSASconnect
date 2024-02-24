# Test that the include.empty.watches returns the expected number of 
# zero watches and non-zeroa obs.
withzeros <- ECSAS.extract(
  species = "DOVE",
  years = 2020,
  ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")
)
zeros <- sum(is.na(withzeros$FlockID))
withoutzeros <- ECSAS.extract(
  species = "DOVE",
  years = 2020,
  include.empty.watches = FALSE,
  ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")
)
test_that("include.empty.watches works as expected", {
  expect_identical(sum(is.na(withoutzeros$FlockID)), 0L)
  expect_identical(nrow(withoutzeros) + zeros, nrow(withzeros))
})


test_that("species input errors are detected",{
  expect_error(ECSAS.extract(species = 1, ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")), "Variable 'species")
  expect_error(ECSAS.extract(species = "ABCDc", ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")), "Variable 'species'")
  expect_error(ECSAS.extract(species = NA, ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")), "Variable 'species'.+May not be NA")
  expect_error(ECSAS.extract(species = NaN, ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")), "Variable 'species'.+May not be NA")
})

# Make sure both single and multiple species specs are accepted
test_that("Correct species inputs are OK", {
  # Single species
  expect_no_error(ECSAS.extract(
    species = "DOVE",
    years = 2020,
    ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")
  ))
  
  # Multiple species
  expect_no_error(ECSAS.extract(
    species = c("DOVE", "TBMU"),
    years = 2020,
    ecsas.path = here::here("tests/testthat/Master ECSAS v 3.64.mdb")
  ))
  
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
