context("authentication")

test_that("basic login", {
  auth_travis()
})


context("Accounts")

test_that("check accounts", {
  get_accounts()
})


context("Users")

test_that("check users", {
  sync_users()
  get_users()
})


context("repo")

test_that("get repo", {
  (r <- get_repo("cloudyr/travisci"))
  expect_true(inherits(r, "travis_repo"))
})

test_that("slug to id", {
  expect_true(is.numeric(travisci:::slug_to_id("cloudyr/travisci")))
})


test_that("get repo settings", {
  (s <- get_repo_settings("cloudyr/travisci"))
  expect_true(is.list(s))
})

test_that("environment variables", {
  # get environment variables
  # set environment variables
  TRUE
})

test_that("get branch", {
  b <- get_branch("cloudyr/travisci", "master")
})


test_that("get all builds", {
  get_builds()
})

test_that("get specific build", {
  (b <- get_builds(86424608))
  expect_true(inherits(b$build, "travis_build"))
})

test_that("restart and cancel build", {
  expect_true(restart_build(86424608))
  expect_true(cancel_build(86424608))
})

test_that("restart last build by slug", {
  #r <- restart_last_build("cloudyr/travisci")
  #expect_true(r)
  #cancel_build(attributes(r, "build_id"))
  TRUE
})


test_that("get requests", {
  TRUE
})



context("cache")

test_that("get cache", {
  TRUE
})


context("logs")

test_that("get logs", {
  TRUE
})

context("lint")

test_that("lint", {
  TRUE
})

