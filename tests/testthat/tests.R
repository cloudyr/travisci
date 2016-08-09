context("authentication")

test_that("basic login", {
    auth_travis()
})


context("Accounts")

test_that("check accounts", {
    expect_true(is.list(get_accounts()))
})


context("Users")

test_that("check users", {
    expect_true(sync_users())
    expect_true(is.list(get_users()))
})


context("repo")

test_that("get repo", {
    (r <- get_repo("cloudyr/travisci"))
    expect_true(inherits(r, "travis_repo"))
})

test_that("slug to id", {
    expect_true(is.numeric(travisci:::slug_to_id("cloudyr/travisci")))
})


test_that("Enable/Disable repo hook", {
    expect_true(disable_hook("cloudyr/travisci"), "Disable repo hook")
    expect_true(enable_hook("cloudyr/travisci"), "Enable repo hook")
})


test_that("get repo settings", {
    (s <- get_repo_settings("cloudyr/travisci"))
    expect_true(is.list(s))
})

test_that("environment variables", {
    # set environment variables
    a <- add_env_vars("cloudyr/travisci", list(TESTVAR = 1))
    expect_true(inherits(a[[1]], "travis_envvar"))
    # update environment variables
    expect_true(inherits(update_env_vars("cloudyr/travisci", id = a[[1]], list(TESTVAR = 2))[[1]], "travis_envvar"))
    # get environment variables
    expect_true(inherits(get_env_vars("cloudyr/travisci")[[1]], "travis_envvar"))
    # delete environment variables
    expect_true(delete_env_var("cloudyr/travisci", id = a[[1]]))
})

test_that("get branch", {
    b <- get_branch("cloudyr/travisci", "master")
})


test_that("get all builds", {
    g <- get_builds()
    expect_true(is.list(g))
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
