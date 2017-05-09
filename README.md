# Travis-CI API Client Package #

**travisci** is a simple client package for the [Travis-CI](http://travis-ci.org/) [REST API](http://docs.travis-ci.com/api/#overview). It can control containerized builds of R packages and other software projects as part of a continuous integration workflow. To learn more about developing R packages with Travis, see [the Travis documentation for R](http://docs.travis-ci.com/user/languages/r/).

## Code Examples ##

To use the Travis-CI API, you will need a Github account and an authentication key. That authentication key will be used temporarily by the Travis-CI API to generate a further access key that will authenticate subsequent API requests. To do this, you'll need to store a GitHub token in the `GITHUB_TOKEN` environment variable. **travisci** then uses this to populate a `TRAVIS_CI_TOKEN` environment variable, which will be used for subsequent authentications.

To obtain a GitHub token, you can do the following, replacing "username" and "password" with your GitHub login details:

```R
auth_travis("username", "password")

# or, if you already have a GitHub token stored:
auth_travis()
```

A token can also be created [via the browser](https://github.com/settings/tokens). `auth_travis()` will, as a side effect, store the Travis-CI token received into the `TRAVIS_CI_TOKEN` environment variable.

### Setting up Travis for a Repo ###

Create a new package (using `devtools::create()`, `package.skeleton()`, etc.) and then from that directory do the following, use devtools to setup GitHub and initial a `.travis.yml` file:

```R
library("devtools")
use_github()
use_travis()
```

Then use travisci to enable the webhook that will trigger Travis CI builds for every push to the GitHub repo:

```R
library("travisci")
auth_travis()
sync_users()
h <- get_hooks()
p <- as.package(".")$package
enable_hook(h[[which(unlist(lapply(g, `[`, "name")) == p)]])
```


### Getting repos and branches ###

```{r}
get_repo("cloudyr/travisci")
get_branch("cloudyr/travis", "master")
```

### Cancel and restart builds ###

```{r}
b <- get_builds("cloudyr/travisci")

# cancel
cancel_build(b[[1]]$id)

# restart
restart_build(b[[1]]$id)
```

### Settings and environment variables ###

```{r}
get_env_vars("cloudyr/travisci")

# set new environment variables
e <- add_env_vars("cloudyr/travisci", var = list(VAR1 = "value1", VAR2 = "value2"))

# delete those environment variables
delete_env_vars("cloudyr/travisci", id = e[[1]]$id)
```

### Trigger Dependent Builds ###

If you have two packages (PackageA and PackageB), in which PackageB depends on PackageA, you may want to trigger builds of PackageB every time you push changes to PackageA in order to test functionality in the dependent package. To do this, you can write an entry in your PackageA `.travis.yml` that will use travisci to trigger a build of PackageB. To do this, you also will need to supply an encrypted Travis-CI token into the PackageA repo so that the code will work (or include an encrypted GitHub token, so you can authenticate). Your PackageA `.travis.yml` will need the following:

```
r_github_packages:
- cloudyr/travisci
after_success:
- Rscript -e "travisci::restart_last_build('username/PackageB')"
```

Your PackageB `.travis.yml` will need to draw on the GitHub version of PackageA during its own build process:

```
r_github_packages:
- username/PackageA
```

A more complicated approach would restart the PackageB build, then use `get_build()` to determine whether the restarted PackageB build was successful by examining the `state` value for the build, which can be "created", "started", "passed", "failed", or "canceled". A simple `while` loop with period checking of the build state would allow one to further trigger a notification or a build failure for PackageA.

## Installation ##

[![CRAN](http://www.r-pkg.org/badges/version/travisci)](http://cran.r-project.org/package=travisci)
[![Build Status](https://travis-ci.org/cloudyr/travisci.png?branch=master)](https://travis-ci.org/cloudyr/travisci)
[![codecov.io](http://codecov.io/github/cloudyr/travisci/coverage.svg?branch=master)](http://codecov.io/github/cloudyr/travisci?branch=master)

This package is not yet on CRAN. To install the latest development version from GitHub:

```R
# latest stable version
install.packages("travisci", repos = c(getOption("repos"), "http://cloudyr.github.io/drat"))

# latest (unstable) version from GitHub
if (!require("ghit")) {
    install.packages("ghit")
}
ghit::install_github("cloudyr/travisci")
```

---
[![cloudyr project logo](http://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)