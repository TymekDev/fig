# fig
> _A Config Package with No "Con"_

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/fig)](https://cran.r-project.org/package=fig)
[![R build status](https://github.com/TymekDev/fig/workflows/R-CMD-check/badge.svg)](https://github.com/TymekDev/fig/actions)
[![Codecov test coverage](https://codecov.io/gh/TymekDev/fig/branch/main/graph/badge.svg)](https://app.codecov.io/gh/TymekDev/fig?branch=main)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)][MIT]
<!-- badges: end -->


## Installation
<!-- TODO:
```r
install.packages("fig")
```
-->

To install latest available version from GitHub use remotes package:
```r
# install.packages("remotes")
remotes::install_github("TymekDev/fig")
```


## Usage
fig can be used in two different ways:
1. On a global level using `fig_*` functions
1. On a class instance level

Check out the following sections to find the way that suits your needs.

### Using `fig_*` Functions
This approach works with no additional prep. It is especially useful if you want
to access stored values without the hassle to pass an argument through entire
codebase. Values stored with `fig_*` live inside fig package making them
accessible from any place in the code.

```r
fig_store("foo", 123)
fig_get("foo") # == 123
```
For more examples see package documentation.

### Using a `Fig` Class Instance
Using `Fig` class allows storing values separately in several distinct class
instances. If you need a config separation in your code, then `Fig` class is for you.
Note that, unlike with `fig_*` functions, you have to pass the instance around
to access values stored in it.

```r
fig <- Fig$new()
fig$store("foo", 123)
fig$get("foo") # == 123

fig2 <- Fig$new()$store("foo", 456)
fig2$get("foo") # == 456
```
For more examples see package documentation.


## Features
### Precedence
> :information_source: _This feature is useful if you want to override a stored
> value without introducing a code change (e.g. log level, API URL, ...)._

fig supports a two level precedence. Every `fig_get()` function and `get()`
method call performs a lookup in a following order:
1. System environment variables
1. Stored values

```r
fig_store_many(foo = 123, bar = 456)
fig_get_many("foo", "bar") # == list(123, 456)

# Environment variable value gets picked over stored value
withr::with_envvar(list(foo = "xyz"), {
  fig_get_many("foo", "bar")) # == list("xyz", 456)
})

fig_delete("foo")

# Environment variable value gets picked over missing stored value
withr::with_envvar(list(foo = "xyz"), {
  fig_get("foo")) # == "xyz"
})

fig_get("foo") # == NULL
```

**Note:** as seen in the above example fig does not perform any type coercion.
System environment variables are returned as characters.

### Key Splitting
TODO


## Examples
### Working with `config`
TODO


## Contributing
TODO


## About
Licensed under [MIT].
Inspired by [viper](https://github.com/spf13/viper).
Written in [Neovim](https://github.com/neovim/neovim).
Tested with [testthat](https://github.com/r-lib/testthat).
Used in [radian](https://github.com/randy3k/radian).


<!-- Links -->
[MIT]: https://opensource.org/licenses/MIT
