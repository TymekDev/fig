# fig
> _A Config Package with No "Con"_

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/fig)](https://cran.r-project.org/package=fig)
[![R build status](https://github.com/TymekDev/fig/workflows/R-CMD-check/badge.svg)](https://github.com/TymekDev/fig/actions)
[![Codecov test coverage](https://codecov.io/gh/TymekDev/fig/branch/main/graph/badge.svg)](https://app.codecov.io/gh/TymekDev/fig?branch=main)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)][MIT]
<!-- badges: end -->


## Installation
```r
install.packages("fig")
```

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

# Environment variable value gets picked over a stored value
withr::with_envvar(list(foo = "xyz"), {
  fig_get_many("foo", "bar")) # == list("xyz", 456)
})

fig_delete("foo")

# Environment variable value gets picked over a missing stored value
withr::with_envvar(list(foo = "xyz"), {
  fig_get("foo")) # == "xyz"
})

fig_get("foo") # == NULL
```

#### Notes
- As seen in the above example fig does not perform any type coercion. System
  environment variables are returned as characters, regardless of stored value
  type.
- Dots (`.`) in keys are replaced with underscores (`_`) during system
  environment lookup.
- System environment lookup is case sensitive.

### Environment Variable Prefix
This feature goes in pair with precedence. fig can be configured with
`env_prefix` argument (default: `""`). It can be provided via `fig_configure()`
(or `configure()` method) or during `Fig` instance creation.

`env_prefix` determines value prepended to the key before performing a system
environment lookup.

```r
fig <- Fig$new(env_prefix = "RCONNECT_")
withr::with_envvar(list(RCONNECT_SERVER = "example.com", {
  fig$get("SERVER") # == "example.com"
})
```

### Key Splitting
> :information_source: _This feature is useful if you want interact
> (dynamically) with a nested config without a hassle._

fig can be configured with `split_on` argument (default: `"."`). It can be
provided via `fig_configure()` (or `configure()` method) or during `Fig`
instance creation.

`split_on` determines a level delimiter for keys, i.e. with `split_on` set
`"foo.bar"` is treated as `bar` nested under `foo`.

```r
fig_store("foo", list(bar = 1))
fig_get("foo.bar") # == 1

# Storage
# └── foo
#     └── bar
#         └── 1

fig_configure(split_on = "") # Disable this functionality
fig_store("foo.bar", 2)
fig_get("foo.bar") # == 2

# Storage
# ├── foo
# │   └── bar
# │       └── 1
# └── foo.bar
#     └── 2
```

**Note:** this behavior currently is not supported by delete functions and
methods.


## Examples
### Working with config
If you are using [config](https://github.com/rstudio/config) package and would
like to enjoy fig's features, then the suggested approach is to wrap your main
`config::get()` call in `fig_store_list()` (or `store_list()` method).

```r
# default:
#   foo: 123
fig_store_list(config::get())

fig_get("foo") # == 123

withr::with_envvar(list(foo = "xyz"), {
  fig_get("foo") # == "xyz"
})
```

This way you can use fig functions to have [precedence](#precedence) and [key
splitting](#key-splitting) working for retrieving values from your config.


<!--
## Contributing
TODO
-->


## About
Licensed under [MIT].
Inspired by [viper](https://github.com/spf13/viper).
Written in [Neovim](https://github.com/neovim/neovim).
Tested with [testthat](https://github.com/r-lib/testthat).
Used in [radian](https://github.com/randy3k/radian).


<!-- Links -->
[MIT]: https://opensource.org/licenses/MIT
