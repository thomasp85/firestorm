
<!-- README.md is generated from README.Rmd. Please edit that file -->

# firestorm

<!-- badges: start -->

[![R-CMD-check](https://github.com/thomasp85/firestorm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thomasp85/firestorm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/thomasp85/firestorm/graph/badge.svg)](https://app.codecov.io/gh/thomasp85/firestorm)
<!-- badges: end -->

firestorm is a package providing load-balancing (WIP) and reverse
proxying for fiery based web servers as plugins. It supports proxying of
both HTTP and WebSocket

## Installation

You can install the development version of firestorm from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("thomasp85/firestorm")
```

## Example

Below you can see an example of using firestorm with a fiery server.
Here we relay all requests to the fiery server to another service
running on the same computer.

``` r
library(fiery)
library(firestorm)

# create a reverse proxy to another service running on your computer
# at 127.0.0.1:9876
revprox <- ReverseProxy$new("http://127.0.0.1:9876")

# Attach it to a fiery server
app <- Fire$new()
app$attach(revprox)

# Start the server
app$ignite()
```
