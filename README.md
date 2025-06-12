
<!-- README.md is generated from README.Rmd. Please edit that file -->

# firestorm

<!-- badges: start -->

<!-- badges: end -->

firestorm is a package providing load-balancing and reverse proxying for
fiery based web servers as plugins. It supports proxying of both HTTP
and WebSocket

## Installation

You can install the development version of firestorm from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("thomasp85/firestorm")
```

## Example

This is a basic example which shows you how to solve a common problem:

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
