
<!-- README.md is generated from README.Rmd. Please edit that file -->

# thrive

<!-- badges: start -->
<!-- badges: end -->

## Notes

General goal is to freshen up the way survival plotting works

Key goals:

- implement generics and method dispatch (not a user facing change)
- Focus largely on transforming DATA and let users do ggplot magic for
  presentation

Currently working on implementing a pared-down `surv_fit`.

`surv_fit` and `survfit` are actually more different than the
documentation initially lets on.

`surv_fit` actually returns its input data along with the normal
`survfit` output, but it ‘spoofs’ its class by adding the class on the
end, making it look like a `survfit` object.

It appears to be packing them in the ‘call’ portion, which should just
be redundant data. This ends up making objects roughly 4x the size.
Don’t want to prematurely optimize here, but it seems like we can do
better.

## Installation

You can install `thrive` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KaiAragaki/thrive")
```
