# caliper
Interact with Caliper software from R

## Installation

```r
library(devtools)
devtools::install_github('dkyleward/caliper', build_vignettes = TRUE)
```

## Getting Started

```r
library(caliper)
RunMacro("G30 Tutorial Folder")
#> "C:/Users/..."
```

See package vignettes for more details. For the basics, start with:

```r
vignette("using-caliper")
```
