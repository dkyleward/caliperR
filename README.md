# caliper
Use Caliper software from R

## Installation

```r
# install.packages("devtools")
devtools::install_github('Caliper-Corporation/caliperR', build_vignettes = TRUE)
```

## Getting Started

```r
library(caliperR)
dk <- connect()
RunMacro("G30 Tutorial Folder")
#> "C:/Users/..."
```

For more details, see:

```r
vignette("using-caliperR")
```
