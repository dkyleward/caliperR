# caliper
Interact with Caliper software from R

## Installation

### For R 3.6.x users:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github('dkyleward/caliperR', build_vignettes = TRUE)
```

### For R 4.0 users:

The RDCOMClient package has not been updated for R 4.0 ([issue
1](https://github.com/omegahat/RDCOMClient/issues/24) [issue
2](https://github.com/omegahat/RDCOMClient/issues/19)). In the mean time, I have
created a 'special' binary of my RDCOMClient fork that will work (thanks to
@RikSchoemaker
[here](https://stackoverflow.com/questions/61735315/cant-build-rdcomclient-using-rtools40-and-r-4-0/62658906#62658906)).
Use the following code to install a working version of RDCOMClient and then
caliperR.

```r
dir <- tempdir()
zip <- file.path(dir, "RDCOMClient.zip")
url <- "https://github.com/dkyleward/caliperR/releases/download/v1.0.0/RDCOMClient.zip"
download.file(url, zip)
install.packages(zip, repos = NULL, type = "win.binary", INSTALL_opts = '--no-lock')
if (!require("devtools")) install.packages("devtools")
devtools::install_github('dkyleward/caliperR', build_vignettes = TRUE)
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
