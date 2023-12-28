# econdatar
<!-- badges: start -->
[![R-CMD-check](https://github.com/coderaanalytics/econdatar/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/coderaanalytics/econdatar/actions/workflows/check-standard.yaml)
<!-- badges: end -->

## Quick start

```r
install.packages(c("remotes", "tcltk"), repos = "https://cran.mirror.ac.za")
library("remotes")
install_github("coderaanalytics/econdatar", ref = "2.0.0")
```

Install from disk

```r
install.packages("path/to/econdatar", repos = NULL , type = "source")
```

Re-install (upgrade when new versions are available) - **start a new session after running**

```r
library("remotes")
remove.packages("econdatar")
install_github("coderaanalytics/econdatar")
```

Or if selecting a particular release **(recommended)**, [see](https://github.com/coderaanalytics/econdatar/tags)

```r
library("remotes")
remove.packages("econdatar")
install_github("coderaanalytics/econdatar", ref = "2.0.0")
```

Please see the [EconData blog](https://randomsample.co.za) for in depth tutorials

## License

Copyright © 2023 Codera Pty Ltd

Available under the terms of the MIT License, see `LICENSE`.
