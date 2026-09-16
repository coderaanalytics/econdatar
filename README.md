# econdatar
<!-- badges: start -->
[![R-CMD-check](https://github.com/coderaanalytics/econdatar/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/coderaanalytics/econdatar/actions/workflows/check-standard.yaml)
<!-- badges: end -->

## Quick start

```r
install.packages(c("remotes", "tcltk"), repos = "https://cran.mirror.ac.za")
library("remotes")
install_github("coderaanalytics/econdatar", ref = "4.0.4")
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
install_github("coderaanalytics/econdatar", ref = "4.0.4")
```

## Connecting to econdata.co.za

In most cases reading and writing data from econdata.co.za requires you to authenticate yourself. This usually requires you to enter an API key which you can get from the web application. Log in to the web application and click on the  *API key* button on the top left of the page. Paste this key into the dialogue box that appears when running `read_dataset` for example and click *Submit*.

Alternatively, you can set the `ECONDATA_APIKEY` environment variable in order to automate the above process.

If you are connecting to econdata.co.za through a portal other that the main site (someportal.econdata.co.za rather than www.econdata.co.za) you will need to set the `ECONDATA_URL` environment variable, please contact econdata@codera.co.za for further details.

Please see the [EconData user guide](https://www.econdata.co.za/user-guide) for in depth tutorials

## License

Copyright © 2026 Codera Pty Ltd

Available under the terms of the MIT License, see `LICENSE`.
