# econdatar
<!-- badges: start -->
[![R-CMD-check](https://github.com/coderaanalytics/econdatar/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/coderaanalytics/econdatar/actions/workflows/check-standard.yaml)
<!-- badges: end -->

## Quick start

```r
install.packages(c("remotes", "tcltk"), repos = "https://cran.mirror.ac.za")
library("remotes")
install_github("coderaanalytics/econdatar", ref = "4.0.1")
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
install_github("coderaanalytics/econdatar", ref = "4.0.1")
```

## Connecting to econdata.co.za

In most cases reading and writing data from econdata.co.za requires you to authenticate yourself. This usually requires you to enter an API token which you can get from the web application. Log in to the web application and go to the *Account* page, click on *Show key* and copy the API token by clicking anywhere on the token itself. Paste this token into the dialogue box that appears when running `read_dataset` for example and click *Submit*.

If you are using a service account you can set the `ECONDATA_CREDENTIALS` env variable with the format `client_id;client_secret` in order to automate the above process.

If you are connecting to econdata.co.za through a portal other that the main site (someportal.econdata.co.za rather than www.econdata.co.za) you will need to set the `ECONDATA_URL` and `ECONDATA_AUTH_URL` env variables, please contact econdata@codera.co.za for further details.

Please see the [EconData blog](https://econdata.co.za) for in depth tutorials

## License

Copyright © 2025 Codera Pty Ltd

Available under the terms of the MIT License, see `LICENSE`.
