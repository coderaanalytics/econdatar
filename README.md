# Quick start

```r
install.packages(c("remotes", "tcltk"), repos = "https://cran.mirror.ac.za")
library("remotes")
install_github("coderaanalytics/econdatar", ref = "1.1.4")
```

Install from disk

```r
install.packages("path/to/econdatar", repos = NULL , type = "source")
```

Re-install (when new versions are available) - **start a new session after running**

```r
library("remotes")
remove.packages("econdatar")
install_github("coderaanalytics/econdatar")
```

Or if selecting a particular release, [see](https://github.com/coderaanalytics/econdatar/tags)

```r
library("remotes")
remove.packages("econdatar")
install_github("coderaanalytics/econdatar", ref = "1.1.4")
```

Please see the [EconData blog](https://randomsample.co.za) for in depth tutorials

# License

Copyright © 2022 EconData, developed by Codera Pty Ltd

Available under the terms of the Eclipse Public License 2.0, see `LICENSE`.
