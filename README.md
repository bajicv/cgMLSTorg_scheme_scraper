# cgMLST.org webpage scraper



## Description

This R script allows
  - listing all of the available cgMLST schemes on [cgMLST.org](https://www.cgmlst.org/)
  - finding the version and/or date and time of the last change on scheme of interest
  - downloading specified schemes from cgMLST.org

## Requirements
- [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html)
- [rvest](https://cran.r-project.org/web/packages/rvest/index.html)
- [knitr](https://cran.r-project.org/web/packages/knitr/index.html)
- [optparse](https://cran.r-project.org/web/packages/optparse/index.html)

In case you would like to use this script you can easily install all the required packages by running the code below in your R session: 

```R
# Listing required packages
required_packages <- c("tidyverse", "rvest", "knitr", "optparse")

# Check if required packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

# Install missing packages
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}
```

## Usage

The script can be used directly from command line. 

To see help message

```bash
Rscript --vanilla cgMLSTorg_scheme_scraper.R --help
```

To list all available schemes on cgMLST.org

```bash
Rscript --vanilla cgMLSTorg_scheme_scraper.R
```

To finding the date and time of the last change on scheme of interest

```bash 
Rscript --vanilla cgMLSTorg_scheme_scraper.R -f last_change -i Abaumannii
```

To download specific scheme

```bash
Rscript --vanilla cgMLSTorg_scheme_scraper.R -f download -i Abaumannii
```