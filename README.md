Freshwater navigation canals support invasive species across spatial scales <img src="https://raw.githubusercontent.com/FRBCesab/templates/main/logos/compendium-sticker.png" align="right" style="float:right; height:120px;"/>
=========================================================

<!-- badges: start -->
[![License: GPL (>= 2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->



<p align="left">
  • <a href="#overview">Overview</a><br>
  • <a href="#data-sources">Data sources</a><br>
  • <a href="#content">Content</a><br>
  • <a href="#prerequisites">Prerequisites</a><br>
  • <a href="#installation">Installation</a><br>
  • <a href="#usage">Usage</a><br>
  • <a href="#citation">Citation</a><br>
  • <a href="#contributing">Contributing</a><br>
  • <a href="#acknowledgments">Acknowledgments</a><br>
  • <a href="#references">References</a>
</p>



## Overview

This research compendium provides code and data used to reproduce analyses of the paper: 

> Sexton AN _et al._ (2024) Freshwater navigation canals support invasive species across spatial scales. Submitted to **Global Change Biology**.



## Data sources

This project uses the following databases:

| Database      | Usage                          | Reference       |                   Link                   |
|:--------------|:-------------------------------|:----------------|:----------------------------------------:|
| GBIF          | Get species occurrences        | GBIF.org (2024) |      [link](https://www.gbif.org/)       |
| Natural Earth | Create a World grid            | None            | [link](https://www.naturalearthdata.com) |



## Content

This repository is structured as follow:

- :file_folder: [supplementary-materials/](https://github.com/CStaentzel/GBIF-paper/tree/main/supplementary-materials) contains data files associated to the paper
- :file_folder: [analyses/](https://github.com/CStaentzel/GBIF-paper/tree/main/analyses) contains R scripts for running analyses
- :page_facing_up: [DESCRIPTION](https://github.com/CStaentzel/GBIF-paper/tree/main/DESCRIPTION) contains project metadata



## Prerequisites

### System requirements

This project handles spatial objects with the R package
[`sf`](https://r-spatial.github.io/sf/) and requires some additional
software: GDAL, GEOS, and PROJ.

### GBIF account

A GBIF account is required to download GBIF occurrences as ZIP files.
First, create a GBIF account by visiting this page
<https://www.gbif.org/user/profile>. Then, store your login information
locally in the `~/.Renviron` file. Use the function
`usethis::edit_r_environ()` to create/open this file and add the
following three lines:

    GBIF_USER='your_username'
    GBIF_PWD='your_password'
    GBIF_EMAIL='your_email'

Restart R, and check if everything is ok:

``` r
Sys.getenv("GBIF_USER")
Sys.getenv("GBIF_PWD")
Sys.getenv("GBIF_EMAIL")
```


## Installation

To install this compendium:

- [Fork](https://docs.github.com/en/get-started/quickstart/contributing-to-projects) 
this repository using the GitHub interface.
- [Clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) 
your fork using `git clone fork-url` (replace `fork-url` by the URL of your fork). 
Alternatively, open [RStudio IDE](https://posit.co/products/open-source/rstudio/) 
and create a New Project from Version Control.



## Usage

Before running any analysis listed in [analyses/](https://github.com/CStaentzel/GBIF-paper/tree/main/analyses), please install the required packages by running:

```r
# Install 'remotes' package
install.packages("remotes")

# Install required packages
remotes::install_deps()
```



## Citation

Please use the following citation: 

> Sexton AN _et al._ (2024) Freshwater navigation canals support invasive species across spatial scales. URL: <https://github.com/CStaentzel/GBIF-paper/>.



## Contributing

All types of contributions are encouraged and valued. For more information, 
check out our [Contributor Guidelines](https://github.com/CStaentzel/GBIF-paper/blob/main/CONTRIBUTING.md).

Please note that this project is released with a 
[Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). 
By contributing to this project, you agree to abide by its terms.



## Acknowledgments

This project has been developed for the
[FRB-CESAB](https://www.fondationbiodiversite.fr/en/about-the-foundation/le-cesab/)
research group
[Navidiv](https://www.fondationbiodiversite.fr/en/the-frb-in-action/programs-and-projects/le-cesab/navidiv/)
that aims to provide useful synthetic knowledge and guidelines to prioritize management and restoration actions considering the various human uses of waterwayscapes.



## References

GBIF.org (2024) GBIF Home Page. Available from: <https://www.gbif.org>
[25 March 2024].
