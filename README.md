
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- DOI badges go here -->
<!-- badges: end -->

This repository contains data and code for a manuscript by Eric Scott
and Emilio Bruna written for submission to The American Naturalist
tentatively titled: \_\_\_\_\_.

The most recent draft of the manuscript is available
[here](https://brunalab.github.io/lagged-ipms/paper.html).

[Notes for collaborators](notes/index.html)

# Reproducibility

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip file from from this URL:
[master.zip](/archive/master.zip).

To run the compendium and reproduce all outputs:

-   Open the project in RStudio by double-clicking the `.Rproj` file.
-   Install packages listed in `packages.R` (if this compendium uses
    `renv`, this may happen automatically or with `renv::restore()`).
-   Install the [`targets` package](https://docs.ropensci.org/targets/).
-   Run `targets::tar_make()` or `targets::tar_make_clustermq()` from
    the R console to run all code and produce all outputs.

<!-- Consider using `targets::tar_github_actions()` if targets aren't too big -->
