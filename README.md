
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Research Compendium Template

This is a research compendium template geared towards producing a
manuscript output as a Word document reproducibly using the `targets`
package for workflow management. See the [`targets`
documentation](https://docs.ropensci.org/targets/) for more information
on how to program analyses in this style. I’ve found that writing
`paper.Rmd` using RStudio’s [visual markdown
editor](https://rstudio.github.io/visual-markdown-editing/) and
automatic text wrapping (with each sentence as a new line) works best
for version control and collaboration with git and GitHub. The visual
editor also integrates seamlessly with [Zotero](https://www.zotero.org/)
for adding in-text citations and bibliographies. The [`renv`
package](https://rstudio.github.io/renv/articles/renv.html) my also be
useful for collaboration to keep package version synced across
collaborators. Before submitting the manuscript, I like to [archive my
GitHub repository with
Zenodo](https://guides.github.com/activities/citable-code/) and add a
DOI badge to the README, and a link to the Zenodo archive to the Data
Availability Statement. In the `review` directory, you’ll find my
preferred way of structuring response to reviewers complete with the
ability to cross-reference responses to avoid repeating yourself.

**GithHub Actions** This template also uses [GitHub
actions](https://github.com/features/actions) to automatically render a
.html version of the manuscript every time an edit to docs/paper.Rmd is
made. If [GitHub pages](https://pages.github.com/) are active, then the
rendered draft is viewable at \<username\>.github.io/\<repository
name\>/paper.html. The workflow for this lives in
.github/workflows/build-manuscript.yaml and can be disabled on GitHub in
the “Actions” tab.

Readme text for *your* repository below:

------------------------------------------------------------------------

<!-- badges: start -->
<!-- DOI badges go here -->
<!-- badges: end -->

This repository contains data and code for a manuscript by \_\_\_\_\_\_
written for submission to \_\_\_\_\_\_ tentatively titled: \_\_\_\_\_.

The most recent draft of the manuscript is available
[here](https://aariq.github.io/paper-template/paper.html).

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
-   Run scripts in /R/ in numerical order.
-   Knit /doc/paper.Rmd to produce output

Additional instructions if project uses `targets`:

-   Install the [`targets` package](https://docs.ropensci.org/targets/).
-   Run `targets::tar_make()` or `targets::tar_make_clustermq()` from
    the R console to run all code and produce all outputs.

**NOTE**: the GitHub actions in .github/workflows/build-manuscript.yaml
will not work with a `targets` project. Consider
`targets::tar_github_actions()` as an alternative
