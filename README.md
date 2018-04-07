---
title: "R Package **vigindex**"
author: "Russell V. Lenth"
date: "April 6, 2018"
output: html_document
---
This package has one function, `vigindex`, that allows a package developer
to add an index of topics covered in Markdown vignettes. Special tags may
be added to the vignette `.Rmd` files, and those tags are then used to 
create entries in the index. 

## Index tags
Index tags follow a special format within HTML comment lines, such as:
```
<!-- @index Regression -->
   ... lines of text ...
### Statistical models {#models}
<!-- @index Models!Fixed-effects; Models!Mixed -->
```
This will create main index extries for Regression and Models;
the latter will have sub-entries for Fixed-effects and Mixed. The
index will contain links to the closest anchor preceding
the tag in the vignette; in this example, the Models entries are
linked to the `#models` anchor, and Regression is linked to
the top of the vignette (assuming there were no anchors before
the one shown). Entries are alphabetized without regard to case or
formatting, and there is a way to specify an alternative sorting key.
See `help("vigindex")` for more details.

## Creating the index
Just run
```
vigindex::vigindex()
```
and a new vignette named `vignette-topics.Rmd` is created in the 
package's `vignettes` directory.

## Installation
Currently, this is not on CRAN. To install the package, first
install the **devtools** package (Windows users also need
to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)).
Then do:
```
devtools::install_github("rvlenth/vigindex")
```
