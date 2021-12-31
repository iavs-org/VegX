VegX
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/iavs-org/VegX/workflows/R-CMD-check/badge.svg)](https://github.com/iavs-org/VegX/actions)
<!-- badges: end -->

## Introduction

A primary technical impediment to large-scale sharing of vegetation data
is the lack of a recognized international exchange standard for linking
the panoply of tools and database implementations that exist among
various organizations and individuals participating in vegetation
research.

## The Veg-X exchange standard

The **Veg-X exchange standard** for plot-based vegetation data (Wiser et
al. 2011) is intended to be used to share and merge vegetation-plot data
of different kinds. Veg-X allows for observations of vegetation at both
individual plant and aggregated observation levels. It ensures that
observations are fixed to physical sample plots at specific points in
space and time, and makes a distinction between the entity of interest
(e.g. an individual tree) and the observational act (i.e. a
measurement). The standard supports repeated measurements of both
individual organisms and plots, and enables the connection between the
entity observed and the taxonomic concept associated with that
observation to be maintained.

### Detailed documentation of Veg-X

Please read article [The Veg-X exchange
standard](https://iavs-org.github.io/VegX/articles/VegXStandard.html) to
know mode about the standard.

### XML schema

Veg-X is written as an **XML schema**, which is a definition of
user-defined tags to structure textual information in order to create
self-describing datasets. XML (Extensible Markup Language) is an open
standard, and XML files are both machine and human-readable (they are
stored in plain-text ASCII format). Visit folder
[vegxschema](https://github.com/iavs-org/VegX/tree/master/vegxschema) to
see the XML schema representation of the standard. This schema should be
used to evaluate whether a given Veg-X XML document conforms to the
definitions and data structure of the standard.

## An R package to use the Veg-X standard

A barrier to the use of a standard like Veg-X is its complexity. To make
the exchange schema of Veg-X usable by the wider community requires the
development of informatics tools for mapping data from different input
formats (e.g. relevé tables from different databases, forest inventory
data or stem-mapped forest plots) into Veg-X, mechanisms to create
unique identifiers to allow source datasets to be combined, and tools to
export data for data analysis and visualisation. The **VegX R package**
has been designed for this purpose. It contains functions to import,
integrate, harmonize and export vegetation data using the Veg-X
standard.

### Package installation

The VegX R package can be installed from GitHub as follows:

``` r
devtools::install_github("iavs-org/VegX", build_vignettes=TRUE)
```

### User manual

Please refer to [How to use the VegX
package](https://iavs-org.github.io/VegX/articles/PackageTutorial.html)
to learn how to use the VegX package.

### Acknowledgements

Developments endorsed and funded by the [International Association for
Vegetation Science](http://iavs.org/)

**Veg-X standard development**:

-   Miquel De Cáceres
-   Sebastian Schmidtlein
-   Christian König
-   Susan K. Wiser
-   Nick Spencer
-   Robert K. Peet
-   Martin Kleikamp
-   Brad Boyle

**VegX R package development**:

-   Miquel De Cáceres
-   Sebastian Schmidtlein
-   Christian König

## References

-   Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK
    (2011). Veg-X - an exchange standard for plot-based vegetation data.
    *Journal of Vegetation Science* 22: 598-609.
    <https://doi.org/10.1111/j.1654-1103.2010.01245.x>
