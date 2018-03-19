VegX
================

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

## R package to use the Veg-X standard

Functions to import, integrate, harmonize and export vegetation data
using the VegX standard (ver. 2.0.0)

### Installation

``` r
devtools::install_github("miquelcaceres/VegX", build_vignettes=TRUE)
```

## References

  - Wiser SK, Spencer N, De Caceres M, Kleikamp M, Boyle B & Peet RK
    (2011). Veg-X - an exchange standard for plot-based vegetation data.
    *Journal of Vegetation Science* 22: 598-609.
