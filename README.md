# bathtub
<img src="logo.svg" alt="a" width="172hw" height="200hw" align="right">

`bathtub` is an R package that estimates the impacts of downstream flooding on stormwater networks using a simple bathtub model, a 1-dimensional infrastructure network, and a minimal amount of data. 

The package `bathtub`:

* Models impacts using a **range of water levels** 
* Models impacts using GIS data representing **flooding extents** 
* Estimates various metrics of impact from high water levels on pipes and structures
* Provides multiple visualization functions ranging from plots to interactive maps

`bathtub` was designed to be practical and can: 
* Download and process elevation data from NOAA and USGS within R (DEMs & lidar)
* Interpolate missing invert elevations within the stormwater network
* Automatically take care of unit conversions and spatial data projections
* Easily visualize, save, & share results

## Install

Install `bathtub` from GitHub using `devtools`:
```R
library(devtools)
install_github("acgold/bathtub")
```

## How it works

`bathtub` combines spatial representations of the stormwater network and landscape with a 1-dimensional model of the stormwater network. `bathtub` uses the `sf`, `stars`, and `raster` packages to provide quick and tidy results.

The model works by:

1. Estimating overland flooding extents using a simple bathtub model and digital elevation models (DEMs)
2. Using the overland flooding extent as a starting point for a 1-D model of stormwater infrastructure 
3. Propogating flooding through the stormwater network based on invert elevations

