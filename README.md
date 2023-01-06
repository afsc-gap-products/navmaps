# navmaps

This package generates geospatial layers for marine navigation and GIS software for use during bottom trawl surveys. The package retrieves and loads spatial data, conducts data processing steps to prepare layers for software, then writes data from simple features objects with well-known text (WKT) geometries to .kml, .gpx, .mdb, .accdb, and .shp formats that are used by Globe, OpenCPN, TimeZero, and ArcMap.


# Installation

Installation methods depend on whether you will be reading or writing Globe MS Access database files (.mdb and .accdb). Reading or writing from .mdb/.accdb requires a 32-bit version of R with Microsoft Access drivers for RODBC. The R Project stopped producing 32-bit releases as of R version 4.2; the last 32-bit release was version 4.1.3.

### Regular installation

This is the preferred installation method if you won't be reading or writing from .mdb and .accdb files. I'd suggest installing this for your latest version of R if you're producing layers for software other than Globe.

```
library(remotes)
install_github("sean-rohan-NOAA/navmaps", auth_token = gh::gh_token())
```


### Installation for 32-bit R (R version <= 4.1.3) 

If you already have R version 4.1.0-4.1.3 and Rtools40 installed, you can skip to step 3.

1. Install 64-bit and 32-bit versions of [R Version 4.1.X](https://cran.r-project.org/bin/windows/base/old/4.1.3/)
2. Install [Rtools40](https://cran.r-project.org/bin/windows/Rtools/rtools40.html).
3. Open the 64-bit installation of R 4.1.X.
4. If purrr and rlang are already installed, uninstall them using remove.packages()
5. Install the rlang package.
6. Install the purrr package (the binary, __DO NOT COMPILE/BUILD FROM SOURCE__).
7. Install the navmaps package:
```
library(remotes)
install_github("sean-rohan-NOAA/navmaps", auth_token = gh::gh_token())
```
8. Close the 64-bit installation of R.
9. Open the 32-bit installation of R that has the same version number as the 64-bit installation you used to complete the steps above. 
10. Check that the proper drivers are installed:

```
library(navmaps)

# Should return an error if drivers aren't detected or do nothing if .mdb or .accdb drivers are installed.
.check_driver()

# You can also view the list of drivers. The one you'll need is named 'Microsoft Access Driver (*.mdb)'
odbc::odbcListDrivers()
```


# Writing simple features objects to marine navigation software files

## POINTS to .gpx waypoint file

GPX point files are supported in OpenCPN and TimeZero.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

grid_centers <- sf::st_centroid(map_layers$survey.grid)

grid_centers[c('longitude', 'latitude')] = sf::st_coordinates(grid_centers)

# Assign TimeZero color for .gpx
grid_centers$shape = tz_pal(values = "yellow", type = "gpx")
grid_centers$color = 1

sf_to_gpx_waypoints(x = grid_centers, 
          file = here::here("output", "test_marks.gpx"), 
          name_col = "STATIONID",
          description_col = "STATIONID",
          color_col = "color", 
          shape_col = "shape")
```

## sf POINTS to .kml points file

KML point files are supported in TimeZero.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

grid_centers <- sf::st_centroid(map_layers$survey.grid)

grid_centers[c('longitude', 'latitude')] = sf::st_coordinates(grid_centers)

# Assign TimeZero color for .kml
grid_centers$shape = tz_pal(values = "yellow", type = "kml")
grid_centers$color = 1

sf_to_gpx_points(x = grid_centers, 
          file = here::here("output", "test_marks.kml"), 
          name_col = "STATIONID",
          description_col = "STATIONID",
          color_col = "color", 
          shape_col = "shape")
```


## sf LINESTRINGS to .kml linestring file

KML linestring files are supported in TimeZero and ArcMap.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

bathy_layer <- map_layers$bathymetry

# Assign TimeZero color for kml
bathy_layer$color <- tz_pal(values = "blue", type = "kml")

sf_to_kml_linestring(x = bathy_layer,
                     name_col = "METERS",
                     description_col = "LAST_UPDAT",
                     color_col = "color",
                     file = here::here("output", "test_lines.kml"))
```


## sf POLYGON/MULTIPOLYGON to .kml polygon

KML polygon files are supported in TimeZero and ArcMap.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

strata <- map_layers$survey.strata
strata$color <- tz_pal(values = "blue", type = "kml")
strata$fill <- 0 # No fill

sf_to_kml_polygon(x = strata,
                  name_col = "Stratum",
                  description_col = "SURVEY",
                  color_col = "color",
                  fill_col = "fill",
                  file = here::here("output", "test_polygon.kml"))
```

## sf LINESTRING, MULTILINESTRING, POLYGON, or MULTIPOLYGON to .gpx track

These files work in OpenCPN and TimeZero.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

bathy_layer <- map_layers$bathymetry
bathy_layer$color <- sample(x = 1:11, 
                            size = nrow(bathy_layer ), 
                            replace = TRUE)

sf_to_gpx_track(
  x = bathy_layer,
  name_col = "METERS",
  description_col = "LAST_UPDAT",
  file = here::here("output", "test_lines.gpx"))

strata <- map_layers$survey.strata

sf_to_gpx_track(
  x = strata,
  name_col = "Stratum",
  description_col = "SURVEY",
  file = here::here("output", "test_polygon.gpx"))
```


## Color palettes

Color palettes and naming conventions differ among marine navigation software. The navmaps package includes color palette functions to simplify color selection and promote consistency among software. 

Each supported software has an associated color palette function in navmaps. Color names (e.g. "red", "cyan") in the functions are shared among color palettes for each navigation software. For example, yellow, lightgreen, and darkgreen are both in tz_pal and globe_pal:

```
tz_pal(values = c("yellow", "lightgreen", "darkgreen"), type = "hex")
globe_pal(values = c("yellow", "lightgreen", "darkgreen"), type = "hex")
```

Setting the 'type' argument allows color pallete functions to return colors that are formatted for a marine navigation software file format:

```
# TimeZero
tz_pal(values = c("yellow", "lightgreen", "darkgreen"), type = "gpx")
tz_pal(values = c("yellow", "lightgreen", "darkgreen"), type = "kml")

# Globe
globe_pal(values = c("yellow", "lightgreen", "darkgreen"), type = "integer")
globe_pal(values = c("yellow", "lightgreen", "darkgreen"), type = "decimal")
```

See documentation for each palette function for type and color options (`help("globe_pal")`). You can also view colors using the show_col_nav() function:

```
show_col_nav(colors = tz_pal(n = Inf, type = "hex"),
             custom_labels = paste0("", tz_pal(n = Inf, type = "names"), "\n",
                                    "gpx: ", tz_pal(n = Inf, type = "gpx"), "\n",
                                    "kml: ", tz_pal(n = Inf, type = "kml")
                                    ),
             main = "TimeZero Colors for .gpx and .kml"
)

show_col_nav(colors = globe_pal(n = Inf, type = "hex"),
             custom_labels = paste0("", globe_pal(n = Inf, type = "names"), "\n",
                                    "dec: ", globe_pal(n = Inf, type = "decimal"), "\n",
                                    "int: ", globe_pal(n = Inf, type = "integer")
             ),
             main = "Globe Colors for .mdb and .accdb"
)
```

In some cases, there are slight differences between colors shown in R and colors that are plotted in marine navigation software. The differences occur because color palettes are mapped to default color options in navigation software, which differs among programs. For simplicity, the package is designed so 'close' colors are substituted rather than having different named colors for each software.


## NOAA README
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## NOAA License
Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.
<br>
<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>
<br>
[U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)
