# navmaps

This package generates geospatial layers for marine navigation and GIS software for use during bottom trawl surveys. The package retrieves and loads spatial data, conducts data processing steps to prepare layers for software, then writes data from simple features objects with well-known text (WKT) geometries to .kml, .gpx, .mdb, .accdb, and .shp formats that are used by Globe, OpenCPN, TimeZero, and ArcMap.

# Resources

-   [Installation](#installation)
-   [Map layers and data sources](#data-sources)
-   [Creating navigation layers](./doc/make_navigation_files.md)
-   [Setting colors and symbols](./doc/colors_symbols.md)
-   [How to add new navigation software](./doc/howto_add_new_file_formats.md)
-   [Historical GLOBE repository](https://github.com/afsc-gap-products/Globe)
-   [Download and format moorings data](./doc/import_lnm_moorings_data.md)

# Installation {#installation}

Installation methods depend on whether you will be reading or writing Globe MS Access database files (.mdb and .accdb). Reading or writing from .mdb/.accdb requires a 32-bit version of R with Microsoft Access drivers for RODBC. The R Project stopped producing 32-bit releases as of R version 4.2; the last 32-bit release was version 4.1.3.

### Regular installation

This is the preferred installation method if you won't be reading or writing from .mdb and .accdb files. I'd suggest installing this for your latest version of R if you're producing layers for software other than Globe.

    library(remotes)
    install_github("afsc-gap-products/navmaps", auth_token = gh::gh_token())

### Installation for 32-bit R (R version \<= 4.1.3)

If you already have R version 4.1.0-4.1.3 and Rtools40 installed, you can skip to step 3.

1.  Install 64-bit and 32-bit versions of [R Version 4.1.X](https://cran.r-project.org/bin/windows/base/old/4.1.3/)
2.  Install [Rtools40](https://cran.r-project.org/bin/windows/Rtools/rtools40.html).
3.  Open the 64-bit installation of R 4.1.X.
4.  If purrr and rlang are already installed, uninstall them using remove.packages()
5.  Install the rlang package.
6.  Install the purrr package (the binary, **DO NOT COMPILE/BUILD FROM SOURCE**).
7.  Install the navmaps package:

```{=html}
<!-- -->
```
    library(remotes)
    install_github("afsc-gap-products/navmaps", auth_token = gh::gh_token())

8.  Close the 64-bit installation of R.
9.  Open the 32-bit installation of R that has the same version number as the 64-bit installation you used to complete the steps above.
10. Check that the proper drivers are installed:

```{=html}
<!-- -->
```
    library(navmaps)

    # Should return an error if drivers aren't detected or do nothing if .mdb or .accdb drivers are installed.
    .check_driver()

    # You can also view the list of drivers. The one you'll need is named 'Microsoft Access Driver (*.mdb)'
    odbc::odbcListDrivers()

### Troubleshooting 32-bit installation

If the 32-bit installation fails:

1.  In R Studio, navigate to Tools \> Global Options.
2.  Switch to a 32-bit installation of R.
3.  Restart R Studio and check that the start-up messages show you're using a 32-bit version of R.
4.  Clone the navmaps repository locally.
5.  Create a new R Studio 'R Package' project in your local navmaps directory.
6.  Install the package using Build \> Install (Ctrl+Shift+B); you may need to first install dependencies individually.

# Map Layers and Data Sources

| Layer Name                                       | Description                                                                                                | Type                                       | Regions           | Sources                                                                                                                     | Update |
|------------|------------|------------|------------|------------|------------|
| Survey Strata                              | Survey stratum polygons                                                                                    | Lines or polygons                          | EBS, NBS, AI, GOA | [akgfmaps](https://github.com/afsc-gap-products/akgfmaps) package  |  See [akgfmaps releases](https://github.com/afsc-gap-products/akgfmaps/releases) |
| Survey Grid                                | Survey grid polygons with station name and stratum info.                                                    | Lines or polygons                          | EBS, NBS          | [akgfmaps](https://github.com/afsc-gap-products/akgfmaps) package                                                                                                          | See [akgfmaps releases](https://github.com/afsc-gap-products/akgfmaps/releases) |
| Survey Stations                            | Survey station centroids with station name and stratum info.                                                | Marks                                      | EBS, NBS          | [akgfmaps](https://github.com/afsc-gap-products/akgfmaps) package                                                                                                          | See [akgfmaps releases](https://github.com/afsc-gap-products/akgfmaps/releases) |
| Trawlable/Untrawlable Grid                 | Survey station grid with station name, stratum, and trawlable/untrawlable/unknown designation.             | Lines or polygons                          | AI, GOA           | [akgfmaps](https://github.com/afsc-gap-products/akgfmaps) and GOA/AI station tables                                                                                  | Annual            |
| Trawlable/Untrawlable Stations             | Survey station centroids with station name, stratum, and trawlable/untrawlable/unknown designation.  | Marks | AI, GOA | [akgfmaps](https://github.com/afsc-gap-products/akgfmaps) package, GOA/AI station tables | Annual  |                                                                                                                             |                   |
| Historical Tow Starts                      | Historical tow starts with performance code, year/cruise, and vessel info.                                 | Marks                                      | EBS, NBS, AI, GOA | race_base_data.hauls, race_data.events, race_data.surveys, race_data.survey_definitions, racebase.performace                | Annual            |
| Historical Tow Midpoint                    | Historical tow midpoint calculated from GPS data. Includes performance code, year/cruise, and vessel info. | Marks                                      | AI, GOA           | race_data.hauls, race_data.cruises, race_data.events, race_data.surveys, race_data.survey_definitions, racebase.performance | Annual            |
| Historical Towpaths                        | Historical towpaths based on smoothed GPS data. Includes performance code, year/cruise, and vessel info.   | Lines                                      | EBS, NBS, AI, GOA | race_data.cruises, race_data.hauls, race_data.position_headers, race_data.positions, race_data.datum_codes                  |                   | Annual |
| Station Allocation                         | Station allocation by vessel for the AI or GOA surveys.                                                    | Marks                                      | AI, GOA           | [StationAllocationAIGOA](https://github.com/afsc-gap-products/StationAllocationAIGOA) | Annual  |
| Oceanographic Moorings (buoys)             |  Location, depth, and contact information about oceanographic moorings. |  Marks | EBS/NBS, AI, GOA  |  USCG. See [Download and format moorings data](./doc/import_lnm_moorings_data.md) | Annual (last update: April 1, 2023) |
| Stellar Sea Lion No Transit Zones | No-transit zones around Steller Sea Lion Haulouts. | Lines or polygons | EBS, AI, GOA | AFSC Permits Coordinator | Last update: April 2022 | 
| Humpback Whale Critical Habitat            | Humpback Whale Critical Habitat areas.  | Lines or polygons  | EBS, AI, GOA | NOAA Fisheries. [Link](https://www.fisheries.noaa.gov/resource/map/humpback-whale-critical-habitat-maps-and-gis-data)   | Last update: March 23,2023 |
| North Pacific Right Whale Critical Habitat | North Pacific Right Whale Critical Habitat areas | Lines or polygons | EBS/NBS, AI, GOA  | NOAA Fisheries. [Link](https://www.fisheries.noaa.gov/resource/map/north-pacific-right-whale-critical-habitat-map-and-gis-data) | Last update: April 2022 |
| Sea Otter Critical Habitat | Sea Otter Critical Habitat areas.  | Lines or polygons  | EBS/NBS, AI, GOA  | USFWS. [Link](https://ecos.fws.gov/ecp/species/2884) |  |
| Spectacled Eider Critical Habitat          | Spectacled Eider Critical Habitat in the northern Bering Sea | Lines or polygons | EBS/NBS | USFWS. [Link](https://ecos.fws.gov/ecp/species/762) | Last update: April 2, 2023 |
| Crab storage                               | Storage locations for crab pot strings. |  Lines or polygons | AI, GOA | @MarkZimmermann-NOAA. Provided by individual vessels.  | Annual |

## NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States. <br> <img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/> <br> [U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)
