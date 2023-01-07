# Layers, data formats, sources, and descriptions by region


| Layer                                      | AI  | GOA | EBS | NBS | Slope | Plotter Format | Source                   | Description                                                                                                           |
|--------|--------|--------|--------|--------|--------|--------|--------|-------------|
| Historical Tow Paths                       | x   | x   | x   | x   | x     | line           | RACEBASE                 | Line between haul start and end coordinates from RACEBASE.HAUL                                                        |
| Tow Mid                                    | x   | x   |     |     | x     | mark           | RACEBASE                 | Midpoint estimated from RACEBASE GPS tables                                                                           |
| Tow Start                                  | x   | x   | x   | x   | x     | mark           | RACEBASE                 | Start point from RACEBASE.HAUL                                                                                        |
| Station Grid                               |     |     | x   | x   |       | line/boundary  | akgfmaps                 | Regional survey grid shapefiles from akgfmaps or RACEBASE region grids                                                |
| Trawlable/Untrawlable Station Grid         | x   | x   |     |     |       | line/boundary  | RACEBASE + akgfmaps      | Regional survey grid shapefiles from akgfmaps or RACEBASE region grids joined with trawlable/untrawlable designations |
| Station Center Marks                       | x   | x   | x   | x   |       | mark           | akgfmaps                 | Centroid of survey grid shapefiles                                                                                    |
| Station Allocation                         | x   | x   |     |     | x     | mark           | Survey group             | CSV file of station allocation, priority, and vessels created by the GOA/AI group                                     |
| Survey Strata                              | x   | x   | x   | x   | x     | line/boundary  | akgfmaps                 | Regional survey stratum shapefile from akgfmaps                                                                       |
| Stellar Sea Lion No-Transit                | x   | x   |     |     |       | line/boundary  | AFSC Permits Coordinator | ESRI Shapefile                                                                                                        |
| Stellar Sea Lion Buffer                    | x   | x   |     |     |       | line/boundary  | AFSC Permits Coordinator | ESRI Shapefile                                                                                                        |
| Sea Otter Critical Habitat                 | x   | x   |     |     |       | line/boundary  | AFSC Permits Coordinator | ESRI Shapefile                                                                                                        |
| North Pacific Right Whale Critical Habitat | x   | x   | x   | x   | x     | line/boundary  | AFSC Permits Coordinator | ESRI Shapefile                                                                                                        |
| Buoys                                      | x   | x   | x   | x   |       | mark           |                          | CSV                                                                                                                   |
| Crab Pot Storage                           | x   |     |     |     |       | line/boundary  | Vessels (confidential)   | Globe MDB, CSV, emailed coordinates                                                                                   |
