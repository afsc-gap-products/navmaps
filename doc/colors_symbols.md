# Color abd symbol palettes

*In development (1/7/2023)*

Colors and symbols vary among navigation software, as do approaches for setting colors and symbols. The navmaps package simplifies color and symbol selection for each software by using a shared set of names for colors and symbols that can be passed to functions that return the corresponding color or symbol used by the software. For example, the color red is assigned as 255 in Globe, ffff0000 or 1 in TimeZero (depending on the output file format; gpx or kml), and Red for OpenCPN. For symbols, "circle1" is 5 in Globe, http://www.maxsea.fr/TimeZero/Images/Icons/MaxSea_Rec_51.png or 51 in TimeZero (depending on the output file format; gpx or kml), and Symbol-Circle-Black in OpenCPN.

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