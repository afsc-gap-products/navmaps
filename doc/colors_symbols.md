---
  title: "Setting colors and symbols"
output:
  md_document:
  variant: gfm
---

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