# How to add file formats for new software:

1.  Inspect the structure of input files for the new software.
2.  Add new software option as a character string to .check_software(valid_software = [software]).
3.  Write new sf_to\_[output] file writing functions if necessary. Use the same structure as other sf_to\_[output] functions.
4.  Create new conditional statements to sf_to_nav_file() to pass sf objects and additional arguments to the proper sf_to\_[output] function for the new software format.
5.  Set mark and line file extension names for the new software in set_file_type(). It's possible that new file types may need to be added if polygons and lines require different file formats.
6.  If necessary, create new symbol and color palettes for the software in colors_symbols.R, using the same structure as existing [software]\_pal() and [software]\_sym_pal() functions. Aim to use the same shape and color naming conventions as other palettes.
7.  Add new symbol and palette functions to navmaps_sym_pal() and navmaps_pal().
8.  If necessary, add new software-specific data handling and manipulation code to functions in make_shapefiles_layers.R.
