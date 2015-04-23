# LatentClassPlots

LatentClassPlots is an R-function intended to draw response probability plots from Latent-Class-Analyses (LCA) estimated in Mplus. 

## Arguments

To draw the plot from an LCA output the only required argument is the `data`-argument. Providing on this will result in a plot using the presets of the additional arguments.

The second argument (`lines`) refers to the helplines which will be drawn in the plot. This can be 'h' for horizontal helplines every .1 units, 'v' for vertical helplines every item, or 'b' to draw both. This will default to drawing both types of helplines.

The third argument (`panels`) is used to determine the layout of the plot. This needs to be a matrix that is passed to the `layout()` function of base R. Please see the help-page of `layout()` for more details on the characteristics of this matrix. If no matrix is provided, a square matrix will be used for paneling.

The fourth argument (`proportions`) is a logical to select whether or not the class proportions will be printed as the plot-titles (provided via the `main`-argument of the `plot()`-function in base R). This defaults to `TRUE`, indicating that proportions will be printed.

The final argument (`items`) is used to provide the item names to be printed on the x-Axis of the plot. If this argument is left empty, the variable names will be read from the Mplus-output.