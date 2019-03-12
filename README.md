# eBird Target Map

An R Shiny map showing the number of lifers to be found in each country, state in the US and Canada, or county in the contiguous United States. To use this app:

1. Install the latest versions of [R](https://cloud.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/#download). Even if you have these intstalled, install again to ensure they're up to date.
2. [Download the contents of this repository](https://github.com/mstrimas/ebird-target-map/archive/master.zip) and unzip them. Then open the file `ebird-target-map.Rproj` in RStudio.
3. Install the required packages by running the following in the RStudio console: `install.packages(c("shiny", "leaflet", "sf", "dplyr"))`
4. At the console, run `shiny::runApp()` to start the application.
5. Test the application using some of the example datasets in the `data/` folder, or [download your own life list](https://ebird.org/MyEBird?cmd=list&rtype=custom&r=world&time=life&fmt=csv) to look at personalized targets!
