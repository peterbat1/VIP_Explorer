# VIP_Explorer

VIP_Explorer is an R Shiny app based on Beaumont et al. (in review) which allows users to examine relationships between complementarity, climate change and land use impacts beyond that possible in the source publication.

## Running the app
There are two ways to run the app: temporary download using the function *runGitHub()* in the _**R**_ shiny package, and permanently downloading the full archive to your computer and running it locally.

> Note: The total volume of files associated with the app is about 42MB

### Temporary download method
Make sure that the following _**R**_ packages have been installed in your local _**R**_ installation:

- shiny
- shinybusy
- shinyscreenshot
- raster
- leaflet
- leafem
- sf
- jsonlite
- geojsonsf

Enter the following commands in the R console or the console panel of _RStudio_:

    library(shiny)
    runGitHub("peterbat1/VIP_Explorer")

The app will be downloaded (be patient, it's a big data set!) and should open in a browser tab. **The app will run as long as you do not close your browser or browser tab.**

If you do close the browser or browser tab, or close the _**R**_ console, or RStudio, you will have to download a new copy to run it again.

### Permanent download method
Make sure that the following _**R**_ packages have been installed in your local _**R**_ installation:

- shiny
- shinybusy
- shinyscreenshot
- raster
- leaflet
- leafem
- sf
- jsonlite
- geojsonsf

Click on the "Code" button and select "Download ZIP" option.

When it has downloaded, place the file in a folder *above* the level you would like the program folder to be located.

Extract the zipped archive which will create the folder "VIP_Explorer".

Within VIP_Explorer you will see the file _app.R_ which can be loaded into RStudio and run by clicking on the "Run app" option.

Alternatively, you can run it in the _**R**_ console by typing the command:

    runApp("/path/to/VIP_Explorer/app.R")

substituting, of course, the path to the VIP_Explorer folder on your computer for "/path/to".

The app will close if you close your browser, or browser tab, or close RStudio. However, you can restart the app at any time without having to wait for a fresh download.


