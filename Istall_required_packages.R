# Thanks to Egor Kotov (https://github.com/e-kotov/2018-03-21_r_data_mos_ru_adr_reg)
# and Pratik Patil for the package installation code ( http://stackoverflow.com/a/29622385 )

# Specify the list of required packages to be installed and load  
Required_Packages <- c("sp", "raster",                                            # spatial data 
                       "RColorBrewer", "lattice", "latticeExtra", "rasterVis",    # spatial data vis
                       "dplyr", "tidyr", "readr",                                 # data manupulation
                       "rvest",                                                   # http requests and html parcing
                       "stringr",                                                 # text
                       "ggplot2"                                                  # cool plots
)
Install_And_Load <- function(Required_Packages) {
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])]
  if(length(Remaining_Packages)) 
  {
    install.packages(Remaining_Packages, repos = "https://cran.rstudio.com", type = "binary")
  }
  for(package_name in Required_Packages)
  {
    library(package_name, character.only = T, quietly = F)
  }
}
Install_And_Load(Required_Packages) # Call the function to install packages