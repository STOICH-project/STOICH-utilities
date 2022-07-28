# STOICH-utilities

The STOICH-utilities R package provides user tools for working with STOICH database files. This package is designed to enable easier and more robust access to the data files from STOICH database releases.

## Getting Started
The STOICH API "stoichUtilities" is an R library, as such you will need to have R installed. It is recommended to install RStudio (https://www.rstudio.com/) to help manage packages and provide an integrated development environment (IDE).
## Installation
### Download Source Files
There are 2 options to get the source files:
* From the GitHub project page click the green Code button and choose "Download Zip"
* Use GitHub Desktop or CLI programs to Clone the STOICH-Utilities repository (click the green Code button to copy the URL from the GitHub project page).
### Open and Build the R Library
Navigate to the source code location and open stoichUtilities\stoichUtilities.Rproj in RStudio.

Build the documentation: *ctrl* + *shift* + *D* or with the menu **Build** -> **Document**

Install the library and restart the R session: *ctrl* + *shift* + *B* or with the menu **Build** -> **Install and Restart**

### Installing Required Packages
The stoichUtilities uses several other packages. Install the required packages.
```R
install.packages(“tidyverse”)
install.packages(“lubridate”)
install.packages(“sf”)
install.packages(“units”)
```

## Using the R Library
### Load the Required Libraries.
```R
# Tidyverse and other support libraries
library(tidyverse)
library(lubridate)
library(sf)
library(units)

# Load stoichUtilities
library(stoichUtilities)
```
### Working with STOICH Data
```R
# Create a variable to store the user's home directory
basePath <- do.call(file.path, as.list(str_split(Sys.getenv("HOME"), "\\\\")[[1]]))

# Load the STOICH data (assumed to be in a directory named "data" inside the home directory)
stoichData <- stoichUtilities::loadSTOICH(dataPath=file.path(basePath, "data"))

# Filter the STOICH data
stoichFiltered <- stoichUtilities::filterSTOICH(dataTables=stoichData, var="State", val="FL", condition="Equal")

# Match organism stochiometry data with water chemsitry data for samples that weren't taken at the exact same time
stoichPaired <- stoichUtilities::locateDataPairsSTOICH(stoichData, timeDiff=2, timeUnits="weeks", distance=5, pairMethod="Min Time", ignoreExisting=TRUE)

# Join all the tables into one large wide table
stoichTable <- stoichUtilities::joinSTOICH(stoichPaired)
```
### Documentation
You can access documentation for any function using the help command.
```R
help(loadSTOICH)
```
