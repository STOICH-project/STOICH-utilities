# STOICH-Utilities Examples

## Options for Loading Data

```R
# Load required packages.
library(tidyverse)
library(stoichUtilities)

# Three options for creating a variable to store the path to the STOICH data.
# With a text string.
basePath <- "C:/Users/chad/Documents/data/STOICH_Release_2025-09-10"
# Building the path starting at your Documents directory.
basePath <- file.path(path.expand("~"), "data", "STOICH_Release_2025-09-10")
# Or if you set the working directory to point to the data.
setwd(basePath)
basePath <- getwd()

# Load the STOICH data (using a predefined path variable)
stoichData <- stoichUtilities::loadSTOICH(dataPath=basePath)
```

## Explore the Data Structure

```R
# After loading the data stoichData is now a list of tables. To see the names of those tables use names.
names(stoichData)

# Print the first 5 rows of the Source table.
stoichData[["tbl_Source"]] |> slice_head(n=5)
```