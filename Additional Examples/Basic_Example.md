***Download the data from https://snr-stoich.unl.edu/Data/STOICH_Release_2025-12-11.zip, extract it on your computer and make a note of the path. You will have to provide the path to the data when calling the loadSTOICH function.**

### **Load the Required Libraries.**

```R
# Tidyverse and other support libraries

library(tidyverse)

library(lubridate)

library(sf)

library(units)


# Load stoichUtilities

library(stoichUtilities)
```

### **Loading the STOICH DB and Testing the STOICH-Utilities**

**This is where you need the path to the folder containing the data.**

```R
# Three options for creating a variable to store the path to the STOICH data.

# Building the path starting at your Documents directory.

basePath <- file.path(path.expand("~"), "data", "STOICH\_Beta\_Release\_2025-12-11")

# Or with a text string.

basePath <- "C:/Users/peter/Documents/data/STOICH\_Beta\_Release\_2025-12-11"

# Or if you set the working directory to point to the data.

basePath <- getwd()


# Load the STOICH data (using a predefined path variable)

stoichData <- stoichUtilities::loadSTOICH(dataPath=basePath)


# Filter the STOICH data

stoichFiltered <- stoichUtilities::filterSTOICH(dataTables=stoichData, var="State", val="Florida", condition="Equal")


# Match organism stoichiometry data with water chemsitry data for samples that weren't taken at the exact same time

stoichPaired <- stoichUtilities::locateDataPairsSTOICH(stoichFiltered, timeDiff=2, timeUnits="weeks", distance=5, pairMethod="Min Time", ignoreExisting=TRUE)


# Join all the tables into one large wide table

stoichTable <- stoichUtilities::joinSTOICH(stoichPaired)
```

