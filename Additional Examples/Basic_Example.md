# **Decide how to work with the STOICH DB**

There are 2 options for working with the STOICH data. You can download the fully joined csv file or use the database tables with the R package stoichUtilities.

## **Fully joined csv.**
Download the fully joined csv file at https://doi.org/10.6073/pasta/d63489af723aec3b4a608c54ba7d636d.

If you are using R you can load it with one of the following:
```R
# with the tidyverse
library(tidyverse)
stoichData <- read_csv(file.path(path.expand("~/../Downloads"), "STOICH_Release_Full_Join_20251211.csv"), guess_max = Inf)

# or with base R:
stoichData <- read.csv(file.path(path.expand("~/../Downloads"), "STOICH_Release_Full_Join_20251211.csv"), guess_max = Inf)
```

## **Using the database tables and stoichUtilities**

Download the data from https://snr-stoich.unl.edu/Data/STOICH_Release_2025-12-11.zip, extract it on your computer and make a note of the path. You will have to provide the path to the data when calling the loadSTOICH function.

### **Load the Required Libraries.**

```R
# Install stoichUtilities
install.packages("stoichUtilities")
```

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

basePath <- file.path(path.expand("~"), "data", "STOICH_Beta_Release_2025-12-11")

# Or with a text string.

basePath <- "C:/Users/peter/Documents/data/STOICH_Beta_Release_2025-12-11"

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

