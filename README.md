# jointcenteR

jointcenteR is a collection of functions to make routine JCHS tasks easier. The package is a combination of functions that are intended to help at each stage of analysis,
from loading data into the environment to quickly running weighted tabulations to easily exporting tables to excel. The ultimate goal of this package is to make it just as
easy for researchers to turn to R as it is to use other programs and to make the learning curve a little less steep. Please note that this package is still in development
and functions will continue to be refined as further testing is completed.

## Installation

You can install the development version of jointcenteR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("airbrycki/jointcenteR")
```

## Load data from JCHS hhplus files

Six functions in the package are for loading local JCHS data files and preparing them for analysis:
* load_acs(): takes inputs of a year and file path where ACS .csv files are located and loads the corresponding American Community Survey file into the global environment
* load_met(): similarly takes year and file path to load ACS metro files into the global environment
* load_mult(): takes inputs of a file path and multiyear file name to load the JCHS 2001-2014 file into the global environment
* split_mult(): extracts year of interest from the multiyear file and puts it in global environment as a new dataframe
* add_ACSlabels(): factors variables in the ACS file using variables pulled directly from the Census data dictionary
* add_JCHSlabels(): factors variables in the ACS file that are JCHS created using hard-coded values

#### Examples
```{r example}
library(jointcenteR)
acspath <- "C:/Data/ACS/"
# load 2023 acs hhplus file into global environment
load_acs(2023, acspath)

# load 2023 acs metro file into global environment
load_met(2023, acspath)

# load JCHS multiyear hhplus file into global environment
multfilename <- "ACS_multiyear_hhplus.csv"
load_mult(acspath, multfilename)
# split 2001 data out of multiyear file into new dataframe
split_mult(2001)

# add census labels to file
dict_url <- "https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2023.csv"
acs2023_l <- add_ACSlabels(acs2023, dict_url)

# add jchs labels to ACS hhplus file
acs2023_l <- add_JCHSlabels(acs2023)
```
## Get and process public data

Four functions make it easier to pull public data and to put it in a tidy format for analysis:
* get_hvs(): retrieves quarterly Housing Vacancy Survey variables
* get_nrc(): retrieves monthly or annual national data from the New Residential Construction survey, including single- and multi-family permits,
  starts, under construction, and completions
* get_permits(): retrieves annual county or state permit data from the Building Permits Survey
* process_ahar(): organizes data files from the Annual Homelessness Assessment Report to enable analysis of state and national trends over time

#### Examples
```{r example2}
library(jointcenteR)
# pull housing vacancy survey data
hvs <- get_hvs()

# pull construction data
nrc <- get_nrc(type = "annual")

# pull permit data
stp23 <- get_permits("state", 2023)

# process ahar data - first step is downloading .xlsb and converting it to a .xlsx saved locally
file <- "C:/Data/2007-2023-PIT-Counts-by-State.xlsx"
ahar <- process_ahar(2023, file)
```

### Tabulate and inflate estimates

Four functions streamline the process for creating weighted or unweighted estimates, calculating weighted medians, or inflating values:
* tab(): produces weighted or unweighted estimate tables with count and share
* wtd_med(): wrapper function that calculates weighted medians
* inflate_ai(): inflates values using CPI-U for All Items
* inflate_ls(): inflates values using CPI-U Less Shelter

#### Examples
```{r example3}
library(jointcenteR)
# weighted estimates
tab(acs2023, region, ten, dis, w = wgtp)
# or using piped argument
acs2023 |>
  dplyr::filter(tensimp == 2) |>
  tab(region, cost_burden, w = wgtp)

# unweighted estimates
tab(acs2023, tensimp, region)

# calculate weighted medians (note: just a wrapper, doesn't work for assigning values to a variable in piped statement)
wtd_med(acs2023, ten, hincp, w = wgtp)
# or in piped statement
acs2023 |>
   dplyr::filter(tensimp == 2) |>
   wtd_med(hincp, w = wgtp)

# inflate value using CPI-U for All Items
acs2022 |>
    mutate(hincp_infl_23 = inflate_ai(hincp, 2022, 2023))
# or to just extract reference year
acs2022 |>
    mutate(hincp_infl_23 = inflate_ai(hincp, unique(year), 2023))
# or to just do an on the fly calculation
inflate_ai(50000, 2022, 2023)

#inflate value using CPI-U Less Shelter
acs2022 |>
    mutate(grntp_infl_23 = inflate_ls(grntp, 2022, 2023))
# or to just extract reference year
acs2022 |>
    mutate(grntp_infl_23 = inflate_ls(grntp, unique(year), 2023))
# or to just do an on the fly calculation
inflate_ls(1000, 2022, 2023)
```

### Export

Two functions are for preparing output and exporting it to excel:
* factcheck_format(): takes a .txt file and splits it at sentence breaks and carriage returns, putting each line in a new row for factchecking
* export_table(): exports an object to an excel file tab and assigns the tab the same name as the object

#### Examples
```{rexample4}
library(jointcenteR)
# split a chapter into individual rows for factchecking
# first save word doc as .txt using Unicode (UTF-8) encoding
chapter1 <- factcheck_format("C:/Data/chaptertext.txt")

# export table or object to an excel sheet
tenTable <- acs2023 |>
  tab(tensimp, w = wgtp)
export_table("C:/Data/tabswb.xlsx", "C:/Data/tabswb.xlsx", tenTable)
```
