#### jointcenteR overview ####

### Package description with examples-----
# https://github.com/airbrycki/jointcenteR/blob/main/README.md

### Package manual-----
# https://github.com/airbrycki/jointcenteR/blob/main/man/jointcenteR_0.0.0.9000_full_manual.pdf

### Install package----
# if you don't already have devtools installed, do this once:
# install.packages("devtools")
devtools::install_github("airbrycki/jointcenteR")
library(jointcenteR)

### Loading data examples-----

## load one year of ACS data
# define the path where .csv is saved
# (tip: save this as a code snippet in global options so you don't have to
# type it out or find it every time Tools > Edit Code Snippets)
acspath <- "C:/Users/wla131/Harvard University/SON rental chapter - Documents/2022 Rental/Data Sources/ACS/"

# use load function
# takes inputs of a year and file path where ACS .csv files are located and
# loads the corresponding American Community Survey file into the
# global environment
load_acs(yr = 2023, path = acspath)
# load_acs(2023) does the same thing if your path is already saved as acspath

# to view help documentation in RStudio, type:
?load_acs

# to view underlying function code, type:
load_acs

# the map function from the purrr package makes it easy to load multiple
# years at once
purrr::map(c(2019, 2023), load_acs, path = acspath)

## similar process for metro file
load_met(2023, acspath)

## load multiyear ACS file - this is just a wrapper
multfilename <- "ACS_multiyear_hhplus_RentersPlusVacant.csv"
load_mult(acspath, multfilename)

## extract specific years of ACS file
split_mult(2001)

# extract multiple years
purrr::map(c(2001, 2010), split_mult)

# cleanup before moving on
rm(acs2001, acs2010, met2023, multiyear, multfilename)
gc()

## add labels to ACS file
dict_url <- "https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2023.csv"
acs2023_l <- add_ACSlabels(acs2023, dict_url)

## add JCHS labels to file
acs2023_l <- add_JCHSlabels(acs2023)

# labels will show up in tabulations then - here's a comparison of labeled vs.
# unlabeled
acs2023_l |>
  dplyr::filter(!is.na(as.integer(tensimp))) |>
  dplyr::group_by(tensimp) |>
  dplyr::summarise(tot = sum(wgtp))

acs2023 |>
  dplyr::filter(!is.na(tensimp)) |>
  dplyr::group_by(tensimp) |>
  dplyr::summarise(tot = sum(wgtp))


### Get and process public data examples-----
## pull housing vacancy survey data
hvs <- get_hvs()

## pull construction data
nrc <- get_nrc(type = "annual")

## pull permit data
stp23 <- get_permits("state", 2023)

## process ahar data - first step is downloading .xlsb and
# converting it to a .xlsx saved locally
path <- "C:/Users/wla131/Harvard University/SON rental chapter - Documents/2022 Rental/Data Sources/AHAR/"
file <- "2007-2024-PIT-Counts-by-State.xlsx"
ahar <- process_ahar(2023, paste0(path, file))

aharlist <- purrr::map(2007:2023, process_ahar, paste0(path, file))
ahar <- aharlist |>
  purrr::reduce(dplyr::left_join, by ="State")

# cleanup
rm(aharlist, ahar, hvs, nrc, stp23)

### Tabulate and inflate estimates examples----
## weighted estimates
tab(acs2023_l, region, tensimp, w = wgtp)

# or using piped argument
acs2023_l |>
  dplyr::filter(as.integer(tensimp) == 2) |>
  tab(region, cost_burden, w = wgtp)

## unweighted estimates
tab(acs2023_l, tensimp, region)

# calculate weighted medians (note: just a wrapper, doesn't work for assigning
# values to a variable in piped statement)
wtd_med(acs2023_l, hincp, tensimp, w = wgtp)
# or in piped statement
acs2023_l |>
  dplyr::filter(!is.na(tensimp)) |>
  dplyr::group_by(tensimp) |>
  wtd_med(hincp, w = wgtp)

## inflate value using CPI-U for All Items
load_acs(2019)
acs2019 |>
  dplyr::filter(!is.na(tensimp)) |>
  dplyr::mutate(hincp_infl_23 = inflate_ai(hincp, unique(year), 2023)) |>
  dplyr::group_by(tensimp) |>
  dplyr::summarise(med_hincp_infl = Hmisc::wtd.quantile(hincp_infl_23,
                                                        probs = 0.5, w = wgtp),
                   med_hincp = Hmisc::wtd.quantile(hincp, probs = 0.5, w =wgtp))

# or to just do an on the fly calculation
inflate_ai(50000, 2019, 2023)

##inflate value using CPI-U Less Shelter
acs2019 |>
  dplyr::filter(!is.na(grntp)) |>
  dplyr::mutate(grntp_infl_23 = inflate_ls(grntp, unique(year), 2023)) |>
  dplyr::summarise(med_grntp_infl = Hmisc::wtd.quantile(grntp_infl_23,
                                                        probs = 0.5, w = wgtp),
                   med_grntp = Hmisc::wtd.quantile(grntp, probs = 0.5, w =wgtp))

# or to just do an on the fly calculation
inflate_ls(1000, 2019, 2023)


### Export data examples-----
# split a chapter into individual rows for factchecking
# first save word doc as .txt using Unicode (UTF-8) encoding
chapter1 <- factcheck_format("C:/Users/wla131/Desktop/testtext.txt")
# can then use the export function to
export_table(chapter1, "factcheck.xlsx", "factcheck.xlsx")

# export table or object to an excel sheet
cost_burdens <- acs2023_l |>
  dplyr::filter(as.integer(tensimp) == 2) |>
  tab(cost_burden, w = wgtp)

export_table(cost_burdens, "factcheck.xlsx", "factcheck.xlsx")





