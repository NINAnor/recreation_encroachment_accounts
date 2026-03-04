# recereation_encroachment_accounts

## Purpose of this document
This document gives brief information on the context and purpose of the project on encroachment in recreation areas. It also provides basic information on how to run the code, the data, packages and functions used.

## Project
This is an internal project supported by the MAREA project. Its goal is the compilation of encroachment accounts within recreation areas and especially valuable nature in Norwegian municipalities.


## Structure of the R project
This project has 3 sub-folders:

- .docs where all document related to the project are stored.

- .src includes all the R scripts used in this project.

- .test is used to store any test run of the code.

- .results contains all the results if any.

## Data
We use a number of datasets including:

- Outdoor recreation areas from [naturebase.no](https://geocortex02.miljodirektoratet.no/vertigisstudio/web/?app=a3a09afee5c24c459c53a9a9ff0915f1). Go to Frisluftsliv > Kartlagte og verdsatte friluftslivområder > Kartlagte friluftslivområder, verdi.

- Land take data from Zander and NRK [Zenodo repository](https://zenodo.org/records/15130992), version 1 (3/04/2025).

- Eurostat administrative boundaries for municipalities in Norway [Local Administrative Units](^https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units/local-administrative-units$)

- Sepcially important nature types [Naturtyper - verdsatte](^https://kartkatalog.geonorge.no/metadata/naturtyper-verdsatte/64cbb884-a19d-4356-a114-380cfe4a7314$)


## Packages
```{r}
library(knitr)
library(tidyverse)
library(kableExtra)
library(here)
library(anybadger)
library(yaml)
library(tibble)
library(conflicted)
library(httr2)
library(giscoR)
library(zen4R)
library(jsonlite)
library(sf)
library(purrr)
library(numform)
library(DT)
library(tmap)
```


## Functions
A lot of different functions have been created to make the core code more easy to read and follow. These functions can be found in .src/functions:
- api_zenodo.R: download a Zenodo dataset through an API request.
- api_geonorge: request data from Geonorge with an API request.
- add_change_encroachment_area.R: add a net change row to an extent account table for encroachment areas.
- add_change_recreation_area.R: add a net change row to an extent account table for recreation areas.
- add_fylke.R: add fylke number and / or names.
- add_uncertainty:individual.R: add the traffic light uncertainty at polygon level.
- concatenate_individual_uncertainties.R: aggregate the polygon-level uncertainty to the municipal / county / national level.
- add_uncertainty_global.R: compile the final uncertainty (traffic light) of the results.
- global_uncertainty.R: function used within add_uncertainty_global.R.
- interactive_map.R: creates an interactive map.
- static_map.R: create a static map.
- map_nina_format.R: format the spatial data created with the `sf package`so they can be displayed proprely on maps.nina.no.

## Code
Five core code scripts have been created as quarto document (.qmd), one for each encroachment account or indicator that were of interest in this project. They are accessible along with its HTML version in .src/code. The quarto document has been adapted from an internal NINA quarto template from the ecRix project. It was initially created by Anders Kolstad.

The quarto documents and HTML versions contain both the code but a detailed explanation of the different steps and the metadata of the data used for each account and / or indicator:

- **encroachment_all_municipalities.qmd**: (i) total land take within counties and municipalities; (ii) area and type of specially important nature lost within counties and municipalities.
- **encroachment_recreation_areas.qmd**: same as for encroachment_all_municipalities.qmd BUT for outdoor recreation areas.
- **accounting-tables_display.qmd**: display accounting tables for users. Links to the interactive table sin the NINA report 2754 relate to this file.
