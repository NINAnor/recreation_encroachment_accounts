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

- recreation areas from [naturebase.no](https://geocortex02.miljodirektoratet.no/vertigisstudio/web/?app=a3a09afee5c24c459c53a9a9ff0915f1). Go to Frisluftsliv > Kartlagte og verdsatte friluftslivområder > Kartlagte friluftslivområder, verdi.

- encroachment data from Zander's [Zenodo repository](https://zenodo.org/records/15130992), version 1 (3/04/2025).

- 




## Packages
```{r}
library(sf)
```


## Functions
A lot of different functions have been created to make the core code more easy to read and follow. These functions can be found in .src/functions.

## Code
Five core code scripts have been created as quarto document (.qmd), one for each encroacjment account or indicator that were of interest in this project. They are accessible along with its HTML version in .src/code. The quarto documents and HTML versions contain both the code but a detailed explanation of the different steps and the metadata of the data used for each account and / or indicator:

- **Total encroachment of "especially valuable nature"**: encroachment_valuable_nature.qmd
- **Total encroachment of recreation areas**: encroachment_all_recreation_areas.qmd
- **Total encroachment of especially valuable recreation areas**, these are recreation areas classified as A and B according to M98 Miljødirektoratet methodology:  encroachment_recreation_areas_AB.qmd
- **Total encroachment of “other especially valuable nature” (non-recreation)**, ???:  encroachment_other_valuable_nature.qmd
- **Area of "other especially valuable nature"" types within especially valuable recreation areas**, ???: encroachment_other_valuable_nature_AB.qmd
