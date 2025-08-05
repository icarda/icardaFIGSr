# icardaFIGSr: A Toolkit for Focused Identification of Germplasm Strategy (FIGS)

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/icardaFIGSr)](https://CRAN.R-project.org/package=icardaFIGSr)
[![R-CMD-check](https://badges.cranchecks.info/summary/icardaFIGSr.svg)](https://cran.r-project.org/web/checks/check_results_icardaFIGSr.html)

## Overview

The `icardaFIGSr` package provides tools for applying the Focused Identification of Germplasm Strategy (FIGS) to plant genetic resources data. With FIGS, users can subset collections efficiently to identify promising accessions based on traits, environmental data, and statistical workflows. This package is designed to support researchers and genebank managers in the identification of targeted germplasm subsets for plant breeding, research & developpement, and conservation purposes.

## Features

- **Data Retrieval**:
  - Access and preprocess genebank and environmental datasets.
  - Handle climatic data and crop-specific parameters effectively.

- **Modeling and Analysis**:
  - Train machine learning models with flexible workflows for classification and regression.
  - Generate variable importance metrics and predictions.
  - Evaluate model performance using tools like ROC curves and confusion matrices.

- **Built-in Datasets**:
  - Access preloaded datasets such as `DurumWheatDHEWC`, `BarleyRNOWC`, and `FIGS` subsets, among others.

## Installation

Install the latest release from CRAN:

```R
install.packages("icardaFIGSr")
```

Or, install the development version from GitHub:

```R
devtools::install_github("icarda/icardaFIGSr")
```

## Getting Started

### Load the Package

```R
library(icardaFIGSr)
```

### Example Workflow

#### 1. Load a Built-in Dataset

```R
data("DurumWheatDHEWC")
head(DurumWheatDHEWC)
```

#### 2. Model Training and Variable Importance

```R
# Train a regression model on the loaded dataset
model <- tuneTrain(data = DurumWheatDHEWC, y = 'DHE', method = 'rf', summary = defaultSummary, classProbs = FALSE)

# Evaluate variable importance
var_imp <- varimpPred(newdata = model$`Test Data`, y = 'DHE', model = model$Training)
var_imp$VariableImportancePlot
```

#### 3. Extract Onset Data

```R
# Extract onset and climatic data for durum wheat
durum <- getAccessions(crop = 'Durum wheat', coor = FALSE)
onset_data <- getOnset(sites = unique(durum$SiteCode), crop = 'ICDW',
                var = c('tavg', 'prec'), cv = TRUE)
# Climate data
head(onset_data[[1]])

# Onset and phenological data
head(onset_data[[2]])
```

#### 4. Visualize Spatial Data

```R
# Map accessions by population type
mapAccessions(df = durum, long = "Longitude", lat = "Latitude", y = "PopulationType")
```

## Vignettes

More details and examples are available as vignettes:

1. [Accessing Crop-Related Data](articles/CropData.html): `vignette("CropData")`
2. [Predictive Modeling using tuneTrain()](articles/ML_Workflows.html): `vignette(ML_Workflows)`
3. [Extracting Sites Climate Data](articles/Sites_climate.html): `vignette(Sites_climate)`

To view vignettes locally:

```R
browseVignettes("icardaFIGSr")
```

## Acknowledgments

This package was developed with contributions from:

> * **Zakaria Kehel** (*Maintainer and Author*)
> * **Chafik Analy** (*Author*)
> * **Khadija Aouzal** (*Author*)
> * **Khadija Aziz** (*Author*)
> * **Bancy Ngatia** (*Author*)
> * **Zainab Azough**, **Amal Ibnelhobyb**, **Fawzy Nawar** (*Contributors*)

## Contact

For questions, please contact : Khadija Aouzal [k.aouzal@cgiar.org](mailto:k.aouzal@cgiar.org) or Zakaria Kehel [z.kehel@cgiar.org](mailto:z.kehel@cgiar.org)

