# icardaFIGSr: A Toolkit for Focused Identification of Germplasm Strategy (FIGS)

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/icardaFIGSr)](https://CRAN.R-project.org/package=icardaFIGSr)  
[![R-CMD-check](https://github.com/Analychaf/icardaFIGSr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Analychaf/icardaFIGSr/actions/workflows/R-CMD-check.yaml)

## Overview

The `icardaFIGSr` package provides tools for applying the Focused Identification of Germplasm Strategy (FIGS) to genebank data. With FIGS, users can subset collections efficiently to identify promising accessions based on traits, environmental data, and statistical workflows. This package is designed to support researchers and genebank managers in the identification of targeted germplasm subsets for plant breeding, research & developpement, and conservation purposes.

## Features

- **Data Management**:
  - Access and preprocess genebank and environmental datasets.
  - Handle climatic data and crop-specific parameters effectively.

- **Modeling and Analysis**:
  - Train machine learning models with flexible workflows for classification and regression.
  - Generate variable importance metrics and predictions.
  - Evaluate model performance using tools like ROC curves and confusion matrices.

- **Visualization**:
  - Create intuitive plots for climatic and phenotypic data.
  - Explore spatial and statistical patterns in genebank collections.

- **Built-in Datasets**:
  - Access preloaded datasets such as `DurumWheatDHEWC`, `BarleyRNOWC`, and `FIGS` subsets.

## Installation

Install the latest release from CRAN:

```R
install.packages("icardaFIGSr")
```

Or, install the development version from GitHub:

```R
devtools::install_github("Analychaf/icardaFIGSr")
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

#### 2. Generate Variable Importance

```R
# Load data and train a classification model
model <- tuneTrain(data = DurumWheatDHEWC, y = 'DHE', method = 'rf')

# Evaluate variable importance
var_imp <- varimpPred(newdata = model$`Test Data`, y = 'DHE', model = model$Model)
var_imp$VariableImportancePlot
```

#### 3. Extract Onset Data

```R
# Extract climatic data based on onset of planting
onset_data <- getOnset(sites = unique(DurumWheatDHEWC$SiteCode), crop = 'ICDW', var = c("tavg", "prec"))
head(onset_data[[1]])
```

#### 4. Visualize Spatial Data

```R
# Map accessions by site
mapAccessions(data = DurumWheatDHEWC, longitude = "Longitude", latitude = "Latitude", site_id = "SiteCode")
```

## Vignettes

Comprehensive tutorials and examples are available as vignettes:

1. [Crop Data Analysis](articles/CropData.html)
2. [Machine Learning Workflows](articles/ML_Workflows.html)
3. [Climate Data Integration](articles/Sites_climate.html)

To view vignettes locally:

```R
browseVignettes("icardaFIGSr")
```

## Contributing

We welcome contributions to `icardaFIGSr`! Please see our [Contributing Guidelines](https://github.com/Analychaf/icardaFIGSr/blob/main/CONTRIBUTING.md) for details on how to get involved.

## License

This package is licensed under the MIT License. See the LICENSE file for details.

## Acknowledgments

This project was developed with contributions from:
<br>
- **Zakaria Kehel** (*Maintainer and Author*)<br>
- **Khadija Aziz** (*Author*)<br>
- **Khadija Aouzal** (*Author*)<br>
- **Chafik Analy** (*Author*)<br>
- **Bancy Ngatia** (*Author*)<br>
- **Zainab Azough**, **Amal Ibnelhobyb**, **Fawzy Nawar** (*Contributors*)<br>

## Contact

For questions, please contact :
<br>
Zakaria Kehel: [z.kehel@cgiar.org](mailto:z.kehel@cgiar.org) or
Khadija Aouzal: [k.aouzal@cgiar.org](mailto:k.aouzal@cgiar.org)

---

Explore the ICARDA FIGS R Package and leverage advanced tools to optimize your genebank management and research.

