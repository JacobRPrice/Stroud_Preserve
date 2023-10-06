
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Introduction

This repository contains all of the scripts needed to replicate the
results and output presented in:

**Precipitation and soil moisture dominate N-cycling within fields
transitioning from conventional to organic farming**  
- by -  
[Jacob R. Price](https://jacobrprice.github.io/), [Diana
Oviedo-Vargas](https://stroudcenter.org/people/oviedo-vargas/), [Marc
Peipoch](https://stroudcenter.org/people/peipoch/), [Melinda D.
Daniels](https://stroudcenter.org/people/daniels/), & [Jinjun
Kan](https://stroudcenter.org/people/kan/)

This README will describe the contents of this repo and how to replicate
our results.

**Note(s):**

- These scripts were composed and executed on Mac machines; some
  modification may be required for use with PC or Linux environments.

## Process Flow Chart

*If you can’t see the chart make your browser wider and reload the
page.*

<!-- Syntax: -->
<!-- https://mermaid.js.org/syntax/flowchart.html -->
<!-- live editor: -->
<!-- https://mermaid-js.github.io/mermaid-live-editor/ -->

``` mermaid
flowchart LR
    A[(Raw Min/Nit)] --> D{{Prepare_Data.R}}
    B[(Raw EEA)] --> D
    C[(Raw qPCR)] --> D
    D --> E[(dat.RDS)]
    D --> Y[Table S1: Sample Counts]
    
    E --> F{{Seasonality.R}} 
    F --> G["Fig. 3: Seasonality"]
    F --> H["Fig. S2: Sup. Seasonality"]
    
    E --> I{{boxplots.R}} --> J["Fig. 4: Boxplots"]

    E --> K{{anova.R}}
    K --> L["Table 3: ANOVA - MS & T "]
    K --> M["Table 4: ANOVA - CC"]
    K --> N["Table 5: Est Marg Means"]

    E --> O{{weather_SMS.R}}
    P[(Weather Station)] --> O 
    Q[(Soil Moist. Sens.)] --> O
    O --> R[(SMS_dat.RDS)]
    O --> S[(raindat.csv)]
    O --> T[(tempdat.csv)]

    E --> U{{correlation.R}} 
    R --> U
    U --> V["Table S2: Cor Res"]

    E --> W{{eea.R}} --> X["Fig. S3: EEA Ratios"]
```

## Repository Contents

- ./ - the project home directory; where you’re currently located :-)  
- ./README.Rmd & ./README.md - what you’re reading at the moment  
- ./data/ - contains all of the raw data needed to replicate our
  analysis and processed data in case you just want to look at the
  results themselves  
- ./src/ - contains all of the (R) source files
