
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Stroud\_Preserve

<!-- badges: start -->

<!-- badges: end -->

The goal of Stroud\_Preserve is to …

## Process Flow Diagram

<!-- Source for idea: -->

<!-- https://twitter.com/diegohaz/status/1590043504428077056 -->

``` mermaid
stateDiagram-v2
[*] --> Raw_Data
Raw_Data --> Processed_Data
Processed_Data --> Temporal_Variation
Temporal_Variation --> Figures
Processed_Data --> ANOVA
ANOVA --> Figures
ANOVA --> Tables
```

<!-- Syntax: -->

<!-- https://mermaid.js.org/syntax/flowchart.html -->

``` mermaid
flowchart TD
A[(Raw Min/Nit)] --> D{{Prepare_Data.R}}
B[(Raw EEA)] --> D
C[(Raw qPCR)] --> D
D --> E[(dat.RDS)]
E --> F{{Seasonality.R}} 
F --> (Figure 3: Seasonality)
```

## Description of scripts within this repository:

  - ./r/Prepare\_Data.R - imports the various raw data files in ./data/
    and prepares them for use with the remaining (analytical) scripts in
    this repo  
  - ./r/Sampling\_Table.R - creates a .csv file with the sample counts
    by sampling date and analysis type  
  - ./r/seasonality.R - generates figures illustrating how parameter
    values … erm… vary over the course of the year
  - ./r/boxplots.R - generates boxplots
  - ./r/eea.R - creates eea ratio plots
  - ./r/anova.R - carries out hypothesis tests
  - ./r/weather\_station.R - produces visualizations and summary
    statistics from weather station data

## Reproducing the Results

In order to reproduce our findings or view our approach…

### ANOVA

In order to generate our ANOVA results, execute the following line in
the project’s root directory.

``` bash
bash ./src/anova_script.sh
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub.
