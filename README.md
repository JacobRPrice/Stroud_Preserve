
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Stroud\_Preserve

<!-- badges: start -->

<!-- badges: end -->

The goal of Stroud\_Preserve is to …

## Description of scripts within this repo:

  - ./r/Prepare\_Data.R - this script imports the various raw data files
    in ./data/ and prepares them for use with the remaining (analytical)
    scripts in this repo.  
  - ./r/Sampling\_Table.R - Creates a .csv file with the sample counts
    by sampling date and analysis type.

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
