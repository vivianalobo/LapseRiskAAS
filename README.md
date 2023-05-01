
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LapseRiskASS

##### Lapse risk modelling in insurance: a Bayesian mixture approach

##### Authors: Viviana G R Lobo, Thais C O Fonseca and Mariane B Alves

Bayesian Survival models via mixture of Log-Normal distribution extends
the well-known survival models accommodating different behaviour over
time and considers higher censored survival times. The proposal combines
mixture distributions Fruhwirth-Schnatter(2006)
<https://doi.org/10.1007/s11336-009-9121-4>, and data augmentation
techniques Tanner and Wong (1987)
<https://doi/abs/10.1080/01621459.1987.10478458>.

More details about the lnmixsurv package see:
<https://github.com/vivianalobo/lnmixsurv.git>

## Requirements

The codes use the Bayesian mixture survival approach implemented in the
lnmixsurv package.

``` r
require("lnmixsurv")
```

## Example

We present the Telco customer analysis. This is a basic example which
shows you how to assess the dataset and run descriptive analysis and
inference procedure

``` r
#### Telco customer churn dataset 
### loading data and others
source("R/up(telco).R")
```

For acess a descriptive analysis
