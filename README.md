
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

More details about the **lnmixsurv** package see:
`https://github.com/vivianalobo/lnmixsurv.git`

### Prerequisites

The codes use the Bayesian mixture survival approach implemented in the
**lnmixsurv** package.

``` r
require("lnmixsurv")
```

### Running

1.  The Telco customer churn data are now saved under /data.
2.  A detailed descriptive analysis are shown under /Rmd.
3.  Run `DescriptiveAnalysis.Rmd` or open `DescriptiveAnalysis.html` to
    see the discussion about the data.
4.  The inference procedure are now saved under /R. See `script.R`
5.  Run `script.R` to make inference.
