# psygenet2r

`psygenet2r` is an R package for query PsyGeNET database (www.psygenet.org) and perform co-morbidity studies within R framework.

## What is this repository for?

This report is used for package distribution while we walk thought BioConductor validation process.

## Package' Status

 * __Version__: 0.99.0
 * __Authors__: Alba Gutierrez-Sacristan (GRIB-UPF), Carles Hernandez-Ferrer (CREAL)
 * __Maintainer__: Alba Gutierrez-Sacristan (GRIB-UPF)

## How to start

### Installation

While BioCondutor finish validating the package, `psygenet2r` can be installed using `devtools` from this repository:

```R
library(devtools)
install_bitbucket("albags/psygenet2r")
```

### Querying PsyGeNET:

The following lines show two examples of how PsyGeNET can be queried using `psygenet2r`:

 * __Gene Query__

```R
library(psygeent2r)
qg <- psygenetGene(gene = 4852, 
    database = "ALL", 
    check = FALSE
)
```

 * __Disease Query__

```R
library(psygeent2r)
qd <- psygenetDisease(disease = "Single Major Depressive Episode", 
    database = "ALL",
    score = c('>', 0.5) 
)
```