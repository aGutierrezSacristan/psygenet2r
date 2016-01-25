# psygenet2r

`psygenet2r` is an R package for query PsyGeNET database (www.psygenet.org) and perform co-morbidity studies within R framework.

## What is this repository for?

This report is used for package distribution while we walk thought BioConductor validation process.

## Package' Status

 * Version: 0.99.0
 * Authors: Alba Gutierrez-Sacristan (GRIB-UPF), Carles Hernandez-Ferrer (CREAL)
 * Maintainer: Alba Gutierrez-Sacristan (GRIB-UPF)

## How to start

### Installation

While BioCondutor finish validating the package, `psygenet2r` can be installed using `devtools` from this repository:

```R
library(devtools)
install_bitbucket("psygenet2r", "albags")
```

### Querying PsyGeNET:

The following lines show two examples of how PsyGeNET can be queried using `psygenet2r`:

 * Gene Query

```R
library(psygeent2r)
qg <- psygenetGene(gene = 4852, 
                   database = "ALL", 
                   check = FALSE
)
```

 * Disease Query

```R
library(psygeent2r)
qd <- psygenetDisease(disease = "Single Major Depressive Episode", 
                      database = "ALL",
                      score    = c('>', 0.5 ) 
)
```