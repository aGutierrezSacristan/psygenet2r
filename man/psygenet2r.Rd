% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psygenet2r.R
\docType{package}
\name{psygenet2r}
\alias{psygenet2r}
\alias{psygenet2r-package}
\title{psygenet2r: Package to query PsyGeNET database and to perform comorbidity studies}
\description{
psygenet2r has two categories of functions: querying functions and 
analysis and plotting functions.
}
\section{querying functions}{

The functions to retrieve data from PsyGeNET are \link{psygenetDisease} and
\link{psygenetGene}. There are some other support functions like
\link{psygenetGeneSentences}.
}

\section{analysis and plotting functions}{

The functions \link{extract} and \link{extractSentences} allows to retrieve
the row data obtained from on-line resources. The functions \code{plot} and
\link{pantherGraphic} draws a variety of charts to illustrate the obtained 
results. The function \link{enrichedPD} was built to perform enrichment 
studies on PsyGeNET data. Finally the function \link{jaccardEstimation} 
computes a Jaccard Index from a given input on PsyGeNET data.
}

