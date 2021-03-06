% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_AllClasses.R
\docType{class}
\name{DataGeNET.Psy-class}
\alias{DataGeNET.Psy-class}
\title{Class DataGeNET.Psy}
\description{
Class \code{DataGeNET.Psy} is the basic object use in \code{psygenet2r} 
package. It is the main data container to using the different functions to
query PsyGeNET database and generate teir output. The constructors of this 
class are the functions \code{psygenetGene} and \code{psygenetDisease}.
}
\section{Slots}{

\describe{
\item{\code{type}}{Character containing \code{'gene'} of \code{'disease'}. It is 
used to eprform the correct query to PsyGeNET.}

\item{\code{search}}{Character containing \code{'single'} of \code{'list'}.It is 
used to eprform the correct query to PsyGeNET.}

\item{\code{database}}{Character containing the name of the database that will be
queried. It can take the values \code{'MODELS'} to use Comparative 
Toxigenomics Database, data from mouse and rat; \code{'GAD'} to use Genetic 
Association Database; \code{'CTD'} to use Comparative Toxigenomics Database, 
data from human; \code{'PsyCUR'} to use Psychiatric disorders Gene 
association manually curated; \code{'CURATED'} to use Human, manually 
curated databases (PsyCUR and CTD); or \code{'ALL'} to use all these 
databases.}

\item{\code{term}}{Charcter with the term(s) to search into the database(s).}

\item{\code{qresult}}{\code{data.frame} with the obtained result}
}}

\seealso{
psygenetGene, psygenetDisease, DataGeNET.Psy-methods
}
