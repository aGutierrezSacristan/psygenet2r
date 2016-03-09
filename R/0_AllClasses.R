#' Class DataGeNET.Psy
#'
#' Class \code{DataGeNET.Psy} is the basic object use in \code{psygenet2r} 
#' package. It is the main data container to using the different functions to
#' query PsyGeNET database and generate teir output. The constructors of this 
#' class are the functions \code{psygenetGene} and \code{psygenetDisease}.
#'
#' @name DataGeNET.Psy-class
#' @aliases DataGeNET.Psy-class
#' @rdname DataGeNET.Psy-class
#' @exportClass DataGeNET.Psy
#' @slot type Character containing \code{'gene'} of \code{'disease'}. It is 
#' used to eprform the correct query to PsyGeNET.
#' @slot search Character containing \code{'single'} of \code{'list'}.It is 
#' used to eprform the correct query to PsyGeNET.
#' @slot database Character containing the name of the database that will be
#' queried. It can take the values \code{'MODELS'} to use Comparative 
#' Toxigenomics Database, data from mouse and rat; \code{'GAD'} to use Genetic 
#' Association Database; \code{'CTD'} to use Comparative Toxigenomics Database, 
#' data from human; \code{'PsyCUR'} to use Psychiatric disorders Gene 
#' association manually curated; \code{'CURATED'} to use Human, manually 
#' curated databases (PsyCUR and CTD); or \code{'ALL'} to use all these 
#' databases.
#' @slot term Charcter with the term(s) to search into the database(s).
#' @slot qresult \code{data.frame} with the obtained result
#' @seealso psygenetGene, psygenetDisease, DataGeNET.Psy-methods
setClass( "DataGeNET.Psy",
    representation =
        representation( 
            type     = "character",  # gene or disease
            search   = "character",  # single or list
            database = "character",  # where to search
            term     = "character",  # what to search
            qresult  = "data.frame" # result
    ),
    prototype = 
        prototype( 
            type     = "",
            search   = "",
            database = "",
            term     = "",
            qresult  = data.frame()
    )
)

#' Class JaccardIndexPsy
#'
#' Class \code{JaccardIndexPsy} is theresult of the process to look for a 
#' Jaccard Index between muliple diseases in \code{psygenet2r} package.
#'
#' @name JaccardIndexPsy-class
#' @aliases JaccardIndexPsy-class
#' @rdname JaccardIndexPsy-class
#' @exportClass JaccardIndexPsy
#' @slot nit Number of iterations to calculate the estimated Jaccard index
#' @slot type Slot to save type of query (disease-disease, gene-disease)
#' @slot table \code{data.frame} containing the result table of Jaccard indexes
#' @slot i1 [internal use] vector with names of first component
#' @slot i2 [internal use] vector with names of second component
#' @examples 
#' ji <- jaccardEstimation( c( "COMT", "CLOCK", "DRD3" ), "umls:C0005586", "CURATED" )
#' @seealso psygenetGene, psygenetDisease, JaccardIndexPsy-methods
setClass( "JaccardIndexPsy",
    representation = 
        representation( 
            nit   = "numeric",        # number of interations in bootstrap
            type  = "character",
            table = "data.frame",
            i1    = "character",
            i2    = "character"
    ),
    prototype = 
        prototype( 
            nit   = 0,
            type  = "standard",
            table = data.frame(),
            i1    = "",
            i2    = ""
    )
)
