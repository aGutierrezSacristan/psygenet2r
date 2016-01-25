#' Query PsyGeNET for given gene(s) and generates an \code{DataGeNET.Psy}
#'
#' Given the name of one or multiple gene and retrives their information 
#' from PsyGeNET and creates an object of type \code{DataGeNET.Psy}.
#'
#' @name getUMLs
#' @rdname getUMLs-methods
#' @aliases getUMLs
#' @param word Disese to convert to UMLS using PsyGeNET database.
#' @param database Name of the database that will be queried. It can take the 
#' values \code{'MODELS'} to use Comparative Toxigenomics Database, data from 
#' mouse and rat; \code{'GAD'} to use Genetic Association Database; \code{'CTD'}
#' to use Comparative Toxigenomics Database, data from human; \code{'PsyCUR'} to
#' use Psychiatric disorders Gene association manually curated; \code{'CURATED'}
#' to use Human, manually curated databases (PsyCUR and CTD); or \code{'ALL'} 
#' to use all these databases. Default \code{'CURATED'}.
#' @return The corresponding UMLs for the input disease/s
#' @examples
#' umls <- getUMLs( "Alcoholism", "CURATED" )
#' @export getUMLs
getUMLs <- function(word, database = "CURATED") {
  oql <- "DEFINE
    c0='/data/main',
    c1='/data/genes',
    c2='/data/diseases',
    c3='/data/datasources'
    ON
    'http://www.psygenet.org/web/PsyGeNET'
    SELECT
    c1 (Gene_Symbol, Gene_Id),
    c2 (Disease_code, DiseaseName, PsychiatricDisorder),
    c0 (Disease_Id)
    FROM
    c0
    WHERE
    c3 = 'DB'
    ORDER BY
    c0.Score DESC" 
  
  oql <- stringr::str_replace(
    string      = oql,
    pattern     = "DB",
    replacement = database 
  )
  
  dataTsv <- RCurl::getURLContent( 
    getUrlPsi(), 
    readfunction  = charToRaw(oql), 
    upload        = TRUE, 
    customrequest = "POST"
  )
  
  data <- read.csv( textConnection( dataTsv ), header = TRUE, sep = "\t", stringsAsFactors = FALSE )[ , c(4, 5, 6) ]
  colnames(data) <- c("DiseaseName", "PsychiatricDisorder", "umls")
  data <- data[!duplicated(data$umls), ]

  words <- data[,1]
  ClosestMatch <- function(string,StringVector) {
    matches <- agrep(string, StringVector, value = TRUE)
    data <- data[data[,1] %in% matches,]
    return(data)
  }  
  
  results <- ClosestMatch(word,words)
  return(results)
  
}







