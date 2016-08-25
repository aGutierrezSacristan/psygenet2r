#' Query PsyGeNET for given disease(s) and extract the pmids sentences that report 
#' a gene-disease asssociation.
#' 
#' Given a disease or a disease list, retrives the pmids and sentences for each 
#' gene-disease association from PsyGeNET and creates an object of type 
#' \code{DataGeNET.Psy}.
#' 
#' @param diseaseList Name or vector of names (that can be both code or uml) to 
#' specific diseases from PsyGeNET. The diseases non existing in PsyGeNET will
#' be removed from the output.
#' @param database Name of the database that will be queried. It can take the 
#' values \code{'psycur15'} to use data validated by experts for first release 
#' of PsyGeNET; \code{'psycur16'} to use data validated by experts for second 
#' release of PsyGeNET; or \code{'ALL'} to use both databases. 
#' Default \code{'ALL'}.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @return An object of class \code{DataGeNET.Psy}
#' @examples
#' diseasesOfInterest <- c( "Bipolar Disorder","Depressive Disorder, Major" )
#' psyDisSen <- psygenetDiseaseSentences( diseaseList = diseasesOfInterest,
#'                                        database = "ALL" )
#' @export psygenetDiseaseSentences
psygenetDiseaseSentences <- function( diseaseList, database = "ALL", verbose = FALSE ) {
  
  if( verbose ) {
    message( "Staring querying PsyGeNET for you disease list in ", database , " database." )
  }
  
  oql <- "DEFINE
  c0='/data/sentences',
  c1='/data/genes',
  c2='/data/diseases',
  c3='/data/datasources'
  ON
  'http://www.psygenet.org/web/PsyGeNET'
  SELECT
  c1 (Gene_Symbol, Gene_Id),
  c2 (Disease_code, DiseaseName),
  c0 (OriginalDB, Pubmed_Id, Sentence)
  FROM
  c0
  WHERE
  (
  c2 = 'DISEASE'
  AND
  c3 = 'DTB'
  )" 

  wDiseases <- vector()
  eDiseases <- vector()
  data <- data.frame()
  present <- 0
  
  table <- psyGenDisId( database, "Diseases" )
  table[ , 1] <- as.character ( table[ , 1] )
  table[ , 2] <- as.character ( table[ , 2] )
  table[ , 3] <- as.character ( table[ , 3] )
  
  for(disease in diseaseList) {
    if( disease %in% table[ , 1] ) { #umls
        eDiseases <- c( eDiseases, disease )
        present <- 1
    } else if( disease %in% table[ , 2] ) { #code
        disease  <- table[table[ , 2] == disease, 1]
        eDiseases <- c( eDiseases, disease )
        present <- 1
    } else if( disease %in% table[ , 3] ) { #name
        disease  <- table[table[ , 3] == disease, 1] 
        eDiseases <- c( eDiseases, disease )
        present <- 1
    } else {
        wDiseases <- c( wDiseases, disease )
        present   <- 0
    }
    
    if( present != 0 ){
      oql2 <- stringr::str_replace(
        string      = oql,
        pattern     = "DISEASE",
        replacement = disease
      )
      oql2 <- stringr::str_replace(
        string      = oql2,
        pattern     = "DTB",
        replacement = database 
      )
      dataTsv <- rawToChar(RCurl::getURLContent(
        getUrlPsi(), 
        readfunction  = charToRaw(oql2), 
        upload        = TRUE, 
        customrequest = "POST", 
        .encoding     = "UTF-8"
      ))
      dataNew <- read.csv( textConnection( dataTsv ), header = TRUE, sep="\t" ) 
      data <- rbind( data, dataNew )
    }
  }
  
  if (length(wDiseases) != 0) {
    diseases <- paste( wDiseases, collapse = ", " )
    warning( "One or more of the given diseases is not in PsyGeNET ( '", database, "' ). Diseases: ", diseases )
  }
  
  result <- new( "DataGeNET.Psy", 
                 type     = "disease", 
                 search   = "list", 
                 term     = as.character( eDiseases ), 
                 database = database, 
                 qresult  = data
  )
  return( result )
}