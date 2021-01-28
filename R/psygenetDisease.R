#' Query PsyGeNET for given disease(s) and generates an \code{DataGeNET.Psy}
#'
#' Given the name of one or multiple diseases and retrives their information 
#' from PsyGeNET and creates an object of type \code{DataGeNET.Psy}.
#'
#' @name psygenetDisease
#' @rdname psygenetDisease-methods
#' @aliases psygenetDisease
#' @param disease Name or vector of names (that can be both code or uml) to 
#' specific diseases from PsyGeNET. The diseases non existing in PsyGeNET will
#' be removed from the output.
#' @param database Name of the database that will be queried. It can take the 
#' values \code{'psycur15'} to use data validated by experts for first release 
#' of PsyGeNET; \code{'psycur16'} to use data validated by experts for second 
#' release of PsyGeNET; or \code{'ALL'} to use both databases. 
#' Default \code{'ALL'}.
#' @param evidenceIndex A vector with two elements: 1) character with greather 
#' \code{'>'} or with lower \code{'<'} meaing greather or equal and lower or
#' equal; 2) the evidence index cut-off to be compared. By default: \code{c('>', 0)}.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to don't see
#' the warnings.
#' @return An object of class \code{DataGeNET.Psy}
#' @examples
#' d.sch <- psygenetDisease( "schizophrenia", "ALL" )
#' @note The "Evidence Index" is gotten from PsyGeNET. For more information
#' about it and its calculation, pease visit \code{psygenet.org}.
#' @export psygenetDisease
psygenetDisease <- function( disease, database = "ALL", evidenceIndex=c('>', 0), verbose = FALSE, warnings = TRUE ) {
  check_database( database )
  if( length( disease ) != length( unique( disease ) ) ) {
    disease <- unique( disease )
    if( warnings ) {
      warning(
        "Removing duplicates from input diseases list."
      )
    }
  }
  if(length(evidenceIndex) != 2) {
    stop("Invalid argumetn 'evidenceIndex'. It must have two elements.")
  } else if(!evidenceIndex[1] %in% c('>', '<')) {
    stop("Invalid argument 'evidenceIndex'. First elemnt must be '>' or '<'.")
  }
  
  if( verbose ) {
    message( 
      "Staring querying PsyGeNET for ", paste(disease, collapse = ", "), " in ", 
      database, " database." 
    )
  }
  
  oql <- "DEFINE
    c0='/data/main',
    c1='/data/genes',
    c2='/data/diseases',
    c3='/data/datasources'
    ON
    'http://www.psygenet.org/web/PsyGeNET'
    SELECT
    c1 (Gene_Symbol, Gene_Id, Gene_Description),
    c2 (Disease_code, Disease_Id, DiseaseName, PsychiatricDisorder),
    c0 (Score, Disease_Id, Number_of_Abstracts,Number_of_AbstractsValidated)
    FROM
    c0
    WHERE
    (
    c3 = 'DB'
    AND
    c2 = 'DISEASE_ID'
    )
    ORDER BY
    c0.Score DESC"
    
  
  eDiseases <- vector()
  wDiseases <- vector()
  result    <- data.frame()
  
  ## We get the all DISEASE ids from PsyGeNET to check if the given disease
  ## are present there.
  tabDisease <- ListPsyGeNETIds( database, "disease" )
  tabDisease[ , 2 ] <- as.character ( tabDisease[ , 2 ] )
  ## /
  
  for( ii in 1:length( disease ) ){
    if( verbose ) {
      message( 
        "Validating disease ", disease[ii], " into PsyGeNET"
      )
    }
    present   <- 0
    ## Check if disease is CODE
    if( substr ( disease[ii], 1, 1 ) == "C" & ( nchar( disease[ii] ) == 8 )){
      cc <- substr( disease[ii], 2, 8)
      disease_t <- as.numeric( cc )
      
      if( !is.na( disease_t ) ) {
        disease[ii] <- paste( "umls:", disease[ii], sep = "" )
      }
      rm( disease_t )
    }
    ## /
    ## Check if disease is CODE/UML and it exists in PsyGeNET
    if( substr ( disease[ ii ] , 1, 4 ) == "umls" ){
      disease_r <- disease[ ii ]
      if( disease[ ii ] %in% tabDisease[ , 1] ){
        eDiseases <- c( eDiseases, disease_r )
        present   <- 1
        if( verbose ) {
          message( "\t PRESENT" )
        }
      }else{
        wDiseases <- c( wDiseases, disease[ ii ] )
        present   <- 0
        if( verbose ) {
          message( "\tNOT PRESENT" )
        }
      }
    } else {
      if( disease[ ii ] %in% tabDisease[ , 3] ){
        disease_r <- disease[ ii ]
        disease[ ii ]  <- as.character( 
          tabDisease[ tabDisease[ , 3 ] == disease[ ii ], 1 ]
        )
        eDiseases <- c( eDiseases, disease_r )
        present   <- 1
        
      }else{
        wDiseases <- c(wDiseases, disease[ ii ])
        present   <- 0
      }
    }
    ## /
 
    ## If the current disease exists in PsyGeNET the main query is performed
    if( present == 1){
      oql_current <- stringr::str_replace(
        string      = oql,
        pattern     = "DB",
        replacement = database
      )
      if( verbose ) {
        message( 
          "Retreiving data for ", disease[ii], " from PsyGeNET."
        )
      }
      
      if( substr ( disease[ii], 1, 4 ) == "umls" ){
        oql_current <- stringr::str_replace(
          string      = oql_current,
          pattern     = "DISEASE_ID",
          replacement = disease[ii]
        )
      } else if( class( disease ) == "character" ){
        oql_current <- stringr::str_replace(
          string      = oql_current,
          pattern     = "c2 = 'DISEASE_ID'",
          replacement = paste0( "c2.DiseaseName IN ('", disease[ii], "')" )
        )
      } else {
        stop("Something was wrong on 'oql_current' during the main query ",
          "to PsyGeNET for disease <", disease[ii], ">."
        )
      }
      dataTsv <- download_data( oql_current )
      dataNew <- read.csv( textConnection( dataTsv ), header = TRUE, sep = "\t" )
      result <- rbind( result, dataNew )
    }
  }
  
  if( length( wDiseases ) != 0 ) {
    diseases <- paste( wDiseases, collapse = ", " )
    if( warnings ) {
      warning( "One or more of the given diseases is not in PsyGeNET ( '", database, "' ):\n", diseases )
    }
  }
  
  if(evidenceIndex[1] == '>') {
    result <- result[result$c0.Score>=as.numeric(evidenceIndex[2]),]
  } else {
    result <- result[result$c0.Score<=as.numeric(evidenceIndex[2]),]
  }
  
  result <- new( "DataGeNET.Psy",
                 type     = "disease",
                 term     = as.character( eDiseases ),
                 database = database,
                 qresult  = result
  )
  return( result )
}
