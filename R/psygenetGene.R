#' Query PsyGeNET for given gene(s) and generates an \code{DataGeNET.Psy}
#'
#' Given the name of one or multiple gene and retrives their information 
#' from PsyGeNET and creates an object of type \code{DataGeNET.Psy}.
#'
#' @name psygenetGene
#' @rdname psygenetGene-methods
#' @aliases psygenetGene
#' @param gene Name or vector of names (that can be both code or symbol) to 
#' specific genes from PsyGeNET. The genes non existing in PsyGeNET will
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
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to not see
#' the warnings.
#' @return An object of class \code{DataGeNET.Psy}
#' @examples
#' d.alch <- psygenetGene( "ALDH2", "ALL" )
#' @note The "Evidence Index" is gotten from PsyGeNET. For more information
#' about it and its calculation, pease visit \code{psygenet.org}.
#' @export psygenetGene
psygenetGene <- function( gene, database = "ALL", evidenceIndex=c('>', 0),
                          verbose = FALSE, warnings = TRUE ) {
  check_database( database )
  if( length( gene ) != length( unique( gene ) ) ) {
    gene <- unique( gene )
    warning(
      "Removing duplicates from input genes list."
    )
  }
  
  if(length(evidenceIndex) != 2) {
    stop("Invalid argument 'evidenceIndex'. It must have two elements.")
  } else if(!evidenceIndex[1] %in% c('>', '<')) {
    stop("Invalid argument 'evidenceIndex'. First elemnt must be '>' or '<'.")
  }
  
  if( verbose ) {
    message( 
      "Staring querying PsyGeNET for ", paste(gene, collapse = ", "), " in ", 
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
    c1 (Gene_Symbol, Gene_Id),
    c2 (Disease_Id, Disease_code, DiseaseName, PsychiatricDisorder),
    c0 (Score, Number_of_Abstracts, Number_of_AbstractsValidated)
    FROM
    c0
    WHERE
    (
    c1 = 'GENE_ID'
    AND
    c3 = 'DB'
    )
    ORDER BY
    c0.Score DESC"
  
  eGenes <- vector()
  wGenes <- vector()
  result <- data.frame()
  
  tabGenes <- ListPsyGeNETIds( database, "Gene" )
  
  for( ii in 1:length( unique( gene ) ) ){
    ## Try to know if the given gene is an ID or a SYMBOL
    gene_r <- tryCatch({
        as.numeric( gene[ ii ] )
      },
      warning = function(w) {
        gene[ ii ]
      }
    )
    ## /
    
    ## Check if gene is in PsyGeNET
    if( class( gene_r ) == "character" ){
      if( gene_r %in% tabGenes[ , 1 ]){
        eGenes <- c( eGenes, gene_r )
        present <- 1
      } else {
        wGenes <- c( wGenes, gene_r )
        present <- 0
      }
    } else if( class( gene_r ) == "numeric" ){
      if( gene_r %in% tabGenes[ , 2 ] ){
        eGenes <- c( eGenes, gene_r )
        present <- 1
      }else {
        wGenes <- c(wGenes, gene_r )
        present <- 0
      }
    } else {
      stop("Something was wrong with the gene's id: <", gene_r, ">.")
    }
    ## /
    
    ## If the current gene exists in PsyGeNET the main query is performed
    if( present == 1 ){
      oql2 <- stringr::str_replace(
        string      = oql,
        pattern     = "DB",
        replacement = database
      )
      
      if( class( gene_r ) == "numeric" ){
        oql2 <- stringr::str_replace(
          string      = oql2,
          pattern     = "GENE_ID",
          replacement = as.character( gene_r )
        )
      } else if( class( gene_r ) == "character" ){
        oql2 <- stringr::str_replace(
          string      = oql2,
          pattern     = "c1 = 'GENE_ID'",
          replacement = paste0("c1.Gene_Symbol IN ('", gene_r, "')")
        )
      } else {
        stop("Something was wrong on 'oql_current' during the main query ",
             "to PsyGeNET for gene <", gene_r, ">."
        )
      }
      
      dataTsv <- download_data( oql2 )
      dataNew <- read.csv( textConnection( dataTsv ), header = TRUE, sep = "\t" )
      result <- rbind( result, dataNew )
      
    }
  }
  
  if( length( wGenes ) != 0 ) {
    genes <- paste( wGenes, collapse = ", " )
    if( warnings ) {
      warning( "One or more of the given genes is not in PsyGeNET ( '", database, "' ):\n", genes )
    }
  }
  
  if(evidenceIndex[1] == '>') {
    result <- result[result$c0.Score>=as.numeric(evidenceIndex[2]),]
  } else {
    result <- result[result$c0.Score<=as.numeric(evidenceIndex[2]),]
  }
  
  result <- new( "DataGeNET.Psy",
                 type     = "gene",
                 term     = as.character( eGenes ),
                 database = database,
                 qresult  = result
  )
  return( result )
}
