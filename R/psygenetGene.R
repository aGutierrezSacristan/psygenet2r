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
#' values \code{'MODELS'} to use Comparative Toxigenomics Database, data from 
#' mouse and rat; \code{'GAD'} to use Genetic Association Database; \code{'CTD'}
#' to use Comparative Toxigenomics Database, data from human; \code{'PsyCUR'} to
#' use Psychiatric disorders Gene association manually curated; \code{'CURATED'}
#' to use Human, manually curated databases (PsyCUR and CTD); or \code{'ALL'} 
#' to use all these databases. Default \code{'CURATED'}.
#' @param score A vector with two elements: 1) character with greather 
#' \code{'>'} or with lower \code{'<'} meaing greather or equal and lower or
#' @param check By default \code{TRUE}. Change it to \code{FALSE} to not
#' validate the genes.
#' @param hostMart The URL of Biomart to be used.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to not see
#' the warnings.
#' @return An object of class \code{DataGeNET.Psy}
#' @examples
#' d.alch <- psygenetGene( "ALDH2", "CURATED" )
#' @export psygenetGene
psygenetGene <- function( gene, database = "CURATED", score=c('>', 0), check = TRUE, hostMart = "www.ensembl.org", verbose = FALSE, warnings = TRUE ) {
  check_database( database )
  if( length( gene ) != length( unique( gene ) ) ) {
    gene <- unique( gene )
    warning(
      "Removing duplicates from input genes list."
    )
  }
  
  if(length(score) != 2) {
    stop("Invalid argumetn 'score'. It must have two elements.")
  } else if(!score[1] %in% c('>', '<')) {
    stop("Invalid argument 'score'. First elemnt must be '>' or '<'.")
  }
  
  if( check ) {
    check_genes( gene, hostMart = hostMart, verbose = verbose, warnings = warnings )
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
    c1 (Gene_Symbol, Gene_Id, Gene_Description),
    c2 (Disease_Id, Disease_code, DiseaseName, PsychiatricDisorder),
    c0 (Score, Number_of_Abstracts)
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
          replacement = gene_r
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
      
      dataTsv <- RCurl::getURLContent(
        getUrlPsi(), 
        readfunction  = charToRaw( oql2 ), 
        upload        = TRUE, 
        customrequest = "POST"
      )
      dataNew <- read.csv( textConnection( dataTsv ), header = TRUE, sep = "\t" )
      result <- rbind( result, dataNew )
      
    }
  }
  
  if( length( wGenes ) != 0 ) {
    genes <- paste( paste( "   -", wGenes ), collapse = "\n" )
    if( warnings ) {
      warning( "One or more of the given genes is not in PsyGeNET ( '", database, "' ):\n", genes )
    }
  }
  
  if(score[1] == '>') {
    result <- result[result$c0.Score>=as.numeric(score[2]),]
  } else {
    result <- result[result$c0.Score<=as.numeric(score[2]),]
  }
  
  result <- new( "DataGeNET.Psy",
                 type     = "gene",
                 term     = as.character( eGenes ),
                 database = database,
                 qresult  = result
  )
  return( result )
}
