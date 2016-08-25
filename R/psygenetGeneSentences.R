#' Query PsyGeNET for given gene(s) and extract the pmids sentences that report 
#' a gene-disease asssociation.
#' 
#' Given a gene or a gene list, retrives the pmids and sentences for each 
#' gene-disease association from PsyGeNET and creates an object of type 
#' \code{DataGeNET.Psy}.
#' 
#' @param geneList Name or vector of names (that can be both code or symbol) to 
#' specific genes from PsyGeNET. The genes non existing in PsyGeNET will
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
#' genesOfInterest <- c("PECR", "ADH1C", "CAST", "ERAP1", "PPP2R2B",
#'                      "ESR1", "GATA4", "CDH13")
#' psyGeneSen <- psygenetGeneSentences( geneList = genesOfInterest,
#'                                      database = "ALL")
#' @export psygenetGeneSentences
psygenetGeneSentences <- function( geneList, database = "ALL", verbose = FALSE ) {
  
  if( verbose ) {
    message( "Staring querying PsyGeNET for you gene list in ", database , " database." )
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
  c2 (Disease_code, DiseaseName ),
  c0 (OriginalDB, Pubmed_Id, curatorAnnot, Sentence)
  FROM
  c0
  WHERE
  (
  c1 = 'GENE_ID'
  AND
  c3 = 'DTB'
  )"
  
  wGenes <- vector()
  eGenes <- vector()
  data <- data.frame()
  present <- 0
  
  for( i in 1:length( unique( geneList ) ) ){
    if( class( geneList[ i ] ) == "character" ){
      table <- psyGenDisId( database, "Gene" )
      gene_r <- geneList[ i ]
      if( geneList[ i ] %in% table[,1] ){
        geneList[ i ] <- as.numeric( as.character( table[ table[ , 1 ] == geneList[i], 2 ] ) )
        eGenes  <- c( eGenes, gene_r )
        present <- 1
      } else {
        wGenes  <- c( wGenes, geneList[ i ] )
        present <- 0
      }
    }
    
    if( present != 0 ){
      oql2 <- stringr::str_replace(
        string      = oql,
        pattern     = "GENE_ID",
        replacement = geneList[ i ] 
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
      ) )
      dataNew <- read.csv( textConnection(dataTsv), header = TRUE, sep="\t" ) 
      data <- rbind( data, dataNew )
    }
  }
  
  if (length(wGenes) != 0) {
    genes <- paste( wGenes, collapse = ", " )
    warning( "One or more of the given genes is not in PsyGeNET ( '", database, "' ). Genes: ", genes )
  }
  
  result <- new( "DataGeNET.Psy", 
                 type     = "gene", 
                 search   = "list", 
                 term     = as.character( eGenes ), 
                 database = database, 
                 qresult  = data
  )
  return( result )
}