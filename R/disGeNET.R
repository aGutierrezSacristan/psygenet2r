disgenetAll <- function( database, verbose = FALSE ) {
  if( verbose ) {
    message( "Staring querying DisGeNET in '", database , "' database." )
  }
  
  oql <- "DEFINE
  c0='/data/gene_to_associated_diseases',
  c1='/data/genes',
  c2='/data/genes_to_gene_number',
  c3='/data/sources'
  ON
  'http://bitbucket.org/janis_pi/disgenet_onexus.git'
  
  SELECT
  c1 (name, geneId, description ),
  c0 (numberOfassocDiseases, sourceId),
  c2 (geneId, NoAssocGenes, sourceId, geneId, sourceId)
  FROM
  c0
  WHERE
  c3 = 'DB'
  ORDER BY
  c0.numberOfassocDiseases DESC" 

  oql <- stringr::str_replace(
      string      = oql,
      pattern     = "DB",
      replacement = database 
  )

  dataTsv <- RCurl::getURLContent(
    getUrlDis(), 
    readfunction  = charToRaw(oql), 
    upload        = TRUE, 
    customrequest = "POST"
    ) 
  data <- read.csv(textConnection(dataTsv), header = TRUE, sep="\t")

  return( data )
}

###############################################################################

disGenetCurated <- function ( database ) {
  oql <- "DEFINE
    c0='/data/gene_to_associated_diseases',
  c1='/data/genes',
  c2='/data/genes_to_gene_number',
  c3='/data/sources'
  ON
  'http://bitbucket.org/janis_pi/disgenet_onexus.git'
  SELECT
  c1 (geneId, uniprotId),
  c0 (numberOfassocDiseases),
  c2 (sourceId)
  FROM
  c0
  WHERE
  c3 = 'DB'
  ORDER BY
  c0.numberOfassocDiseases DESC" 
      
  oql <- stringr::str_replace(
      string      = oql,
      pattern     = "DB",
      replacement = database 
  )
  
  dataTsv <- RCurl::getURLContent ( 
    getUrlDis(), 
    readfunction  = charToRaw( oql ), 
    upload        = TRUE, 
    customrequest = "POST" 
  )
  data <- read.csv ( textConnection( dataTsv ), header = TRUE, sep="\t" )  
  proteinUniverse <- unique ( data[,1] )
  return ( proteinUniverse )
  
    
}

