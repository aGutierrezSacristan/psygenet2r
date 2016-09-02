disgenetAll <- function( database, verbose = FALSE ) {
    if( verbose ) {
        message( "Staring querying DisGeNET in '", database , "' database." )
    }
    
    oql <- "DEFINE
    c0='/data/gene_to_associated_diseases',
    c1='/data/genes',
    c2='/data/gene_roles',
    c3='/data/gene_to_gene_number',
    c4='/data/sources'
    ON
    'http://www.disgenet.org/web/DisGeNET'
    
    SELECT
    c1 (name, geneId, description ),
    c0 (numberOfassocDiseases, sourceId),
    c3 (geneId, NoAssocGenes, sourceId, geneId, sourceId)
    FROM
    c0
    WHERE
    c4 = 'DB'
    ORDER BY
    c0.numberOfassocDiseases DESC" 
    
    
    oql <- stringr::str_replace(
        string      = oql,
        pattern     = "DB",
        replacement = database 
    )
    
    dataTsv <- download_data(oql, url = getUrlDis())
    data <- read.csv(textConnection(dataTsv), header = TRUE, sep="\t")
    
    return( data )
}

###############################################################################

disGenetCurated <- function ( database ) {
    oql <- "DEFINE
    c0='/data/gene_to_associated_diseases',
    c1='/data/genes',
    c2='/data/gene_roles',
    c3='/data/gene_to_gene_number',
    c4='/data/sources'
    ON
    'http://www.disgenet.org/web/DisGeNET'
    SELECT
    c1 (geneId, uniprotId),
    c0 (numberOfassocDiseases),
    c3 (sourceId)
    FROM
    c0
    WHERE
    c4 = 'DB'
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

