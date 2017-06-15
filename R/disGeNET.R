##############################################################################

disGenetCurated <- function (  ) {

    oql <- "DEFINE
    	c0='/data/gene_to_associated_diseases',
        c1='/data/genes',
        c2='/data/sources'
        ON
        'http://www.disgenet.org/web/DisGeNET'
        SELECT
        c1 (symbol, geneId),
        c0 ( Ndiseases, source)
        FROM
        c0
        WHERE
        c2 = 'CURATED'
        ORDER BY
        c0.Ndiseases DESC"
  
    dataTsv <- RCurl::getURLContent ( 
        getUrlDis(), 
        readfunction  = charToRaw( oql ), 
        upload        = TRUE, 
        customrequest = "POST" 
    )
    data <- read.csv ( textConnection( dataTsv ), header = TRUE, sep = "\t" )  
    proteinUniverse <- unique ( data[ , 1 ] )
    return ( proteinUniverse )
}

