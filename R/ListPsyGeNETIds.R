ListPsyGeNETIds <- function( database, query_type ) {
  oql <- "DEFINE
    c0='/data/main',
    c1='/data/genes',
    c2='/data/diseases',
    c3='/data/datasources'
    ON
    'http://www.psygenet.org/web/PsyGeNET'
    SELECT
    c1 (Gene_Symbol, Gene_Id),
    c2 ( Disease_code, DiseaseName, PsychiatricDisorder),
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
    
    data <- read.csv( textConnection( dataTsv ), header = TRUE, sep = "\t" )
    
    query_type <- tolower(query_type)
    if ( query_type == "gene" ){
        genes <- data[ !duplicated( data[ , 1] ), ]
        genes <- genes[ , c( 1, 2 ) ]
        return ( genes )
    } else if ( query_type == "disease" ){
        diseases <- data [ !duplicated( data[ , 4] ), ]
        diseases <- diseases[ , c( 6, 3, 4 ) ]
        return( diseases )
    } else {
      stop("Invalid value for 'query_type'. Only allowed 'Gene' or 'Disease'.")
    }
}
