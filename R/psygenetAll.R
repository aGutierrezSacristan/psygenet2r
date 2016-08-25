psygenetAll <- function( database, verbose = FALSE ) {
  if( verbose ) {
    message( "Staring querying PsyGeNET for ", gene, " in ", database , " database." )
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
  c3 = 'DB'
  ORDER BY
  c0.Score DESC"
  
  oql <- stringr::str_replace(
    string      = oql,
    pattern     = "DB",
    replacement = database
  )
  
  dataTsv <- rawToChar(RCurl::getURLContent( 
    getUrlPsi(), 
    readfunction  = charToRaw( oql ), 
    upload        = TRUE, 
    customrequest = "POST" 
  ))
  data <- read.csv( textConnection( dataTsv ), header = TRUE, sep="\t" )
  return( data )
}
