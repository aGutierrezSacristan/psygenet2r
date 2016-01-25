check_database <- function( database, 
    allowed_db = c( "MODELS", "GAD", "CTD", "PsyCUR", "CURATED", "ALL" ) ) {  
        database <- toupper( database )
        if( sum( database %in% allowed_db ) == 1 ) {
            return( database )
        } else {
            stop("Invalid given 'database' ('", database, "'). ", 
                 'Try ?psygenetGene or \"?psygenetDisease\" to check the ',
                 'available databases.')
        }
}
