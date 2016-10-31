diseaseNameMapping <- function( input , type = "short" ){
    
    table <- input
    
    if( class( table ) == "data.frame"){
        
        if( type != "short" & type != "acronym" ) {
            stop("Invalid argument 'type'. Valid arguments: 'short', 'acronym'.")
        }    
        if( type == "short"){
            table$c2.PsychiatricDisorder <- gsub( "Alcohol use disorders", "Alcohol UD", table$c2.PsychiatricDisorder)
            table$c2.PsychiatricDisorder <- gsub( "Bipolar disorders and related disorders", "Bipolar disorder", table$c2.PsychiatricDisorder)
            table$c2.PsychiatricDisorder <- gsub( "Depressive disorders", "Depression", table$c2.PsychiatricDisorder)
            table$c2.PsychiatricDisorder <- gsub( "Schizophrenia spectrum and other psychotic disorders", "Schizophrenia", table$c2.PsychiatricDisorder)
            table$c2.PsychiatricDisorder <- gsub( "Cocaine use disorders", "Cocaine UD", table$c2.PsychiatricDisorder)
            table$c2.PsychiatricDisorder <- gsub( "Cannabis use disorders", "Cannabis UD", table$c2.PsychiatricDisorder)
            table$c2.PsychiatricDisorder <- gsub( "Substance induced psychosis", "SI-Psychosis", table$c2.PsychiatricDisorder)
            table$c2.PsychiatricDisorder <- gsub( "Substance induced depressive disorder", "SI-Depression", table$c2.PsychiatricDisorder)

            }else 
                if ( type == "acronym" ){
                table$c2.PsychiatricDisorder <- gsub( "Alcohol use disorders", "AUD", table$c2.PsychiatricDisorder)
                table$c2.PsychiatricDisorder <- gsub( "Bipolar disorders and related disorders", "BD", table$c2.PsychiatricDisorder)
                table$c2.PsychiatricDisorder <- gsub( "Depressive disorders", "DEP", table$c2.PsychiatricDisorder)
                table$c2.PsychiatricDisorder <- gsub( "Schizophrenia spectrum and other psychotic disorders", "SCHZ", table$c2.PsychiatricDisorder)
                table$c2.PsychiatricDisorder <- gsub( "Cocaine use disorders", "CUD", table$c2.PsychiatricDisorder)
                table$c2.PsychiatricDisorder <- gsub( "Cannabis use disorders", "CanUD", table$c2.PsychiatricDisorder)
                table$c2.PsychiatricDisorder <- gsub( "Substance induced psychosis", "SI-PSY", table$c2.PsychiatricDisorder)
                table$c2.PsychiatricDisorder <- gsub( "Substance induced depressive disorder", "SI-DEP", table$c2.PsychiatricDisorder)
                }
    }else 
        
        if( class( table ) == "character"){
            names( table ) <- gsub( "Alcohol use disorders", "Alcohol UD", names( table ) )
            names( table ) <- gsub( "Bipolar disorders and related disorders", "Bipolar disorder", names( table ) )
            names( table ) <- gsub( "Depressive disorders", "Depression", names( table ) )
            names( table ) <- gsub( "Schizophrenia spectrum and other psychotic disorders", "Schizophrenia", names( table ) )
            names( table ) <- gsub( "Cocaine use disorders", "Cocaine UD", names( table ) )
            names( table ) <- gsub( "Cannabis use disorders", "Cannabis UD", names( table ) )
            names( table ) <- gsub( "Substance induced psychosis", "SI-Psychosis", names( table ) )
            names( table ) <- gsub( "Substance induced depressive disorder", "SI-Depression", names( table ) )
    }
    
    
    return( table )
    
}
	