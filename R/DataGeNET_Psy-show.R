setMethod( "show",
  signature = "DataGeNET.Psy",
  definition = function( object ) {
    cat( "Object of class 'DataGeNET.Psy'\n" )
    cat( " . Type:        ", object@type, "\n" )
    cat( " . Database:    ", object@database, "\n" )
    
    n <- length( object@term )
    if( n > 1 ) {
      cat( " . Term:        ", object@term[ 1 ], "...", object@term[ n ], "\n" )
    } else {
      cat( " . Term:        ", object@term, "\n" )
    }
    
    cat( " . Number of Results:  ", nrow( object@qresult ), "\n" )
    if( object@type == "gene" ) {
      cat( " . Number of unique Diseases: ", length( unique( object@qresult$c2.Disease_code ) ), "\n" )
      cat( " . Number of unique Genes:    ", length( unique( object@qresult$c1.Gene_Id ) ), "\n" )
    } else if( object@type == "disease" ) {
      cat( " . Number of unique Genes:    ", length( unique( object@qresult$c1.Gene_Id ) ), "\n" )
      cat( " . Number of unique Diseases: ", length( unique( object@qresult$c2.Disease_Id ) ), "\n" )
    }
  }
)
