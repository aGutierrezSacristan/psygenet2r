setMethod( "show",
  signature = "JaccardIndexPsy",
  definition = function( object ) {
    cat( "Object of class 'JaccardIndexPsy'\n" )
    cat( " . #Boot:              ", object@nit, "\n" )
    cat( " . Type:               ", object@type, "\n" )
    cat( " . #Results:           ", nrow(object@table), "\n" )
  }
)
