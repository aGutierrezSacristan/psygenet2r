#' @describeIn ndisease Get number of diseases
#' @return The number of unique diseases
setMethod( "ndisease",
    signature = "DataGeNET.Psy",
    definition = function( object ) {
        return( length( unique( object@qresult$c2.Disease_code ) ) )
    }
)
