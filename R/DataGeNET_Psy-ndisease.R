#' @describeIn DataGeNET.Psy get number of diseases
#' @return The number of unique diseases
#' @examples
#' ndisease(qr)
setMethod( "ndisease",
    signature = "DataGeNET.Psy",
    definition = function( object ) {
        return( length( unique( object@qresult$c2.Disease_code ) ) )
    }
)
