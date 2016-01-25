#' Getter from \code{DataGeNET.Psy}.
#' 
#' Obtain the number of unique diseases in a \code{DataGeNET.Psy}.
#'
#' @name ndisease
#' @rdname ndisease-methods
#' @aliases ndisease
#' @param object Object of class \code{DataGeNET.Psy}
#' @return The number of unique diseases
#' @examples
#' if (require(psygenet2r)){
#'  ndisease(qr)
#' }
#' @export
setMethod( "ndisease",
    signature = "DataGeNET.Psy",
    definition = function( object ) {
        return( length( unique( object@qresult$c2.Disease_code ) ) )
    }
)
