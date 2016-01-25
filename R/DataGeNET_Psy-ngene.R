#' Getter from \code{DataGeNET.Psy}.
#' 
#' Obtain the number of unique genes in a \code{DataGeNET.Psy}.
#'
#' @name ngene
#' @rdname ngene-methods
#' @aliases ngene
#' @param object Object of class \code{DataGeNET.Psy}
#' @return The number of unique genes
#' @examples
#' if (require(psygenet2r)){
#'  ngene(qr)
#' }
#' @export
setMethod( "ngene",
    signature = "DataGeNET.Psy",
    definition = function( object ) {
        return( length( unique( object@qresult$c1.Gene_Id ) ) )
    }
)
