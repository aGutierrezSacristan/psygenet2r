#' @describeIn extract Extract function for DataGeNET.Psy
#' @return A \code{data.frame} containing the raw result from PsyGeNET
#' @examples
#' data(qr)
#' extract(qr)[1:2, ] # get internat data.frame
#' @export
setMethod( "extract",
    signature = "DataGeNET.Psy",
    definition = function( object ) {
        return( object@qresult )
    }
)
