#' @describeIn DataGeNET.Psy get number of genes
#' @return The number of unique genes
setMethod( "ngene",
    signature = "DataGeNET.Psy",
    definition = function( object ) {
        return( length( unique( object@qresult$c1.Gene_Id ) ) )
    }
)
