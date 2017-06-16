#' Getter from \code{DataGeNET.Psy}.
#' 
#' Obtain the number of unique diseases in a \code{DataGeNET.Psy}.
#'
#' @name ndisease
# @rdname ndisease-methods
#' @aliases ndisease
#' @param object Object of class \code{DataGeNET.Psy}.
#' @examples
#' data(qr)
#' ndisease(qr)
#' @export
setGeneric ("ndisease",
    function(object){standardGeneric("ndisease")}
)

#' Getter from \code{DataGeNET.Psy}.
#' 
#' Obtain the number of unique genes in a \code{DataGeNET.Psy}.
#'
#' @name ngene
# @rdname ngene-methods
#' @aliases ngene
#' @param object Object of class \code{DataGeNET.Psy}.
#' @return The number of unique genes
#' @examples
#' data(qr)
#' ngene(qr)
#' @export
setGeneric ("ngene",
    function(object){standardGeneric("ngene")}
)

#' Method to obtain the sentences that support a gene-disease association from a \code{DataGeNET.Psy} object.
#' 
#' PsyGeNET contains a list of sentences that support a gene-disease association from public
#' literature. The internal table of a \code{DataGeNET.Psy} object 
#' contains this information. The method \code{extractSentences} 
#' allows to extract those sentences that support a gene-disease association given a \code{DataGeNET.Psy} object 
#' and a disorder of interest.
#' 
#' @name extractSentences
# @rdname extractSentences-methods
#' @aliases extractSentences
#' @param object Object of class \code{DataGeNET.Psy}.
#' @param disorder The disorder of interest. Only those sentences supporting a gene-disease association 
#' with this specific disorder will be extracted.
#' @param verbose If set to \code{TRUE} informative messages are show.
#' @return A data frame showing the sentences.
#' @examples
#' data(qr)
#' extractSentences(qr, "Depression")
#' @export extractSentences
setGeneric ("extractSentences",
            function(object, disorder, verbose){standardGeneric("extractSentences")}
)

#' Raw data from \code{DataGeNET.Psy} and \code{JaccardIndexPsy}.
#' 
#' Obtain the raw data from a PsyGeNET's query  stored in a 
#' \code{DataGeNET.Psy} object or the raw data with all the Jaccard Index for 
#' the disease of interest of an \code{JaccardIndexPsy} object.
#'
#' @name extract
# @rdname extract-methods
#' @aliases extract
#' @param object Object of class \code{DataGeNET.Psy} or \code{JaccardIndexPsy}
#' @param ... NO USED
#' @return A \code{data.frame} containing the raw result from PsyGeNET or
#' a \code{data.frame} with the result Jaccard Index for each disease.
setGeneric ("extract",
    function(object, ...){standardGeneric("extract")}
)

