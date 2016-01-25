#' Method to obtain the evidences from a \code{DataGeNET.Psy} object.
#' 
#' Internally, PsyGeNET uses a series of collected evidences from public
#' literature. The internal table of a \code{DataGeNET.Psy} object 
#' can contains this information. The method \code{extractSentences} 
#' allows to extrat this information.
#' 
#' @param input A \code{DataGeNET.Psy} object.
#' @param disorder A disorder to check if any evidence exists.
#' @param vervose If set to \code{TRUE} informative messages are show.
#' @return A data frame showing the evidence.
#' @examples 
#' extractSentences(qr, "Depression")
#' @export extractSentences
setMethod( "extractSentences",
    signature = "DataGeNET.Psy",
    definition = function (input, disorder, verbose = FALSE ) {
    if(missing(disorder)) {
        stop("Missing mandatiry argument 'disorder'")
    }
    input <- input@qresult

    ##############
    
    if( substr ( disorder, 1, 1 ) == "C" & ( nchar( disorder ) == 8 )){
        cc <- substr( disorder, 2, 8)
        disease_t <- as.numeric( cc )
        
        if( !is.na( disease_t ) ) {
            disorder <- paste( "umls:", disorder, sep = "" )
            input <- input[ input$c2.Disease_Id == disorder, ]
            
        }
    } else if( substr ( disorder , 1, 4 ) == "umls" ){
        input <- input[ input$c2.Disease_Id == disorder, ]
    }
    else{
        input <- input[ input$c2.DiseaseName == disorder, ]
    }
    
    #############
    
    input <- input[,c(1,4,6,7,8,9)]
    colnames( input ) <- c("GENE", "DISEASE_UMLS","DISEASE_NAME", 
                           "ORIGINAL_DB", "PUBMED_ID", "SENTENCE")
    return( input )
    
    }
)
