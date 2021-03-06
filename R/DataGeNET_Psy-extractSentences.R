#' @describeIn extractSentences Get sentences or evidences
setMethod( "extractSentences",
    signature = "DataGeNET.Psy",
    definition = function (object, disorder, verbose = FALSE ) {
    if(missing(disorder)) {
        stop("Missing mandatiry argument 'disorder'")
    }
        object <- object@qresult

    ##############
    
    if( substr ( disorder, 1, 1 ) == "C" & ( nchar( disorder ) == 8 )){
        cc <- substr( disorder, 2, 8)
        disease_t <- as.numeric( cc )
        
        if( !is.na( disease_t ) ) {
            disorder <- paste( "umls:", disorder, sep = "" )
            object <- object[ object$c2.Disease_Id == disorder, ]
            
        }
    } else if( substr ( disorder , 1, 4 ) == "umls" ){
        object <- object[ object$c2.Disease_Id == disorder, ]
    }
    else{
        object <- object[ object$c2.DiseaseName == disorder, ]
    }
    
    #############
    
    colnames( object ) <- c("GENE_SYMBOL", "GENE_ID", "DISEASE_UMLS","DISEASE_NAME", 
                           "ORIGINAL_DB", "PUBMED_ID", "ANNOTATION","SENTENCE")
    return( object )
    
    }
)
