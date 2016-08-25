#' Enrichment of a user's input (genes) in PsyGeNET's diseases.
#' 
#' Test the enrichment of a given gene list on Psychiatric Disorders from 
#' PsyGeNET.
#'
#' @name enrichedPD
#' @rdname enrichedPD-methods
#' @aliases enrichedPD
#' @param gene Name or vector of names (that can be both code or uml) to 
#' specific genes from PsyGeNET.
#' @param database Name of the database that will be queried. It can take the 
#' values \code{'psycur15'} to use data validated by experts for first release 
#' of PsyGeNET; \code{'psycur16'} to use data validated by experts for second 
#' release of PsyGeNET; or \code{'ALL'} to use both databases. 
#' Default \code{'ALL'}.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to not see
#' the warnings.
#' @return A \code{data.frame} with the enricment at each Psychiatric Disorder
#' @examples
#' enrichedPD(c("ADCY2", "AKAP13", "ANK3"), "ALL")
#' @export enrichedPD
enrichedPD <- function( gene, database="ALL", verbose = FALSE, warnings = FALSE ) {
    data <- psygenetAll ( database, verbose = verbose )
    
    ##############
    
    eGenes <- vector()
    wGenes <- vector()
    
    tabGenes <- ListPsyGeNETIds( database, "Gene" )
    
    for( ii in 1:length( unique( gene ) ) ){
        ## Try to know if the given gene is an ID or a SYMBOL
        gene_r <- tryCatch({
            as.numeric( gene[ ii ] )
        },
        warning = function(w) {
            gene[ ii ]
        }
        )
        ## /
        
        ## Check if gene is in PsyGeNET
        if( class( gene_r ) == "character" ){
            if( gene_r %in% tabGenes[ , 1 ]){
                eGenes <- c( eGenes, gene_r )
                present <- 1
            } else {
                wGenes <- c( wGenes, gene_r )
                present <- 0
            }
        } else if( class( gene_r ) == "numeric" ){
            if( gene_r %in% tabGenes[ , 2 ] ){
                gene_r <- as.character(tabGenes[tabGenes[, 2 ] == gene_r, 1])
                eGenes <- c( eGenes, gene_r )
                present <- 1
            }else {
                wGenes <- c(wGenes, gene_r )
                present <- 0
            }
        } else {
            stop("Something was wrong with the gene's id: <", gene_r, ">.")
        }
    }
    if( length( wGenes ) != 0 ) {
        genes <- paste( paste( "   -", wGenes ), collapse = "\n" )
        if( warnings ) {
            warning( "One or more of the given genes is not in PsyGeNET ( '", database, "' ):\n", genes )
        }
    }
    
    gene <- eGenes
        
    ##############
    
    sel <- gene[gene %in% data$c1.Gene_Symbol]
    if ( length(sel) != length(gene) ) {
        warning( "One or more of the given genes is not in PsyGeNET ( '", 
                 database, "' ). Genes: ",  
                 paste( gene[!gene %in% data$c1.Gene_Symbol], collapse = ", "))
    }
  
    x <- length(sel)
    TT <- length(unique(data$c1.Gene_Symbol))
  
    xs <- sapply(unique(data$c2.PsychiatricDisorder), function(dd) {
        data.f = data[data$c2.PsychiatricDisorder == dd, ]
        n <- sum(sel %in% data.f$c1.Gene_Symbol)
        t <- length(unique(data.f$c1.Gene_Symbol))  
        phyper(n - 1, t, TT - t, x, lower.tail = FALSE)
    })
  
    data.frame(MPD=unique(data$c2.PsychiatricDisorder), p.value=xs)
}
