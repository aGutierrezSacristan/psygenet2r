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
#' values \code{'MODELS'} to use Comparative Toxigenomics Database, data from 
#' mouse and rat; \code{'GAD'} to use Genetic Association Database; \code{'CTD'}
#' to use Comparative Toxigenomics Database, data from human; \code{'PsyCUR'} to
#' use Psychiatric disorders Gene association manually curated; \code{'CURATED'}
#' to use Human, manually curated databases (PsyCUR and CTD); or \code{'ALL'} 
#' to use all these databases. Default \code{'CURATED'}.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @return A \code{data.frame} with the enricment at each Psychiatric Disorder
#' @examples
#' enrichedPD(c("ADCY2", "AKAP13", "ANK3"), "CURATED")
#' @export enrichedPD
enrichedPD <- function( gene, database="CURATED", verbose = FALSE ) {
    data <- psygenetAll ( database, verbose = verbose )
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
