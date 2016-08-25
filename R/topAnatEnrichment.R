#' Enrichment of a user's input (genes) in anatomical terms (TopAnat).
#' 
#' Test the enrichment of a given gene list on Psychiatric Disorders from 
#' PsyGeNET.
#'
#' @name topAnatEnrichment
#' @rdname topAnatEnrichment-methods
#' @aliases topAnatEnrichment
#' @param gene Name or vector of names (that can be both code or uml) to 
#' specific genes from PsyGeNET.
#' @param database Name of the database that will be queried. It can take the 
#' values \code{'psycur15'} to use data validated by experts for first release 
#' of PsyGeNET; \code{'psycur16'} to use data validated by experts for second 
#' release of PsyGeNET; or \code{'ALL'} to use both databases. 
#' Default \code{'ALL'}.
#' @param dataType It can take the values \code{'rna_seq'} or \code{'affymetrix'}.
#' Default \code{'rna_seq'}.
#' @param statistics Default \code{'fisher'}.
#' @param cutOff Default \code{'0.01'}.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to not see
#' the warnings.
#' @return A \code{data.frame} with the enrichment results
#' @examples
#' topAnatEnrichment(c("ADCY2", "AKAP13", "ANK3"), "ALL")
#' @export topAnatEnrichment

topAnatEnrichment <- function( gene, dataType = "rna_seq", statistic = "fisher", cutOff = 0.01, verbose = FALSE, warnings = FALSE ){

    data <- psygenetAll ( database = "ALL", verbose = verbose )
    
    ##############
    
    eGenes <- vector()
    wGenes <- vector()
    
    tabGenes <- ListPsyGeNETIds( database = "ALL", "Gene" )
    
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
    
    
    
    geneList <- gene[gene %in% data$c1.Gene_Symbol]
    if ( length(geneList) != length(gene) ) {
        warning( "One or more of the given genes is not in PsyGeNET ( '", 
                 database, "' ). Genes: ",  
                 paste( gene[!gene %in% data$c1.Gene_Symbol], collapse = ", "))
    }
    
    #data type: rna_seq or affymetrix
    myTopAnatData <- BgeeDB::loadTopAnatData(species = "9606", 
                                     datatype = dataType)
    
        
    #ensembl for mapping genes
    ensembl <- biomaRt::useMart("ensembl")
    ensembl <- biomaRt::useDataset("hsapiens_gene_ensembl", mart=ensembl)
    
    genesId2ensembl <- biomaRt::getBM(attributes=c('ensembl_gene_id',
                                          'hgnc_symbol',
                                          'chromosome_name'),
                                      values ="1",
                                      mart = ensembl)
    
    genesId2ensembl <- genesId2ensembl[genesId2ensembl$hgnc_symbol != "",]
    genesId2ensembl <- genesId2ensembl[genesId2ensembl$hgnc_symbol %in% genesOfInterest,]
    
    myGenes <- as.data.frame(genesId2ensembl[,1])
    colnames(myGenes) <- "ensembl_gene_id"
    
    # Background are all genes with a GO annotation
    universe <- biomaRt::getBM(attributes= "ensembl_gene_id", 
                      filters=c("with_go_go"), 
                      values=list(c(TRUE)), 
                      mart=ensembl)
    
    # Prepares the gene list vector 
    geneList <- factor(as.integer(universe[,1] %in% myGenes[,1]))
    names(geneList) <- universe[,1]
    
    #TopAnat result
    myTopAnatObject <-  BgeeDB::topAnat(myTopAnatData, geneList)
        
    # Prepares the topGO object
    results <- topGO::runTest(myTopAnatObject, 
                       algorithm = 'classic', 
                       statistic = statistic)
    
    # Display results sigificant at a cutOff FDR threshold
    tableOver <- BgeeDB::makeTable(myTopAnatData, 
                           myTopAnatObject, 
                           results, 
                           cutOff)
    
    return( tableOver)
}

