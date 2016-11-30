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
#' @param datatype It can take the values \code{'rna_seq'}, \code{'affymetrix'}, 
#' \code{"est"} or \code{"in situ"}. 
#' Default \code{c("rna_seq","affymetrix","est","in_situ")}.
#' @param statistic Default \code{'fisher'}.
#' @param cutOff Default \code{1}.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param warnings By default \code{TRUE}. Change it to \code{FALSE} to not see
#' the warnings.
#' @return A \code{data.frame} with the enrichment results
#' @examples
#' \dontrun{
#' topAnatEnrichment(gene=c("ADCY2", "AKAP13", "ANK3"))
#' }
#' @export topAnatEnrichment

topAnatEnrichment <- function( gene, datatype = c("rna_seq","affymetrix","est","in_situ"), statistic = "fisher", cutOff = 1, verbose = FALSE, warnings = FALSE ){

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
    
    bgee <- BgeeDB::Bgee$new(species = "Homo_sapiens", dataType = datatype)
    myTopAnatData <- BgeeDB::loadTopAnatData( bgee )

    # #ensembl for mapping genes
    # hostMart = "uswest.ensembl.org";
    # 
    # ensembl <- biomaRt::useMart( biomart = "ENSEMBL_MART_ENSEMBL", 
    #                              dataset="hsapiens_gene_ensembl", 
    #                              host = hostMart)
    # 
    # ensembl <- biomaRt::useMart("ensembl")
    # ensembl <- biomaRt::useDataset("hsapiens_gene_ensembl", mart=ensembl)
    # 
    # genesId2ensembl <- biomaRt::getBM(attributes=c('ensembl_gene_id',
    #                                       'hgnc_symbol',
    #                                       'chromosome_name'),
    #                                   values ="1",
    #                                   mart = ensembl)
    # 
    # genesId2ensembl <- genesId2ensembl[genesId2ensembl$hgnc_symbol != "",]
    # 
    # psygenet_universe <- genesId2ensembl[genesId2ensembl$hgnc_symbol %in% tabGenes$c1.Gene_Symbol, ]
 
    
    conv <- read.delim(system.file(paste0("extdata", .Platform$file.sep,
              "ensemble2gene_symbol.tsv"), package = "psygenet2r"), stringsAsFactors = FALSE)
    
    myGenes <- conv[conv$hgnc_symbol %in% geneList, 1, drop = FALSE]
    colnames(myGenes) <- "ensembl_gene_id"
    
    # # Background are all genes with a GO annotation
    # universe <- biomaRt::getBM(attributes= "ensembl_gene_id", 
    #                   filters=c("with_go_go"), 
    #                   values=list(c(TRUE)), 
    #                   mart=ensembl)
    # 
    universe <- conv[ , 1, drop = FALSE]
    
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
    
    return( tableOver )
}


