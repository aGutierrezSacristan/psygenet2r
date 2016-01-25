check_genes <- function( genes, hostMart = "www.ensembl.org", verbose = FALSE, 
                         warnings = TRUE ) {
    ## This function can onlybe used if the R package biomaRt is present
    if( !requireNamespace( "biomaRt", quietly = TRUE ) ) {
        return( TRUE )
    }
    ## /

    ## All the given genest must be of ID or SYMBOL
    if( verbose ) {
        message("The given genes will be testes on biomaRt tochek they are",
        "correct. Remember that all the geiven genes must be the same type",
        "(ot Gene ID or Gene Symbol).")
    }
    ## /

    ## Try to know if the given gene is an ID or a SYMBOL
    gene_t <- tryCatch({
        as.numeric( genes[ 1 ] )
        "entrez"
    },
        warning = function(w) {
            "hgnc"
        }
    )
    ## /

  ensembl <- biomaRt::useMart( biomart = "ENSEMBL_MART_ENSEMBL", 
                               dataset="hsapiens_gene_ensembl", 
                               host = hostMart)
  
    if( gene_t == "entrez" ) {
        x <- biomaRt::getBM(attributes = "entrezgene",
            filters    = c( "entrezgene" ),
            values     = genes, 
            mart       = ensembl
        )
    } else {
        x <- biomaRt::getBM(attributes = "hgnc_symbol",
            filters    = c( "hgnc_symbol" ),
            values     = genes, 
            mart       = ensembl
        )
    }
    
    x <- x[ , 1 ]
    ngenes <- genes[ !genes %in% x ]
  
    if( length( ngenes ) != 0 ) {
        gg <- paste( paste( "   -", ngenes ), collapse = "\n" )
        if( warnings ) {
            warning("Not all given genes could be validated:\n", gg,
            "\nUsing the first given gene, they where queried into ",
            "'hsapiens_gene_ensembl' as '", gene_t, "'."
            )
        }
    }

    return( TRUE )
}
