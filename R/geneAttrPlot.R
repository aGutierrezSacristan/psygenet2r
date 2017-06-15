#' Ploting the relation between genes and disease-categories
#'
#' Given a set of genes or a result of \code{psygenetGene} creates four types
#' of plots showing the relation of the genes with the disease's category
#' in psyGeNET.
#'
#' @name geneAttrPlot
#' @rdname geneAttrPlot-methods
#' @aliases geneAttrPlot
#' @param x Vector of genes of interest of \code{DataGeNET.Psy} resulting of
#' \code{psyegnetDisease}.
#' @param type Type of the drawn chart. By default it is \code{"pie"}. It can 
#' takes \code{"pie"} to plot a pie chart with the number of genes for each psychiatric category, 
#' \code{"disease category"} for visualizing a barplot with the total and specific number of genes for
#' each psychiatric disorder, \code{"evidence index"} for a barplot showing for each psychiatric disorder
#' the number of gene-disease associations according to the Evidence index
#' and \code{"gene"} for visualizing a barplot with the total and specific number of diseases associated to each gene.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @return A plot for a \code{DataGeNET.Psy} in terms of the panther-class.
#' @export geneAttrPlot
#' 

geneAttrPlot <- function( x, type = "pie", verbose = FALSE ){
    type <- tolower( type )
    if( type == "pie") {
        .pie_plot(x, verbose = verbose)
    } else if( type == "disease category" ) {
        .category_plot(x, verbose = verbose)
    } else if( type == "evidence index" ) {
        .index_plot(x, verbose = verbose)
    } else if( type == "gene" ) {
        .gene_plot(x, verbose = verbose)
    }else{
	     stop( paste0( "Invalid 'type' value. Accepted: 'pie', ",
        "'disease category', 'evidence index' and 'gene'." ) )	
	}

}


## -------------------------------------------------------------------------- ##
.gene_plot <- function( x, cuiBarColor = "#790080", diseaseCategoryBarColor = "#ffdb01", verbose ) {
    if( length(x@term) == 1 ) {
        stop( "For this type of chart, a multiple query created with 'psygenetGene' is required." )
    }
    
    if( class( x ) == "DataGeNET.Psy" ) {
        #if( x@type == "gene" ) {
        table <- x@qresult
        #} else {
        #    stop( "Invalid 'type' of 'DatageNET' object. Expected result of 'psygenetGene'." )
        #}
    } else if( class( x ) == "character" ) {
        x <- psygenetGene( x , databse = "ALL" )
        table <- x@qresult
    }
    
    ourList <- table[ , c( 1,2,4,7 ) ]

    ourList <- diseaseNameMapping( ourList )

    geneList <- as.character(unique(ourList$c1.Gene_Symbol))
    
    result <- data.frame(matrix(0, ncol = 3, nrow = length(geneList)))
    colnames(result) <- c("gene", "cuis","categories")
    result$gene <- geneList
    
    ourList$c2.Disease_Id <- as.character(ourList$c2.Disease_Id)
    ourList$c2.PsychiatricDisorder <- as.character(ourList$c2.PsychiatricDisorder)
    
    for( i in 1:nrow(result)){
        selection <- ourList[ourList$c1.Gene_Symbol == result$gene[i],]
        result$cuis[i]<- length(unique(selection$c2.Disease_Id))
        result$categories[i] <- length(unique(selection$c2.PsychiatricDisorder))
    }
    
    result.m <- reshape2::melt(result, id.vars='gene')
    
    p <- ggplot2::ggplot(result.m, ggplot2::aes(gene, value)) +   
        ggplot2::geom_bar(ggplot2::aes(fill = variable), 
                          position = "dodge", 
                          stat="identity", 
                          colour = "black")
    
    p <- p + ggplot2::scale_fill_manual(values=c(cuiBarColor, diseaseCategoryBarColor))
    p <- p + ggplot2::theme_classic( ) + ggplot2::theme( plot.margin = unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ), 
                                                         axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), text = ggplot2::element_text ( size = 14 ) ,
                                                         axis.text.x = ggplot2::element_text ( angle = 45, size = 10, hjust = 1 ))
    return( p )
    
}

## -------------------------------------------------------------------------- ##
.index_plot <- function( x, verbose ) {
    if( length(x@term) == 1 ) {
        stop( "For this type of chart, a multiple query created with 'psygenetGene' is required." )
    }
    
    if( class( x ) == "DataGeNET.Psy" ) {
        #if( x@type == "gene" ) {
        table <- x@qresult
        #} else {
        #    stop( "Invalid 'type' of 'DatageNET' object. Expected result of 'psygenetGene'." )
        #}
    } else if( class( x ) == "character" ) {
        x <- psygenetGene( x , databse = "ALL" )
        table <- x@qresult
    }
    
    tt <- table[ , c( 1, 4, 7, 8 ) ] 
    tt$pairs <- paste(tt[,1], tt[,2], sep="-")
    tt <- diseaseNameMapping( tt )
    categories <- as.character(unique(tt$c2.PsychiatricDisorder))
    
    result <- data.frame(matrix(0, ncol = 5, nrow = length(categories)))
    colnames(result) <- c("Category", "Total", "100% association", "100% no association", "Both")
    result$Category <- categories
    
    for(i in 1:nrow(result)){
        selection <- tt[tt$c2.PsychiatricDisorder == result$Category[i], ]
        result[i,2] <- length(unique(selection$pairs))
        
        pos <- selection[selection$c0.Score == 1,]
        neg <- selection[selection$c0.Score == 0,]
        mix <- selection[selection$c0.Score > 0 & selection$c0.Score < 1 ,]
        
        result[i,3] <- length(unique(pos$pairs))
        result[i,4] <- length(unique(neg$pairs))
        result[i,5] <- length(unique(mix$pairs))
        
    }
    
    
    result.m <- reshape2::melt(result, id.vars='Category')
    result.m$value <- as.numeric(result.m$value)
    
    p <- ggplot2::ggplot(result.m, ggplot2::aes(Category, value)) +   
        ggplot2::geom_bar(ggplot2::aes(fill = variable), position = "dodge", stat="identity") +
        ggplot2::theme_classic( ) + 
        ggplot2::theme( plot.margin = unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ), 
             axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), 
             text = ggplot2::element_text ( size = 14 ) ,
             axis.text.x = ggplot2::element_text ( angle = 45, size = 7, hjust = 1 )) +
        ggplot2::scale_fill_discrete(name = "")
    
    return( p )
}

## -------------------------------------------------------------------------- ##
.category_plot <- function( x, uniqueGenesBarColor = "#E69F00", totalGenesBarColor = "#136593", verbose ) {
    
    if( length(x@term) == 1 ) {
        stop( "For this type of chart, a multiple query created with 'psygenetGene' is required." )
    }
    
    if( class( x ) == "DataGeNET.Psy" ) {
        #if( x@type == "gene" ) {
        table <- x@qresult
        #} else {
        #    stop( "Invalid 'type' of 'DatageNET' object. Expected result of 'psygenetGene'." )
        #}
    } else if( class( x ) == "character" ) {
        x <- psygenetGene( x , databse = "ALL" )
        table <- x@qresult
    }
    
    ourList <- table[ , c( 1,2,4,7 ) ]
    ourList <- diseaseNameMapping( ourList )
    categories <- as.character(unique(ourList$c2.PsychiatricDisorder))
    
    result <- data.frame(matrix(0, ncol = 3, nrow = length(categories)))
    colnames(result) <- c("category", "uniqueGenes", "totalGenes")
    result$category <- categories
    
    ourList$c2.PsychiatricDisorder <- as.character(ourList$c2.PsychiatricDisorder)
    
    for( i in 1:nrow(result)){
        selection <- ourList[ourList$c2.PsychiatricDisorder == result$category[i],]
        nselectin <- ourList[ourList$c2.PsychiatricDisorder != result$category[i],]
        
        tgenes <- as.character(unique(selection$c1.Gene_Symbol))
        ngenes <- as.character(unique(nselectin$c1.Gene_Symbol))
        uniqueG <- tgenes[! tgenes %in% ngenes]
        
        result$uniqueGenes[i]<- length(uniqueG)
        result$totalGenes[i] <- length(tgenes)
        
    }
    
    result.m <- reshape2::melt(result, id.vars='category')
    p <- ggplot2::ggplot(result.m, ggplot2::aes(category, value)) +   
        ggplot2::geom_bar(ggplot2::aes(fill = variable), 
                          position = "dodge", 
                          stat="identity", 
                          colour = "black")
    
    p <- p + ggplot2::scale_fill_manual(values=c( uniqueGenesBarColor, totalGenesBarColor ))
    p <- p + ggplot2::theme_classic( ) + ggplot2::theme( plot.margin = unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ), 
                                                         axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), text = ggplot2::element_text ( size = 14 ) ,
                                                         axis.text.x = ggplot2::element_text ( angle = 45, size = 10, hjust = 1 ))
    return( p )  
}


## -------------------------------------------------------------------------- ##
.pie_plot <- function(x, AUDcolor = "#FF3C32", BDcolor = "#FFC698", DEPcolor = "#9BE75E", 
                      SCHZcolor = "#1F6024", CUDcolor = "#5AB69C", 
                      SIDEPcolor = "#50B8D6", CanUDcolor = "#5467C3", SYPSYcolor = "#A654C3", verbose) {
    if( class( x ) == "DataGeNET.Psy" ) {
        #if( x@type == "gene" ) {
        table <- x@qresult
        #} else {
        #    stop( "Invalid 'type' of 'DatageNET' object. Expected result of 'psygenetGene'." )
        #}
    } else if( class( x ) == "character" ) {
        if(verbose) {
            message("Quering PsygeNET for given gene-list")
        }
        x <- psygenetGene(x, databse = "ALL", verbose = verbose)
        table <- x@qresult
    }
    
    ourList <- table[ , c( 1, 7 ) ]
    ourList <- ourList[ !duplicated( ourList ), ]
    ourList <- diseaseNameMapping( ourList )
   
    # Genes per Category
    genesCat <- as.data.frame(table(ourList$c2.PsychiatricDisorder))
    genesCat <- genesCat[!duplicated(genesCat),]
    slices <- genesCat$Freq
    lbls <- paste(as.character(genesCat$Var1), "(",as.character(genesCat$Freq), ")") 
    

    setsOrder <- c ( "Alcohol UD", 
                     "Bipolar disorder", 
                     "Depression", 
                     "Schizophrenia", 
                     "Cocaine UD", 
                     "SI-Depression", 
                     "Cannabis UD",
                     "DI-Psychosis" 
                     )
    

        
    colors <- c(  AUDcolor, BDcolor, DEPcolor, SCHZcolor, CUDcolor, 
                  SIDEPcolor, CanUDcolor, SYPSYcolor)
    
    names( colors ) <- setsOrder
    
    pie(slices,
        labels=lbls,
        main="Genes per disease category", 
        col = colors[ genesCat$Var1 ]
    )
}
