#' Plots the content of a \code{DataGeNET.Psy} object
#' 
#' This functions llows to create a variety of plots for \code{DataGeNEt.Psy}
#' and \code{JaccardIndexPsy} objects.
#'
#' @param x Object of class \code{DataGeNET.Psy}
#' @param y NOT USED
#' @param layout Function to design the location of the different nodes. By 
#' default \code{layout.fruchterman.reingold} from \code{igraph} is used.
#' @param type Type of the drawn chart. By default it is \code{"GDA network"} 
#' but it also can be \code{"GDCA network"}, \code{"GDCA heatmap"}, 
#' \code{"GDA heatmap"} and \code{"publications"}. The first two are network
#' representations of the second two. While the last one draws a barplot
#' with the number of PMIDs between genes and diseases.
#' @param verbose By default \code{FALSE}. If set to \code{TRUE} information
#' on the drawing process will be shown.
#' @param ... (Check NOTE section) Passed to inner functions for different 
#' plots.
#' @return A plot for \code{DataGeNET.Psy}.
#' @examples
#' data(qr)
#' plot(qr) # for all-disease plot
#' plot(qr, type = 'GDCA network') # for MPI plot
#' @note The "Evidence Index" is gotten from PsyGeNET. For more information
#' about it and its calculation, pease visit \code{psygenet.org}.
#' Argument \code{...} can be filled with specific argument depending on 
#' the type of plot:
#' \tabular{llll}{
#' \strong{Type}\tab\strong{Argument}\tab \strong{Color} \tab \strong{Description}\cr
#' GDA network  \tab geneColor       \tab Yellow  \tab Determines the color of the gene nodes\cr
#'              \tab diseaseColor    \tab Blue    \tab Determines the color of the disease nodes\cr
#' GDA heatmap  \tab highColor       \tab Blue    \tab Determines the color of the heatmap for the highest value\cr
#'              \tab lowColor        \tab White   \tab Determines the color of the heatmap for the lowest value\cr
#' GDCA network \tab ei0color        \tab Yellow  \tab Determines the color for those associations with evidence index 0\cr
#'              \tab eiAmbiguitcolor \tab Grey    \tab Determines the color for those associations with evidence index greater than 0 and lower than 1\cr
#'              \tab ei1color        \tab Blue    \tab Determines the color for those associations with evidence index 1\cr
#' GDCA heatmap \tab AUDcolor        \tab #FF3C32 \tab \cr
#'              \tab BDcolor         \tab #FFC698 \tab \cr
#'              \tab DEPcolor        \tab #9BE75E \tab \cr
#'              \tab SCHZcolor       \tab #1F6024 \tab \cr
#'              \tab CUDcolor        \tab #5AB69C \tab \cr
#'              \tab SIDEPcolor      \tab #50B8D6 \tab \cr
#'              \tab CanUDcolor      \tab #5467C3 \tab \cr
#'              \tab SYPSYcolor      \tab #A654C3 \tab \cr
#'              \tab geneColor       \tab Orange  \tab
#' }
#' @export plot
#' @aliases DataGeNET.Psy-plot
setMethod( 
    f = "plot",
    signature = "DataGeNET.Psy",
    definition = function( x, y, layout = igraph::layout.fruchterman.reingold,
                           type = "GDA network", verbose = FALSE, ... ) {
        plot_datagenet_psy( x, layout=layout, type=type, verbose=verbose, ... )
    }
    # @name plot
    # @rdname DataGeNET.Psy-methods
    # @aliases plot
)

#####
plot_datagenet_psy <- function( object, layout, type, verbose, ... ) {
    if( !type %in% c( "GDA network", "GDCA network", 
                      "GDCA heatmap", "GDA heatmap", "publications"
                      ) ) {
        stop( "Invalid content of argument 'type'." )
    }
    if( type == "GDA network" ) {
        plot_psy_disease( search = object@term, table = object@qresult, 
            layout = layout, verbose = verbose, ... )
    } else if( type == "GDCA network" ) {
        plot_psy_psychiatric( search = object@term, table = object@qresult, 
            layout = layout, verbose = verbose )
#     } else if( type == "venn" ) {
#         plot_psy_venn( search = object@term, table = object@qresult, 
#             verbose = verbose )
#     } else if( type == "vennA" ) {
#         plot_psy_vennAlternative( search = object@term, table = object@qresult, 
#             verbose = verbose )
    }else if( type == "publications" ) {
        plot_pmids_barplot( search = object@term, table = object@qresult, 
            class = object@type, verbose = verbose, ... )
    }else if( type == "GDA heatmap" ) {
        plot_psy_heatmapGenes( search = object@term, table = object@qresult, 
            verbose = verbose, ... )
#     } else if( type == "heatmapScore" ) {
#         plot_psy_heatmapScore( search = object@term, table = object@qresult, 
#             verbose = verbose, ... )
    } else if ( type == "GDCA heatmap" ) {
        if( object@type == "gene" ) {
            plot_psy_heatmap( search = object@term, table = object@qresult, 
                verbose = verbose )
        } else if( object@type == "disease" ) {
            plot_psy_heatmapDisease( search = object@term, 
                table = object@qresult, verbose = verbose )
        }
    } else {
        stop( paste0( "Invalid 'type' value. Accepted: 'disease', ",
        "'GDA network'  'GDCA network', 
        'publications', 'GDA heatmap' and 'GDCA heatmap'." ) )
    }
}
#####

#####
plot_psy_disease <- function( search, table, geneColor = "#FFFF33", diseaseColor = "#3385FF", layout, verbose, inc = 5, 
                              cutOff=FALSE ) {
    
    if(cutOff){
        table <- table[ table$c0.Score >= cutOff, ]
    }
    edges <- data.frame( table[ , 1 ], table[ , 6 ] )
    netw  <- igraph::graph.data.frame( edges, directed = FALSE )
    netw  <- igraph::simplify( netw )
    lay   <- layout( netw )
    
    if( verbose ) {
        message( "The network contains ", igraph::vcount( netw ), 
                 " nodes and ", igraph::ecount( netw ), " edges." )
    }
    
    diseases <- unique( table$c2.DiseaseName )
    
    ttl <- " "
    
    igraph::plot.igraph( netw,
                         vertex.frame.color = "white",
                         layout             = lay,
                         vertex.color       = ifelse( igraph::V( netw )$name %in% diseases, diseaseColor, geneColor ),
                         vertex.label.dist  = 0,      #puts the name labels slightly off the dots
                         vertex.frame.color = 'blue', #the color of the border of the dots
                         vertex.label.color = 'black',#the color of the name labels
                         vertex.label.font  = 0,      #the font of the name labels
                         vertex.label       = igraph::V( netw )$name, #specifies the lables of the vertices. in this case the 'name' attribute is used
                         edge.color         = "darkgrey",
                         edge.width         = 5,
                         edge.arrow.size    = 0.5,
                         vertex.size        = 10,
                         vertex.label.cex   = 0.8,    #specifies the size of the font of the labels
                         main               = ttl
    )
}
#####

#####
plot_psy_psychiatric <- function( search, table, geneColor = "#FFFF33",
                                  AUDcolor = "#FF3C32", BDcolor = "#FFC698", DEPcolor = "#9BE75E", 
                                  SCHZcolor = "#1F6024", CUDcolor = "#5AB69C", 
                                  SIDEPcolor = "#50B8D6", CanUDcolor = "#5467C3", SYPSYcolor = "#A654C3",
                                  layout, verbose, inc = 5 ) {

    table <- diseaseNameMapping( table )
    
    edges <- as.data.frame( table( data.frame( table[ , 1 ], table[ , 7 ], 
                                               stringsAsFactors = FALSE ) ) )
    colnames( edges ) <- c( "gene", "main", "count" )
    
    netw  <- igraph::graph.data.frame( edges, directed = FALSE )
    netw  <- igraph::simplify( netw )
    lay   <- layout( netw )
    
    genes <- as.character( unique( table[ , 1 ] ) )
    
    if( verbose ) {
        message( "The network contains ", 
                 length( unique( as.character( table[ , 7 ] ) ) ), 
                 " of the 8 possible psychiatric disorders." )
    }
    
    colors <- c( AUDcolor, BDcolor, DEPcolor, SCHZcolor, CUDcolor, 
                 SIDEPcolor, CanUDcolor, SYPSYcolor,
                 rep( geneColor, length( genes ) ) )
    
    names( colors ) <- c( "Alcohol use disorders", 
                          "Bipolar disorders and related disorders", 
                          "Depressive disorders", 
                          "Schizophrenia spectrum and other psychotic disorders", 
                          "Cocaine use disorders", 
                          "Substance induced depressive disorder", 
                          "Cannabis use disorders",
                          "Substance induced psychosis", genes )
    
    colors <- diseaseNameMapping( colors )
    
   
    if( length(search) == 1 ) {
        ss <- sum( as.numeric( edges[ , 3 ] ) )
        sz <- c( ( as.numeric( edges[ , 3 ] ) / ss ) * 100, 10 )
        names( sz ) <- c( as.character( edges$main ), genes )
        
        ttl <- " "
        
        igraph::plot.igraph( netw,
                             vertex.frame.color = "white",
                             layout             = lay,
                             vertex.color        = colors[ igraph::V( netw )$name ],
                             vertex.label.dist   = 0,      #puts the name labels slightly 
                             #off the dots
                             vertex.frame.color  = 'blue', #the color of the border of the dots
                             vertex.label.color  = 'black',#the color of the name labels
                             vertex.label.font   = 0,      #the font of the name labels
                             vertex.label        = igraph::V( netw )$name, #specifies the 
                             #lables of the vertices. in this 
                             #case the 'name' attribute is used
                             edge.color          = "darkgrey",
                             edge.arrow.size     = 0.5,
                             vertex.size         = sz[ igraph::V( netw )$name ],
                             vertex.label.cex    = 0.8,    #specifies the size of the font of
                             #the labels
                             main                = ttl
        )
    } else {
        ttl <- " "
        igraph::plot.igraph( netw,
                             vertex.frame.color  = "white",
                             layout              = lay,
                             vertex.color        = colors[ igraph::V( netw )$name ],
                             vertex.label.dist   = 0,      #puts the name labels slightly off 
                             #the dots
                             vertex.frame.color  = 'blue', #the color of the border of the dots
                             vertex.label.color  = 'black',#the color of the name labels
                             vertex.label.font   = 0,      #the font of the name labels
                             vertex.label        = igraph::V( netw )$names, #specifies the 
                             #lables of the vertices. in this 
                             #case the 'name' attribute is used
                             edge.color          = "darkgrey",
                             edge.width          = ( inc * as.numeric( 
                                 as.character( edges[, 3 ] ) ) ) / 30,
                             edge.arrow.size     = 0.5,
                             vertex.label.cex    = 0.8,    #specifies the size of the font 
                             #of the labels
                             main                = ttl
        )
    }
}
#####

#####
# plot_psy_venn <- function( search, table, verbose ) {
#     
#     if( length(search) == 1 ) {
#         stop( "For this type of chart, a multiple query created with 'psygenetGene' is required." )
#     }
#     
#     tt <- table( table[ , c( 1, 7 ) ] )
# 
#     a <- draw.triple.venn(
#         area1 = sum( tt[ ,"Alcohol-Use-Disorder" ] != 0 ),
#         area2 = sum( tt[ ,"Major-Depression" ] != 0 ),
#         area3 = sum( tt[ ,"Cocaine-Use-Disorder" ] != 0 ),
#         
#         n12   = sum( tt[ ,"Alcohol-Use-Disorder" ] != 0 & 
#                          tt[ ,"Major-Depression" ] != 0 ),
#         n23   = sum( tt[ ,"Major-Depression" ] != 0 & 
#                          tt[ ,"Cocaine-Use-Disorder" ] != 0 ),
#         n13   = sum( tt[ ,"Alcohol-Use-Disorder" ] != 0 & 
#                          tt[ ,"Cocaine-Use-Disorder" ] != 0 ),
#         
#         n123  = sum( tt[ ,"Alcohol-Use-Disorder" ] != 0 & 
#                          tt[ ,"Major-Depression" ] != 0 & 
#                          tt[ ,"Cocaine-Use-Disorder" ] != 0 ),
#         
#         category = c("Alcohol-Use-Disorder", "Major-Depression", 
#                      "Cocaine-Use-Disorder"),
#         lty = "blank",
#         fill = c( "#3F994B", "#715699", "#EDC832" )
#     )
# }
#####

#####
# plot_psy_vennAlternative <- function( search, table, verbose ) {
#     
#     if( length(search) == 1 ) {
#         stop( "For this type of chart, a multiple query created with 'psygenetGene' is required." )
#     }
#     
#     input = table
#     
#     bvenn::bvenn(list(
#         AlcoholUseDisorder = as.character(unique(input[
#             input$c2.PsychiatricDisorder=="Alcohol-Use-Disorder",][1])[,1]),
#         MajorDepression = as.character(unique(input[
#             input$c2.PsychiatricDisorder=="Major-Depression",][1])[,1]),
#         CocaineUseDisorder =as.character(unique(input[
#             input$c2.PsychiatricDisorder=="Cocaine-Use-Disorder",][1])[,1]),
#         colors = c("#edc832", "#3f994b", "#715699", 
#                "purple", "orange", "green", "brown")
#         ),
#     scale=0.8)
# }
#####

#####
plot_psy_heatmap <- function( search, table, lowColor = "#0000FF", highColor ="#004080", verbose ) {
    
    if( length(search) == 1 ) {
        stop( "For this type of chart, a multiple query created with ",
            "'psygenetGene' is required." )
    }
    
    table <- diseaseNameMapping( table )
    disorders <- as.character(unique(table$c2.PsychiatricDisorder))
    tt <- table( table[ , c( 1, 7 ) ] )
    diseasesNumber <- psygenetAll ( "ALL" )
    diseasesNumber <- diseaseNameMapping( diseasesNumber )
    
    
    for(i in 1:length(disorders)){
    cuis <- length ( unique ( 
    diseasesNumber[diseasesNumber$c2.PsychiatricDisorder == disorders[i], 5] ))
    tt[,disorders[i]] <-  (tt[,disorders[i]]/cuis)*100
    }
    
      
    p <- ggplot2::qplot( x = c2.PsychiatricDisorder, y = c1.Gene_Symbol, 
        data = reshape2::melt( tt ), 
        fill = value, geom = "tile" ) + 
        ggplot2::ylab( "Genes" ) + ggplot2::xlab( "Psychiatric Disorder" ) + 
        ggplot2::ggtitle( " ") + 
        ggplot2::scale_fill_gradient2(low = lowColor, high = highColor, 
        space = "rgb", guide = "colourbar")
    p <- p + ggplot2::theme( 
        plot.margin = grid::unit( x=c( 5, 15, 5,15 ), units="mm" ) , 
        axis.line = ggplot2::element_line(size = 0.7, color = "black"), 
        text = ggplot2::element_text(size = 12), 
        axis.text.x = ggplot2::element_text(angle=45, size = 12, hjust = 1), 
        panel.background = ggplot2::element_blank())
    p <- p + ggplot2::guides(fill=ggplot2::guide_legend(title="%"))
    return(p)
}
#####

#####
plot_psy_heatmapGenes <- function( search, ei0color = "#FFFF33", eiAmbiguitycolor = "grey", ei1color = "#3385FF", table, verbose ) {
        
    if( length(search) == 1 ) {
        stop( "For this type of chart, a multiple query created with ",
              "'psygenetGene' is required." )
    }
    
    tt <- ( table[ , c( 1, 6, 8 ) ] )
    tt <- tt [ with ( tt , order(-c0.Score)), ]

    
    for(i in 1:nrow(tt)){
        if(tt$c0.Score[i] > 0 & tt$c0.Score[i] < 1){
            tt$c0.Score[i] <- "> 0 & < 1"
        }
    }
    
    h.value = c(0,"> 0 & < 1", 1) 
    h.color = c(ei0color, eiAmbiguitycolor, ei1color)
    heatmapColors = data.frame(h.value, h.color)
    hColors <- as.character(heatmapColors$h.color)
    names(hColors) <- heatmapColors$h.value
    


    p <- ggplot2::ggplot(data = tt, ggplot2::aes(x = c1.Gene_Symbol, y = c2.DiseaseName)) +
        ggplot2::theme_grey( base_size = 12 ) +
        ggplot2::theme(
            axis.text = ggplot2::element_text ( size = 11 ), 
            axis.title = ggplot2::element_text( size = 11 ), 
            axis.line = ggplot2::element_line(size = 0.7, color = "black"), 
            axis.text.x = ggplot2::element_text(angle=45,size=10,hjust=1), 
            plot.margin = grid::unit(x=c(5,15,5,15), units="mm"),
            text = ggplot2::element_text(size = 11), 
            panel.background = ggplot2::element_blank()
        ) +

        ggplot2::ylab( "Disease" ) + ggplot2::xlab( "Gene" ) +
        ggplot2::guides( fill=ggplot2::guide_legend( title="Evidence Index" ) ) +
        ggplot2::scale_x_discrete( expand = c( 0, 0 ) ) +
        ggplot2::geom_tile(color = "white", ggplot2::aes(fill = factor(c0.Score))) + 
        ggplot2::scale_fill_manual(values = hColors)
    
    return(p)
}
#####

#####
plot_psy_heatmapDisease <- function( search, ei0color = "#FFFF33", eiAmbiguitycolor = "grey", ei1color = "#3385FF", table, verbose) {
    
    if( length(search) == 1 ) {
        stop( "For this type of chart, a multiple query created with ",
            "'psygenetDisease' is required." )
    }
    
    tt <- ( table[ , c( 1, 6, 8 ) ] )
    tt <- tt [ with ( tt , order(-c0.Score)), ]

    
    for(i in 1:nrow(tt)){
        if(tt$c0.Score[i] > 0 & tt$c0.Score[i] < 1){
            tt$c0.Score[i] <- "> 0 & < 1"
        }
    }
    
    h.value = c(0, "> 0 & < 1", 1) 
    h.color = c(ei0color, eiAmbiguitycolor, ei1color)
    heatmapColors = data.frame(h.value, h.color)
    hColors <- as.character(heatmapColors$h.color)
    names(hColors) <- heatmapColors$h.value
    

    p <- ggplot2::ggplot(data = tt, ggplot2::aes(x = c2.DiseaseName, y = c1.Gene_Symbol)) +
        ggplot2::theme_grey( base_size = 12 ) +
        ggplot2::theme(
            axis.text = ggplot2::element_text ( size = 11 ), 
            axis.title = ggplot2::element_text( size = 11 ), 
            axis.line = ggplot2::element_line(size = 0.7, color = "black"), 
            axis.text.x = ggplot2::element_text(angle=45,size=10,hjust=1), 
            plot.margin = grid::unit(x=c(5,15,5,15), units="mm"),
            text = ggplot2::element_text(size = 11), 
            panel.background = ggplot2::element_blank()
        ) +
        
        ggplot2::ylab( "Gene" ) + ggplot2::xlab( "Disease" ) +
        ggplot2::guides( fill=ggplot2::guide_legend( title="Evidence Index" ) ) +
        ggplot2::scale_x_discrete( expand = c( 0, 0 ) ) +
        ggplot2::geom_tile(color = "white", ggplot2::aes(fill = factor(c0.Score))) + 
        ggplot2::scale_fill_manual( values = hColors)

    return(p)

}
#####

#####
plot_pmids_barplot <- function ( table, class, name, type, search, barColor = "grey", verbose ) {
    ## Transform input table to character
    table$c1.Gene_Symbol <- as.character(table$c1.Gene_Symbol)
    table$c1.Gene_Id <- as.character(table$c1.Gene_Id)
    table$c2.Disease_Id <- as.character(table$c2.Disease_Id)
    table$c2.Disease_code <- as.character(table$c2.Disease_code)
    table$c2.DiseaseName <- as.character(table$c2.DiseaseName)
    ## /
    
    inTable <- list( status = FALSE, ncol = 0, look = "" )
    ## Check for disease
    if( substr ( name, 1, 4 ) == "umls" ) {
        if( name %in% table$c2.Disease_Id ) {
            inTable <- list( status = TRUE, ncol = 4, look = "disease" )
        }
    } else if( substr ( name, 1, 1 ) == "C" & ( nchar( name ) == 8 ) ) {
        if( name %in% table$c2.Disease_code ){
            inTable <- list( status = TRUE, ncol = 5, look = "disease" )
        }
    } else if( name %in% table$c2.DiseaseName ){
        inTable <- list( status = TRUE, ncol = 6, look = "disease" )
    }
    ## /
    
    ## Check for gene
    if( name %in% table$c1.Gene_Symbol ) {
        inTable <- list( status = TRUE, ncol = 1, look = "gene" )
    } else if ( name %in% table$c1.Gene_Id ) {
        inTable <- list( status = TRUE, ncol = 2, look = "gene" )
    }
    ## /
        
    if( !inTable$status ) {
        stop( paste0( "'", name, "' is not present in this DataGeNET.Psy object." ) )
    }
    
    ## Create and order table to plot and set X axis
    input <- table[ table[ , inTable$ncol ] == name, ]

    #   Show diseases that have given gene
    if( ( class == "gene" & inTable$look == "gene" ) | ( class == "disease" & inTable$look == "gene" ) ) {
        orderedPubmed <- input[order( -input["c0.Number_of_Abstracts"] ), "c2.Disease_code"]
        input$c2.Disease_code <- factor( input$c2.DiseaseName, levels = as.factor( unique( orderedPubmed ) ), ordered = TRUE )
        p <- ggplot2::ggplot(input, ggplot2::aes ( x = c2.DiseaseName, y = c0.Number_of_Abstracts ), order = as.numeric(c0.Number_of_Abstracts) )
        x <- "diseases"
    }
    
    #   Show genes with given disease
    if( ( class == "disease" & inTable$look == "disease" ) | ( class == "gene" & inTable$look == "disease" ) ) {
        orderedPubmed <- input[order( -input["c0.Number_of_Abstracts"] ), "c1.Gene_Symbol"]
        input$c1.Gene_Symbol <- factor( input$c1.Gene_Symbol, levels = as.factor( unique( orderedPubmed ) ), ordered = TRUE )
        p <- ggplot2::ggplot(input, ggplot2::aes ( x = c1.Gene_Symbol, y = c0.Number_of_Abstracts ), order = as.numeric(c0.Number_of_Abstracts) )
        x <- "genes"
    }
    ## /
    
    ## Draw the plot
    p <- p + ggplot2::geom_bar ( stat = "identity", fill = barColor ) +
      ggplot2::labs ( title = " ", x = x, y = "# of pmids") +
      ggplot2::theme_classic( ) + 
      ggplot2::theme( plot.margin = grid::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                      axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), 
                      text = ggplot2::element_text ( size = 14 ) ,
                      axis.text.x = ggplot2::element_text ( angle = 45, size = 9, hjust = 1 ) )
    return(p)
    ## /

}
#####
  


#####
plot_psy_heatmapScore <- function( search, table, verbose, cutOff=0 ) {
  
  if( length(search) == 1 ) {
    stop( "For this type of chart, a multiple query created with 'psygenetGene' is required." )
  }
  
  tt <- table[ , c( 1, 7, 8 ) ] 
  tt$pairs <- paste(tt[,1], tt[,2], sep="-")
  
  rr <- tt[!duplicated(tt$pairs), ]
  rr$c0.Score <- 0
  
  for ( i in 1: nrow( rr ) ){
    score <- tt[tt$pairs == rr$pairs[i],]
    rr$c0.Score[ i ] <- mean(score$c0.Score)
  } 
  
  rr <- rr[,c(1:3)]
  rr <- rr[ rr$c0.Score > cutOff, ]

  p <- ggplot2::qplot( x = rr$c2.PsychiatricDisorder, y = rr$c1.Gene_Symbol, data = rr, fill = rr$c0.Score, geom = "tile" ) + ggplot2::ylab( "Genes" ) + ggplot2::xlab( "Psychiatric Disorder" ) + ggplot2::ggtitle( " ") + ggplot2::scale_fill_gradient2(low = "#ffe7cf", high ="#ff8c1b", space = "rgb", guide = "colourbar")
  p <- p + ggplot2::theme( plot.margin = grid::unit( x=c( 5, 15, 5,15 ), units="mm" ) , axis.line = ggplot2::element_line(size = 0.7, color = "black"), text = ggplot2::element_text(size = 12), axis.text.x = ggplot2::element_text(angle=45, size = 12, hjust = 1), panel.background = ggplot2::element_blank())
  p <- p + ggplot2::guides(fill=ggplot2::guide_legend(title="evidenceIndex-Mean"))
  p
  return(p)
}
#####
