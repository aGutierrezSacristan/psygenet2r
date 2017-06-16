#' Plot the content of a \code{JaccardIndexPsy} object.
#' 
#' This functions llows to create a variety of plots for \code{DataGeNEt.Psy}
#' and \code{JaccardIndexPsy} objects.
#'
#' @param x Object of class \code{JaccardIndexPsy}.
#' @param y NOT USED
#' @param cutOff Number to filter the shown results.
#' @param zero.remove By deffault \code{TRUE}. It removes those relations
#' with a Jaccard Index of 0.
#' @param noTitle By default \code{FALSE}. If set to true no title will be
#' added to the plot.
#' @param lowColor By default \code{"white"}. It can be changed to any other color. 
#' @param highColor By default \code{"mediumorchid4"}. It can be changed to any other color. 
#' @param verbose By default \code{FALSE}. If set to \code{TRUE} information
#' on the drawing process will be shown.
#' @param ... NOT USED
#' @return A plot for \code{JaccardIndexPsy}.
#' @examples
#' \dontrun{
#' #Being x an JaccardIndexPsy
#' qr <- plot(x)
#' }
#' @export plot
setMethod( "plot",
           signature = "JaccardIndexPsy",
           definition = function( x, y, cutOff, zero.remove = TRUE, noTitle = FALSE, lowColor = "white", highColor = "mediumorchid4", verbose = FALSE, ... ) {
   
   show.pval = FALSE
   input <- x@table
   input$pval <- as.numeric(as.character(input$pval))
   input$JaccardIndex <- as.numeric(as.character( input$JaccardIndex ))
   if(zero.remove) {
     input <- input[ input$JaccardIndex != 0, ]
   }
   if(!missing(cutOff)) {
    input <- input[ input$JaccardIndex >= cutOff, ]
   }
   # @name plot
   # @rdname JaccardIndexPsy-methods
   # @aliases plot
   if(length(unique(input$Disease1))>1){
     m <- max( input$JaccardIndex )
     p <- ggplot2::ggplot( input, ggplot2::aes( x = Disease1, y = Disease2 ) ) + 
       ggplot2::geom_tile(ggplot2::aes(fill = JaccardIndex), colour = "white") + 
       ggplot2::scale_fill_gradient(limits = c(0,m), low = lowColor,   high = highColor, na.value = "white") +
       ggplot2::theme_grey(base_size = 9) + 
       ggplot2::labs( x = "input diseases",  y = "associated diseases") + 
       ggplot2::scale_x_discrete(expand = c(0, 0)) +
       ggplot2::theme( plot.margin=unit(x=c(5,15,5,15),units="mm") , axis.line = ggplot2::element_line(size = 0.5, color = "black"), text = ggplot2::element_text(size = 9), axis.text.x = ggplot2::element_text(angle=45, size = 9, hjust = 1), panel.background = ggplot2::element_blank())
     p
   } else if (length(unique(input$Disease1))==1){
     orderedJaccard <- input[order(-input["JaccardIndex"]), "Disease2" ]
     input$Disease2 <- factor(input$Disease2, levels= as.factor(orderedJaccard))
     input$pval <- -log2(input$pval)
     
     if( show.pval ) {
       input <- reshape2::melt(input[ , c("Disease2", "JaccardIndex", "pval")], id.vars="Disease2", measure.vars=c("JaccardIndex", "pval"))
       p <- ggplot2::ggplot( input, ggplot2::aes( x = Disease2, y = value, fill = variable ) ) + 
         ggplot2::geom_bar( stat = "identity", position="dodge" ) +
         ggplot2::labs ( x = "associated diseases", y = "")
         
     } else {
       p <- ggplot2::ggplot( input, ggplot2::aes ( x = Disease2, y = JaccardIndex ), order = as.numeric(JaccardIndex) ) +
         ggplot2::geom_bar ( stat = "identity", fill = highColor ) +
         ggplot2::labs ( x = "associated diseases", y = "jaccard index")
     }
     p <- p + ggplot2::theme_classic( ) + 
       ggplot2::theme( plot.margin = unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                       axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), 
                       text = ggplot2::element_text ( size = 14 ) ,
                       axis.text.x = ggplot2::element_text ( angle = 45, size = 9, hjust = 1 ))
     if(!noTitle) {
       p <- p + ggplot2::ggtitle( paste0( "Jaccard Index of '", input$Disease1, "'" ) )
     }
     p
   } else {
     stop("No data in given 'JaccardIndexPsy'.")
   }
 }
)
 
