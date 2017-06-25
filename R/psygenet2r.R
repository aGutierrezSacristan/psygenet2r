#' psygenet2r: Package to query PsyGeNET database and to perform comorbidity studies
#' 
#' psygenet2r has two categories of functions: querying functions and 
#' analysis and plotting functions.
#' 
#' @section querying functions:
#' The functions to retrieve data from PsyGeNET are \link{psygenetDisease} and
#' \link{psygenetGene}. There are some other support functions like
#' \link{psygenetGeneSentences}.
#' 
#' @section analysis and plotting functions:
#' The functions \link{extract} and \link{extractSentences} allows to retrieve
#' the row data obtained from on-line resources. The functions \code{plot} and
#' \link{pantherGraphic} draws a variety of charts to illustrate the obtained 
#' results. The function \link{enrichedPD} was built to perform enrichment 
#' studies on PsyGeNET data. Finally the function \link{jaccardEstimation} 
#' computes a Jaccard Index from a given input on PsyGeNET data.
#' 
#' @docType package
#' @name psygenet2r
#' 
#' @import Biobase
#' @import BiocInstaller
#' @import labeling
#' @import GO.db
#' 
#' @importFrom biomaRt useMart getBM
#' @importFrom igraph graph.data.frame simplify vcount ecount V plot.igraph
#' @importFrom ggplot2 labs qplot aes theme guides geom_bar guide_legend element_line ylab xlab element_text ggtitle scale_fill_gradient2 element_blank element_rect theme_grey theme_classic scale_x_discrete coord_flip
#' @importFrom grid unit
#' @importFrom reshape2 melt
#' @importFrom stringr str_replace
#' @importFrom RCurl getURLContent
#' @importFrom parallel mclapply
NULL