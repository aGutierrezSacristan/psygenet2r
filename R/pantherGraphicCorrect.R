#' Query PsyGeNET for given genes and creates a representation in base of
#' their panther-class
#'
#' Given a vector of genes of interest (or using a DataGeNET.Psy object), this
#' function creates a representation of a the panther-class these genes belongs
#' to.
#'
#' @name pantherGraphic
#' @rdname pantherGraphic-methods
#' @aliases pantherGraphic
#' @param x Vector of genes of interest of \code{DataGeNET.Psy} resulting of
#' \code{psyegnetDisease}.
#' @param database Name of the database that will be queried. It can take the 
#' values \code{'MODELS'} to use Comparative Toxigenomics Database, data from 
#' mouse and rat; \code{'GAD'} to use Genetic Association Database; \code{'CTD'}
#' to use Comparative Toxigenomics Database, data from human; \code{'PsyCUR'} to
#' use Psychiatric disorders Gene association manually curated; \code{'CURATED'}
#' to use Human, manually curated databases (PsyCUR and CTD); or \code{'ALL'} 
#' to use all these databases. Default \code{'CURATED'}.
#' @param score threshold to take into account a gene in the analysis
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @param check By default \code{TRUE}. Change it to \code{FALSE} to not
#' validate the genes.
#' @param hostMart The URL of Biomart to be used.
#' @return A plot for a \code{DataGeNET.Psy} in terms of the panther-class.
#' @examples
#' d.alch <- pantherGraphic( c( "COMT", "CLOCK", "DRD3" ), "CURATED" )
#' @export pantherGraphic
pantherGraphic <- function ( x, database = "CURATED", score, verbose = FALSE, check = TRUE, hostMart = "www.ensembl.org" ) {
  if( class( x ) == "DataGeNET.Psy" ) {
    if( x@type == "disease" ) {
      if( !missing( score ) ) {
        tmp_result <- x@qresult[ x@qresult$c0.Score >= score, ]
        geneList <- as.character( tmp_result$c1.Gene_Symbol )
      } else {
        geneList <- as.character( x@qresult$c1.Gene_Symbol )
      }
    } else {
      stop( "Invalid 'type' of 'DatageNET' object. Expected result of 'psygenetDisease' or 'psygenetDiseaseList'." )
    }
  } else if( class( x ) == "character" ) {
    geneList <- x
  }
  
  pantherFile <- psyPanther ( database )
  pantherFile[,3] <- gsub ( "0", "Unclassified", pantherFile[,3] )
  panther <- as.data.frame( transPantherFile ( pantherFile ) )
  
  ourList <- psygenetGene( geneList, database, verbose = verbose, hostMart = hostMart, check = check )
  ourList <- ourList@qresult [ , c( 1,2,4,7 ) ]
  
  # estimate frequency
  disorders <- as.character(unique(ourList$c2.PsychiatricDisorder))
  results <- as.data.frame(matrix(ncol=ncol(ourList), nrow=1))
  colnames(results) <- c("Var1", "Freq", "perc", "diseases")
  
  for(i in 1:length(disorders)){
    temp <- freq_function ( panther, ourList,  disorders[i] )
    results <- rbind(results, temp)
  }
  
  if(length(disorders)>1){
    all <- freq_functionAll ( panther, ourList, "All" )
    results <- rbind(results, all)
  }
  
  results <- results[c(2:nrow(results)),]

  
  for( i in 1:nrow(results)){
    results$Var1 <- as.character( results$Var1 )
    if( nchar(results$Var1[i])>20){
      results$Var1[i] <- paste0(substr( results$Var1[i], 0, 20 ) , "...")
      
    }
  }
  
  
  #add the panther name and sort the results before plot them
  results2 <- results [ results$diseases == "All", ]
  misPanterSOrted <- results2[ with ( results2, order ( -as.numeric(Freq) ) ), ]$Var1
  results$Var1 <- factor ( results$Var1 , levels = as.factor ( misPanterSOrted ) )
  
  setsOrder <- c ( "Cocaine-Use-Disorder", "Alcohol-Use-Disorder", "Major-Depression", "All" )
  results$diseases <- factor ( results$diseases , levels = as.factor(setsOrder ) )
  results <- results [ results$perc != 0, ]
  
  colors <- c( "yellow3", "limegreen", "purple2", "gray60" )
  names( colors ) <- setsOrder
  
  # plot the results
  miplot <- ggplot2::ggplot ( results, ggplot2::aes ( x = Var1, y = perc, fill = diseases ) ) +
    ggplot2::geom_bar ( stat = "identity", position="dodge" ) + 
    ggplot2::labs ( x = "panther class", y = "percentage of genes" ) +
    ggplot2::theme_classic() + 
    ggplot2::theme( axis.line = ggplot2::element_line ( size = 0.7, color = "black" ) , text = ggplot2::element_text ( size = 14 ) , axis.text.x = ggplot2::element_text ( size = 14, hjust = 1 ) ) +
    ggplot2::scale_fill_manual ( values = colors, guide = ggplot2::guide_legend ( reverse=TRUE ) ) + 
    ggplot2::coord_flip() + 
    ggplot2::guides(fill = ggplot2::guide_legend(title="Psychiatric Disorders"))
  return ( miplot )
}

###############################################################################

psyPanther <- function( database ) {
  
  oql <- "DEFINE
  c0='/data/genes2disease',
  c1='/data/genes',
  c2='/data/datasources'
  ON
  'http://www.psygenet.org/web/PsyGeNET'
  SELECT
  c1 (Gene_Symbol, Gene_Id, Gene_Description, panther_class_name),
  c0 (Disease_Number)
  FROM
  c0
  WHERE
  c2 = 'DB'
  ORDER BY
  c0.Disease_Number DESC" 
  
  oql <- stringr::str_replace(
      string      = oql,
      pattern     = "DB",
      replacement = database 
  )
  
  dataTsv <- RCurl::getURLContent(
    getUrlPsi(), 
    readfunction  = charToRaw(oql), 
    upload        = TRUE, 
    customrequest = "POST"
  )
  data <- read.csv( textConnection(dataTsv), header = TRUE, sep = "\t" )
  
  pantherClass <- data[ !duplicated( data[ , 1 ] ), ]
  pantherClass <- pantherClass[ , c( 1, 2, 4 ) ]
  return ( pantherClass )
}

###############################################################################

freq_function <- function(panther, genDis, disease){
  
  disorder <- genDis[genDis$c2.PsychiatricDisorder==disease,]
  panther_class <- panther[panther$c1.Gene_Symbol %in% disorder$c1.Gene_Symbol,]
  panther_class$V3 <- as.character(panther_class$V3)
  freq_table <- table(panther_class$V3)
  freq_table <- as.data.frame(freq_table)
  
  
  freq_table$Freq <- as.numeric(freq_table$Freq)
  freq_table$perc <- (freq_table$Freq/length(unique(disorder$c1.Gene_Id)))*100
  freq_table$diseases <- disease
  return(freq_table)
}

###############################################################################

freq_functionAll <- function(panther, genDis, disease){
  panther_class <- panther[panther$c1.Gene_Symbol %in% genDis$c1.Gene_Symbol,]
  panther_class$V3 <- as.character(panther_class$V3)
  freq_table <- table(panther_class$V3)
  freq_table <- as.data.frame(freq_table)
  
  
  freq_table$Freq <- as.numeric(freq_table$Freq)
  freq_table$perc <- (freq_table$Freq/length(unique(genDis$c1.Gene_Id)))*100
  freq_table$diseases <- disease
  return(freq_table)
}

###############################################################################

transPantherFile <- function( pantherList ) {
  t( data.frame( apply( pantherList, 1, function( row ) {
    c1.Gene_Symbol <- row[ 1 ]
    c1.Gene_Id <- row[ 2 ]
    panther <- strsplit( row[ 3 ], ";" )[[ 1 ]]
    sapply( panther, function( x ) {
      c( c1.Gene_Symbol, c1.Gene_Id, x )
    } )
  })))
}


