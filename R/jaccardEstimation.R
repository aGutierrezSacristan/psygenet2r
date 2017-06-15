#' Calculation of the Jaccard Index between ideseases
#'
#' This function is able to calculate the Jacard Index between: 1. muliple
#' disases, 2. a set og genes and multiple diseases, 3. a set of genes and
#' multiple main psychiatric disorders and 4. multiple diseases and multiple
#' main psychiatric disorders.
#' 
#' Warning: The main psychiatric disorders are understood as a single set
#' of genes composed by the genes of all the diseases that the main 
#' psychiatric disorder cotains.
#'
#' @name jaccardEstimation
#' @rdname jaccardEstimation-methods
#' @aliases jaccardEstimation
#' @param pDisease vector of diseases, vector of genes, vector of main 
#' psychiatric disorder.
#' @param sDisease vector of diseases, vector of genes, vector of main 
#' psychiatric disorder. Only necessary when comparing genes vs. diseases,
#' genes vs. main psychiatric disorders or diseases vs. main psychiatric 
#' disorders. To compare multiple diseases only use \code{pDisease}.
#' @param database Name of the database that will be queried. It can take the 
#' values \code{'psycur15'} to use data validated by experts for first release 
#' of PsyGeNET; \code{'psycur16'} to use data validated by experts for second 
#' release of PsyGeNET; or \code{'ALL'} to use both databases. 
#' @param nboot Number of iterations sued to compute the pvalue associted
#' to the calculated Jaccard Index (default 100).
#' @param ncores Number of cores used to calculate the pvalue associated to
#' the computed Jaccard Index (default 1).
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get a
#' on-time log from the function.
#' @return An object of class \code{JaccardIndexPsy} with the computed
#' calculation of the JaccardIndex.
#' @examples
#' ji <- jaccardEstimation( c( "COMT", "CLOCK", "DRD3" ), "umls:C0005586", "ALL" )
#' @export jaccardEstimation
jaccardEstimation <- function(pDisease, sDisease, database="ALL", nboot = 100, ncores = 1, verbose = FALSE) {
  if(missing(pDisease)) {
    stop("Argument 'pDisease' must be set. Argument 'sDisease' is optional.")
  }
  
  if(verbose) message("Query PsyGeNET for generic diseases.")
  psy <- psygenetAll ( database )
  #universe <- disGenetCurated()
  load(system.file("extdata", "disgenetCuratedUniverse.RData", package="psygenet2r"))
  
  diseases <- getDiseasesType( pDisease, psy, verbose )
  
  if(missing(sDisease)) {
    out <- singleInput(diseases, diseases$type, universe, psy, nboot, ncores, verbose)
  } else {
    diseases2 <- getDiseasesType( sDisease, psy, verbose )
    out <- multipleInput(diseases$diseases, diseases$type, diseases2$diseases, diseases2$type, universe, nboot, ncores, verbose)
  }
  
  return(out)
}

singleInput <- function(diseases, type, universe, psy, nboot, ncores, verbose) {
  if(type != "dise") {
    return(singleInput.genes(diseases$diseases$geneList$genes, psy, universe, nboot, ncores, verbose))
    #stop("Jaccard Index only allows single input if 'pDiseases' is a vector of diseases (Given: ", type, ").")
  }
  if(length(diseases) <= 1){
    stop("Jaccard Index needs, at last, two elements to be calculated.")
  }
  diseases <- diseases$diseases
  items <- combn(names(diseases), 2)
  xx <- lapply(1:ncol(items), function(nc) {
    it1 <- diseases[[items[1, nc]]]$genes
    it2 <- diseases[[items[2, nc]]]$genes
    ji  <- sum(it1 %in% it2) * 1.0 / length(unique(c(it1, it2)))
    bb <- ji.internal(length(it1), length(it2), universe, nboot, ncores)
    pval <- (sum(bb > ji) * 1.0) / (nboot+1)
    return(c(items[1, nc], items[2, nc], length(it1), length(it2), ji, pval))
  })
  xx <- data.frame(do.call(rbind, xx))
  rownames(xx) <- 1:nrow(xx)
  colnames(xx) <- c("Disease1", "Disease2", "NGenes1", "NGenes2", "JaccardIndex", "pval")
  
  new("JaccardIndexPsy", table = xx, type = "disease-disease", nit = nboot, i1 = names(diseases), i2 = "")
}

singleInput.genes <- function(genes, database, universe, nboot, ncores, verbose) {
  warning("Jaccard Index for all diseases in PsyGeNET will be calculated.")
  xx <- parallel::mclapply(unique(as.character(database$c2.DiseaseName)), function(dCode) {
    disease <- database[database$c2.DiseaseName == dCode, "c1.Gene_Symbol"]
    
    ji <- sum(genes %in% disease) * 1.0 / length(unique(c(genes, disease)))
    bb <- ji.internal(length(genes), length(disease), universe, nboot, ncores)
    
    pval <- (sum(bb > ji) * 1.0) / (nboot+1)
    return(c(dCode, length(genes), length(disease), ji, pval))
  }, mc.cores = ncores)
  
  xx <- data.frame(disease1="genes", do.call(rbind, xx))
  rownames(xx) <- 1:nrow(xx)
  colnames(xx) <- c("Disease1", "Disease2", "NGenes1", "NGenes2", "JaccardIndex", "pval")
  
  new("JaccardIndexPsy", table = xx, type = "geneList - disease", nit = nboot, i1 = genes, i2 = "PsyGeNET")
}

multipleInput <- function(primary, typeP, secondary, typeS, universe, nboot, ncores, verbose) {
  if(typeP == typeS) {
    stop("Invalid input type for 'pDisease' and 'sDisease'.")
  }
  
  xx <- lapply(names(primary), function(nn1) {
    data.frame(do.call(rbind, lapply(names(secondary), function(nn2) {
      it1 <- primary[[nn1]]$genes
      it2 <- secondary[[nn2]]$genes
      ji  <- sum(it1 %in% it2) * 1.0 / length(unique(c(it1, it2)))
      bb <- ji.internal(length(it1), length(it2), universe, nboot, ncores)
      pval <- (sum(bb > ji) * 1.0) / (nboot+1)
      return(c(nn1, nn2, length(it1), length(it2), ji, pval))
    })))
  })
  xx <- data.frame(do.call(rbind, xx))
  rownames(xx) <- 1:nrow(xx)
  colnames(xx) <- c("Disease1", "Disease2", "NGenes1", "NGenes2", "JaccardIndex", "pval")
  
  new("JaccardIndexPsy", table = xx, type = paste0(typeP, " - ", typeS), nit = nboot, i1 = names(primary), i2 = names(secondary))
}

getDiseasesType <- function(pDiseases, psy, verbose = TRUE) {
  mpds <- as.character(unique(psy$c2.PsychiatricDisorder))
  cuis <- as.character(unique(psy$c2.Disease_code))
  umls <- as.character(unique(psy$c2.Disease_Id))
  nmms <- as.character(unique(psy$c2.DiseaseName))
  
  type <- NA
  diseases <- lapply(1:length(pDiseases), function(ii) {
    it1 <- pDiseases[ii]
    if (verbose) {
      message("Checking disorder/disease/gene '", it1, "' (", ii, " of ", length(pDiseases), ").")
    }
    if( it1 %in% mpds) {
      if (is.na(type) | (!is.na(type) & type == "mpds")) {
        it1 <- list( name=it1, genes=as.character( unique( psy [ psy$c2.PsychiatricDisorder == it1, 1 ] ) ) )
        type <<- "mpds"
      } else {
        stop("1 All input diseases msut be psyquiatric disorders, diseases (cui or name) or genes.")
      }
    } else if( it1 %in% cuis ) {
      if (is.na(type) | (!is.na(type) & type == "dise")) {
        it1 <- list( name=it1, genes=as.character( unique( psy [ psy$c2.Disease_code == it1, 1 ] ) ) )
        type <<- "dise"
      } else {
        stop("2 All input diseases msut be psyquiatric disorders, diseases (cui or name) or genes.")
      }
    } else if( it1 %in% umls ) {
      if (is.na(type) | (!is.na(type) & type == "dise")) {
        it1 <- list( name=it1, genes=as.character( unique( psy [ psy$c2.Disease_Id == it1, 1 ] ) ) )
        type <<- "dise"
      } else {
        stop("3 All input diseases msut be psyquiatric disorders, diseases (cui or name) or genes.")
      }
    } else if( it1 %in% nmms ) {
      if (is.na(type) | (!is.na(type) & type == "dise")) {
        it1 <- list( name=it1, genes=as.character( unique( psy [ psy$c2.DiseaseName == it1, 1 ] ) ) )
        type <<- "dise"
      } else {
        stop("4 All input diseases msut be psyquiatric disorders, diseases (cui or name) or genes.")
      }
    } else {
      if (is.na(type) | (!is.na(type) & type == "geneList")) {
        it1 <- list( name="gene list", genes=it1 )
        type <<- "geneList"
      } else {
        stop("5 All input diseases msut be psyquiatric disorders, diseases (cui or name) or genes.")
      }
    }
    return(it1)
  })
  if(type == "geneList") {
    diseases <- list( list( name = "geneList", genes = pDiseases ) )
    names(diseases) <- "geneList"
  } else {
    names(diseases) <- pDiseases
  }
  return(list(diseases=diseases, type=type))
}

ji.internal <- function(len1, len2, universe, nboot, ncores) {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    pfun <- lapply
  } else {
    pfun <- parallel::mclapply
  }
  
  unlist(pfun(1:nboot, function(ii) {
    g1 <- sample( universe, len1 )
    g2 <- sample( universe, len2 )
    ja.coefr <- length(intersect(g1, g2)) / length(union(g1, g2))
  }, mc.cores = ncores))
}
