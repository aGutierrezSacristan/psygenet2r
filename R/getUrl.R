getUrlPsi <- function() {
  url <- "http://www.psygenet.org/oql"
  return( url )
}

getUrlDis <- function() {
  url <- "http://www.disgenet.org/oql"
  return( url )
}

download_data <- function(oql, url = getUrlPsi()) {
    dataTsv <- tryCatch({
        RCurl::getURL(
            url, 
            readfunction  = charToRaw( oql ), 
            upload        = TRUE, 
            customrequest = "POST"
        )}, 
    error=function(err) {
        rawToChar(RCurl::getURL(
            url, 
            readfunction  = charToRaw( oql ), 
            upload        = TRUE, 
            customrequest = "POST"
        ))
    })
    return(dataTsv)
}