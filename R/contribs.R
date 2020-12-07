#############################################################################
#' lists contributions of a specific wikipedia page
#' 
#' @description provide the list of the contributions of a wikipedia page: discussions for Talk pages, or revisions for Subject pages.
#' 
#' @param page numeric identifier or character title of the specific wikipedia page
#' @param domain a character value specifying the language of the wikipedia page.The default value is "en" for "english language".
#' @param rvprop Which properties to get for each revision (separate with '|'):
#' \itemize{
#' \item  ids: the ID of the revision
#' \item  flags: revision flags (minor)
#' \item  timestamp: the timestamp of the revision, i.e. day and time
#' \item  user: user that made the revision
#' \item  userid: user id of revision creator
#' \item  size: length (bytes) of the revision
#' \item  sha1: SHA-1 (base 16) of the revision
#' \item  contentmodel: content model id
#' \item  comment: comment by the user for revision
#' \item  parsedcomment: parsed comment by the user for the revision
#' \item  content: text of the revision
#' \item  tags: tags for the revision
#' }
#' Default: user|userid|timestamp
#' 
#' @return an object of class \code{contribsClass}:
#' \itemize{
#' \item{\code{call}}{ the command line}
#' \item{\code{page} }{ title and identification number of the Wikipedia page}
#' \item{\code{contribs}}{ a data frame containing asked properties of the contribs, by default : user, userid, timestamp.
#' If the user does not exist or has no contribution, this item is not created.}
#' \item{\code{testWikiPage}}{ a list of four elements,
#'    \itemize{
#'    \item{\code{takeOnlyFirst}}{ a boolean indicating if the class of \code{page} parameter is invalid, 
#'              for example vector, list, matrix..., and in that case, only the first element is considered.}
#'    \item{\code{redirPage}}{ title of the redirected page. This item is NULL if the page is not redirected.}
#'    \item{\code{test}}{ an integer with value: 
#'      \itemize{
#'      \item{4}{ for invalid domain,}
#'      \item{3}{ for an empty parameter page,}
#'      \item{2}{ when Wikipedia does not have an article with this exact name,}
#'      \item{1}{ for ambiguous page, direct or redirect,} 
#'      \item{0}{ for valid an unambiguous page, direct or redirect. }
#'      }
#'    }
#'    \item{\code{warnMessage}}{ is a vector of warning messages.}
#'    }
#' }
#'}
#' 
#' @details 
#' This function uses the API query syntax: "prop=revisions". For more details, see \url{https://www.mediawiki.org/wiki/API:Backlinks} 
#' If the page is ambiguous, the contributions correspond to the modification on this ambiguous page, not on the possible linked pages.
#' If the user is anonymous, the user in the output corresponds to the IP address, and the userid is zero.
#' 
#' 
#' @author Avner Bar-Hen, Louise Baschet, Francois-Xavier Jollois, Jeremie Riou
#'  
#' @seealso print.contribsClass userInfo
#' @importFrom XML xmlToList xmlTreeParse htmlParse 
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @examples 
#' \dontrun{
#' ## numeric page identifier as parameter
#' contribs(domain = "fr", page = 108907)
#' 
#' ## title page as parameter
#' contribs(domain ="en", page = "Wikipedia")
#' }

#' @export 
#' 
#############################################################################

contribs <- function (page=NULL,domain="en", rvprop = "user|userid|timestamp"){
  
 
  # initialize output 
  out <- NULL
  out$call <- match.call()
  if (is.null(rvprop)) { stop("rvprop argument is required.")}
  
  if(!is.character(rvprop)){stop("rvprop argument must be a character string")}
  
  props <- unlist(strsplit(rvprop, split ="|", fixed = TRUE))
  
  if (! all(props %in% c("ids", "flags", "timestamp", "user", "userid", "size", "sha1", "contentmodel", "comment",
                       "parsedcomment", "content", "tags", "flagged"))){stop("Argument rvprop is not valid")}

  
  # first, test validity of the page 
  test <- testWikiPage(domain = domain, page = page)
  
  # if the domain is valid and the page parameter, and page is not empty :
  if(!(test$test %in% c(2,3,4))) 
  {
    pagebis <-page    
    # if page is a vector, a matrix or a list, take only the first element
    if (test$takeOnlyFirst) { pagebis <- unlist(page)[1] } 
    # if the page is redirected, go to the redirected page 
    if(!is.null(test$redirPage)) { pagebis <- test$redirPage }

    
    if (is.character(pagebis) == TRUE)
    {
      # manage encoding and spaces
      pagebis <- gsub(" ",replacement ="_",x = pagebis)
      pagebis <- URLencode(iconv(pagebis,to="UTF-8"))
      recherche = paste0("titles=", pagebis)
    } else {
      recherche = paste0("pageids=", pagebis)
    }
    url.rev  <- paste("http://",domain,".wikipedia.org/w/api.php?action=query&",recherche,"&prop=revisions&rvlimit=max&format=xml&rvprop=",rvprop, sep="")
    get.rev = GET(url.rev)

  
  # XML informations download for the specific URL
  xml.rev <- xmlToList(xmlTreeParse(get.rev, useInternalNodes = TRUE) )
  list.rev <- xml.rev$query$pages$page$revisions
  xml.revbis <- xml.rev  
  
  # Management of the rvlimit argument (maximum item per page)
  while (!is.null(xml.revbis$"continue")) { 
    continue <-  xml.revbis$"continue"[1] 
    get.revbis <-  GET(paste(url.rev, "&rvcontinue=", continue, sep = "")) # create URL for the selected pageid
    xml.revbis <- NULL
    xml.revbis <- xmlToList(xmlTreeParse(get.revbis,useInternalNodes=TRUE) )
    list.rev <- c(list.rev ,xml.revbis$query$pages$page$revisions)
  }
  assign("global_list.rev", list.rev, envir = .GlobalEnv)
  assign("global_props", props, envir = .GlobalEnv)

  # Information selection
  # out$contribs <- matrix(nrow = length(list.rev), ncol = length(props))
  # colnames(out$contribs) <- props
  # out$contribs = as.data.frame(out$contribs)
  # for (j in 1:length(props)) {
  #   for (i in 1:length(list.rev)) {
  #     v = list.rev[i]$rev[name = props[j]]
  #     if (is.list(v)) { v = toString(unlist(v)) }
  #     out$contribs[i,j] <- v
  #   }  
  # }
  out$contribs = as.data.frame(t(sapply(list.rev, function (r) {
    res = list()
    attrs = r
    if (any("tags" == props)) {
      res$tags = toString(unlist(r$tags))
      attrs = r$.attrs
    }
    if (any("anon" == names(attrs))) {
      attrs = attrs[names(attrs) != "anon"]
    }
    if (any("flags" == props)) {
      res$flags = names(r$.attrs[which(r$.attrs == "")])
      attrs = r$.attrs[which(r$.attrs != "")]
    }
    for (p in names(attrs)) {
      res[[p]] = attrs[p]
    }
    return(res)
  })), row.names = FALSE)
  
  
  # Management of the outputs
  
  out$page <- c(iconv(xml.rev$query$pages$page$.attrs[3],"UTF-8","UTF-8"),xml.rev$query$pages$page$.attrs[1], domain)

  #rownames(out$contribs) <- NULL
  #colnames(out$contribs) <- unlist(strsplit(rvprop, split ="|", fixed = TRUE))
  #out$contribs <- as.data.frame(out$contribs)
  }
  
  out$testWikiPage <- test
  if(!is.null(test$warnMessage)){for( i in 1:length(test$warnMessage) ) {warning(test$warnMessage[i]) } }
  
  class(out) <- c("contribsClass")
  
  return(out)
  
}

