#' Server URL preparation.
#'
#' Prepares the server URL by checking for the presence of a protocol
#' prefix, and prefixing \code{http://} if there is none. Also, returns
#' the URL with a trailing \code{/} if that was not present either.
#'
#' @param server A server name.
#' @return A clean URL with protocol prefix and trailing slash.
#' @examples
#' prepare.server.URL("db.phonologist.org")
#' prepare.server.URL("https://myserver.com/")
#'
#' @export
prepare.server.URL <- function(server){
    url <- ifelse(grepl("://", server), server, paste0("http://",server))
    url <- ifelse(substr(url, nchar(url), nchar(url)) == "/", url, paste0(url, "/"))
    url
}

#' Create an URL for a request to the Experigen API.
#'
#' @param server The server name (no need to be clean)
#' @param request The request verb to the server
#' @param params A list of name=value pairs that will be passed
#' to the server
#' @return A URL that can be queried.
#'
#' @examples
#' create.API.request.URL("db.phonologist.org", "users", list(sourceurl="x.y", experimentName="z"))
#' @export
create.API.request.URL <- function(server, request, params = list()){
   url <- paste0(prepare.server.URL(server), request)
    if(length(params) > 0){
        parampairs <- c()
        for(n in names(params)){
            parampairs <- c(parampairs, paste(n, params[n], sep = "="))
        }
        url <- paste(url, paste(parampairs, collapse = "&"), sep = "?")
    }
    url
}

#' Run a request to the API
#'
#' @param server The server name (no need to be clean)
#' @param request The request verb to the server
#' @param params A list of name=value pairs that will be passed
#' to the server
#' 
#' @export
API.request <- function(server, request, params = list()){
    url <- create.API.request.URL(server, request, params)
}


#' Returns the version of an Experigen server
#'
#' @param server The server name
#' @return A string with the Experigen server version
#' @examples
#' server.version("db.phonologist.org")
#' @export
server.version <- function(server){
    url <- prepare.server.URL(server)
    url <- paste0(url,"/version")
    if(RCurl::url.exists(url)){
        RCurl::getURL(url)
    }
    else {
        "1.0.0"
    }
}
