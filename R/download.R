#' Makes a download request from the server. There will be easier
#' functions provided too.
#'
#' @param sourceURL The source URL for the experiment
#' @param experimentName The experiment name as set in \code{settings.js}
#' @param destination The file to download. By default, all uploaded results are
#' saved in \code{default.csv}, which is the default file to download. 
#' @param auth Whether authentication is needed
#'
#' @return The downloaded data set
#' 
#' @export
#' 
download.experiment <- function(sourceURL, experimentName,
                                destination = "default.csv",
                                auth = FALSE){
    request <- ifelse(versionMain() < 2, "makecsv.cgi", "makecsv")
    if(auth && versionMain() < 2){
        warning("Experiment registration is not supported by the server, requesting without authentication.")
        auth <- FALSE
    }
    API.request(request = request,
                params = list(
                    sourceurl = sourceURL,
                    experimentName = experimentName,
                    file = destination
                ),
                auth = auth)
}


