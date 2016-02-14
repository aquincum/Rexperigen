#' Sets the Experigen server address for further operations.
#' By default, it checks the existence of the server and
#' sets the \code{Rexperigen.server.version} option based on
#' the response of the server. If \code{check = FALSE}, set
#' the version by yourself!
#'
#' @param server The server's URL
#' @param check Whether to check the server's existence
#'
#' @examples
#' setExperigenServer("db.phonologist.org")
#' setExperigenServer("localhost:3000", FALSE)
#' 
#' @export
setExperigenServer <- function(server, check = TRUE){
    options(Rexperigen.server = server)
    if(check){
        version <- server.version(server)
        if(version == NO_SERVER_ERROR){
            options(list(Rexperigen.server = "",
                         Rexperigen.server.version = ""))
            stop(paste("No server found at", server))
        }
        else{
            options(Rexperigen.server.version = version)
        }
    }
    invisible()
}


#' Sets up the experimenter credentials for the further requests.
#' It can also check whether the authentication is successful.
#'
#' @param experimenter The experimenter username
#' @param password The password for the experimenter
#' @param check Whether to check if the experimenter is registered.
#' @param quiet If \code{TRUE}, the function will not print out the
#' result of the existence check
#'
#' @return Whether the existence check is successful. If \code{check = FALSE},
#' it will return \code{TRUE} by default
#'
#' @examples
#' \dontrun{
#' setExperigenCredentials("joesmith", "1234")
#' }
#' setExperigenCredentials("janemiller", "passdrow", check = FALSE)
#' 
#' @export
setExperigenCredentials <- function(experimenter, password, check = TRUE, quiet = FALSE){
    options(list(
        Rexperigen.experimenter = experimenter,
        Rexperigen.password = password
    ))
    if(check){
        if(versionMain() < 2){
            stop("Too low version of Experigen server: does not support registration of experiments")
        }
        me <- API.request(request = "auth/me", auth = TRUE)
        if(me == experimenter){
            if(!quiet){
                print("Success!")
            }
            return(TRUE)
        }
        else{
            if(!quiet){
                print(paste("Problem with login:", me))
            }
            return(FALSE)
        }
    }
    else{
        return(TRUE)
    }
}

#' Returns the main version number of the server.
#' @return The main version number as a numeric
versionMain <- function(){
    v <- getOption("Rexperigen.server.version")
    if (v == "") v <- "0.0.0"
    as.numeric(strsplit(v, "\\.")[[1]][1])
}
