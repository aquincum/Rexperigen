


download.experiment <- function(sourceURL, experimentName,
                                destination = "default.csv",
                                server = "db.phonologist.org",
                                authenticated = FALSE,
                                username = "",
                                password = ""
                                ){
    url <- prepare.server.URL(server)
    xp <- RCurl::getURL(url)
}


