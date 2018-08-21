################################################################################
#' Token renewal method
#' 
#' @description This function renews a token
#' @slot OpencgaR an object OpencgaR generated using initOpencgaR and/or opencgaLogin 
#' where the connection and session details are stored
#' @slot host the host
#' @slot version the version
#' @slot params list containing additional query or body params
#' @seealso \url{https://github.com/opencb/opencga/wiki} and the RESTful API documentation 
#' \url{http://bioinfo.hpc.cam.ac.uk/opencga/webservices/}
#' @export

setMethod("renewToken", "OpencgaR", function(OpencgaR, host, version, params=NULL, ...) {
  urlNewToken <- paste0(host, version, "users/", OpencgaR@user, "/", "login", "?sid=", OpencgaR@sessionId)
  resp <- httr::POST(urlNewToken, add_headers(c("Content-Type"="application/json",
                                                "Accept"="application/json",
                                                "Authorisation"="Bearer")), body="{}")
  content <- httr::content(resp, as="text", encoding = "utf-8")
  if (length(fromJSON(content)$response$result[[1]]$token > 0)){
    OpencgaR@sessionId <- fromJSON(content)$response$result[[1]]$token
    loginInfo <- unlist(strsplit(x=OpencgaR@sessionId, split="\\."))[2]
    loginInfojson <- jsonlite::fromJSON(rawToChar(base64enc::base64decode(what=loginInfo)))
    expirationTime <- as.POSIXct(loginInfojson$exp, origin="1970-01-01")
    OpencgaR@expirationTime <- as.character(expirationTime)
    print("Your session has been renewed!")
  }else{
    warning(paste0("WARNING: Your token could not be renewed, your session will expire in ", 
                   round(x = timeLeft, digits = 2), " minutes"))
  }
})