#' ownCloudManager
#' @docType class
#' @export
#' @keywords owncloud manager
#' @return Object of \code{\link{R6Class}} for modelling an ownCloudManager
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'  \item{\code{new(url, user, pwd, logger)}}{
#'    This method is used to instantiate an ownCloudManager. The user/pwd are
#'    mandatory in order to connect to 'ownCloud'. The logger can be either
#'    NULL, "INFO" (with minimum logs), or "DEBUG" (for complete curl 
#'    http calls logs)
#'  }
#'  \item{\code{connect()}}{
#'    A method to connect to 'ownCloud' and set version/capabilities
#'  }
#'  \item{\code{getVersion}}{
#'    Get the 'ownCloud' server version
#'  }
#'  \item{\code{getCapabilities}}{
#'    Get the 'ownCloud' server capabilities
#'  }
#'}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ownCloudManager <-  R6Class("ownCloudManager",
  inherit = ownCloud4RLogger,
  private = list(
    url = NULL,
    user = NULL,
    pwd = NULL,
    version = NULL,
    capabilities = NULL
  ),
  public = list(
    
    initialize = function(url, user, pwd, logger = NULL){
      super$initialize(logger = logger)
      private$url = url
      private$user <- user
      private$pwd <- user
      self$connect()
    },
    
    #connect
    connect = function(){
      caps_req <- ownCloudRequest$new(
        type = "GET", private$url, "/ocs/v1.php/cloud/capabilities?format=json",
        private$user, private$pwd, logger = self$loggerType
      )
      caps_req$execute()
      caps_resp <- caps_req$getResponse()
      if(caps_resp$ocs$meta$status == "failure"){
        errMsg <- sprintf("Could not connect to ownCloud '%s': %s", private$url, caps_resp$ocs$meta$message)
        self$ERROR(errMsg)
        stop(errMsg)
      }
      if(caps_resp$ocs$meta$status == "ok"){
        self$INFO(sprintf("Successful connection to ownCloud '%s'!", private$url))
        private$version = caps_resp$ocs$data$version
        private$capabilities = caps_resp$ocs$data$capabilities
      }
    },
    
    #getVersion
    getVersion = function(){
      return(private$version)
    },
    
    #getCapabilities
    getCapabilities = function(){
      return(private$capabilities)
    }
  )
)
