#' ownCloudManager
#' @docType class
#' @export
#' @keywords owncloud manager
#' @return Object of \code{\link{R6Class}} for modelling an ownCloudManager
#' @format \code{\link{R6Class}} object.
#' @section General Methods:
#' \describe{
#'  \item{\code{new(url, user, pwd, logger)}}{
#'    This method is used to instantiate an ownCloudManager. The user/pwd are
#'    mandatory in order to connect to 'ownCloud'. The logger can be either
#'    NULL, "INFO" (with minimum logs), or "DEBUG" (for complete curl 
#'    http calls logs)
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ownCloudManager <-  R6Class("ownCloudManager",
  inherit = ocs4R::ocsManager,
  lock_objects = FALSE,
  public = list(
    initialize = function(url, user, pwd, logger = NULL){
      require(ocs4R)
      super$initialize(url, user, pwd, logger)
    }
    
  )
)
