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
#'  \item{\code{connect()}}{
#'    A method to connect to 'ownCloud' and set version/capabilities
#'  }
#'  \item{\code{getVersion()}}{
#'    Get the 'ownCloud' server version
#'  }
#'  \item{\code{getCapabilities()}}{
#'    Get the 'ownCloud' server capabilities
#'  }
#'  \item{\code{getWebdavRoot()}}{
#'    Get the 'ownCloud' WebDAV root URL
#'  }
#' }
#'
#' @section WebDAV methods:
#' \describe{
#'  \item{\code{listFiles(relPath)}}{
#'    WebDAV method to list folders/files given a relative path. The relative path is set
#'    to \code{"/"} by default, which corresponds to the root of the 'ownCloud' repository.
#'  }
#'  \item{\code{makeCollection(name, relPath)}}{
#'    WebDAV method to make a collection. By default \code{relPath} is set to \code{"/"} (root).
#'    The \code{name} is the name of the new collection to be created. The function is recursive
#'    in the sense that a \code{name} can be provided as relative path of a collection tree 
#'    (eg \code{newfolder1/newfolder2/newfolder3}), the function will create recursively as 
#'    many collections are handled in the name. 
#'  }
#'  \item{\code{uploadFile(filename, relPath)}}{
#'    WebDAV method to upload a file. By default \code{relPath} is set to \code{"/"} (root).
#'  }
#' }
#' 
#' @section 'ownCloud' Share API methods:
#' \describe{
#'  \item{\code{getShares(path, reshares, shared_with_me, state, subfiles, pretty)}}{
#'    Get list of shares as \code{list}. To return as \code{data.frame}, set 
#'    \code{pretty = TRUE}. The method accepts additional parameters .More info
#'    at 
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
      private$pwd <- pwd
      self$connect()
    },
    
    #connect
    connect = function(){
      caps_req <- ownCloudRequest$new(
        type = "HTTP_GET", private$url, "ocs/v1.php/cloud/capabilities",
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
    },
    
    #getWebdavRoot
    getWebdavRoot = function(){
      return(private$capabilities$core[["webdav-root"]])
    },
    
    #listFiles
    listFiles = function(relPath = "/"){
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      request <- paste0(self$getWebdavRoot(), relPath)
      list_req <- ownCloudRequest$new(
        type = "WEBDAV_PROPFIND", private$url, request,
        private$user, private$pwd, logger = self$loggerType
      )
      list_req$execute()
      list_resp <- list_req$getResponse()
      return(list_resp)
    },
    
    #makeCollection
    makeCollection = function(name, relPath = "/"){
      col_names <- unlist(strsplit(name, "/"))
      if(length(col_names)==1){
        request <- paste0(self$getWebdavRoot(), relPath, name)
        mkcol_req <- ownCloudRequest$new(
          type = "WEBDAV_MKCOL", private$url, request,
          private$user, private$pwd, logger = self$loggerType
        )
        mkcol_req$execute()
        mkcol_resp <- mkcol_req$getResponse()
        return(mkcol_resp)
      }else{
        self$INFO(sprintf("Nested collections detected in '%s'. Splitting name to make nested collections", name))
        for(i in 1:length(col_names)){
          newRelPath <- "/"
          if(i>1) newRelPath <- paste0(newRelPath, paste(col_names[1:(i-1)], collapse="/"), "/")
          self$makeCollection(col_names[i], newRelPath)
        }
      }
    },
    
    #uploadFile
    uploadFile = function(filename, relPath = "/"){
      if(!startsWith(relPath, "/")) relPath <- paste0("/", relPath)
      if(!endsWith(relPath, "/")) relPath <- paste0(relPath, "/")
      request <- paste0(self$getWebdavRoot(), relPath, filename)
      self$INFO(sprintf("WEBDAV - Uploading file '%s' at '%s'", 
                        filename, paste(private$url, request, sep="/")))
      upload_req <- ownCloudRequest$new(
        type = "HTTP_PUT", private$url, request,
        private$user, private$pwd, 
        filename = filename,
        logger = self$loggerType
      )
      upload_req$execute()
      if(upload_req$getStatus()==201){
        self$INFO(sprintf("Successfuly uploaded file '%s' at '%s'",
                          filename, paste(private$url, request, sep="/")))
      }else{
        errMsg <- sprintf("WEBDAV - Error while uploading '%s' at '%s'",
                          filename, paste(private$url, request, sep="/"))
        self$ERROR(errMsg)
        stop(errMsg)
      }
      upload_resp <- upload_req$getResponse()
      return(upload_resp)
    },
    
    #getShares
    getShares = function(path = NULL, reshares = FALSE, shared_with_me = NULL,
                         state = NULL, subfiles = FALSE, 
                         pretty = FALSE){
      
      if(!private$capabilities$files_sharing$api_enabled){
        errMsg <- "The 'getShares' method requires the 'ownCloud' OCS Sharing API to be enabled."
        errMsg <- paste(errMsg, "Please get in touch with your 'ownCloud' administrator")
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      allowedStates <- c("accepted", "all", "declined", "pending", "rejected")
      if(!is.null(state)) if(!(state %in% allowedStates)){
        errMsg <- sprintf("'state' should be one value among [%s]", 
                          paste(allowedStates, collapse=","))
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      request <- "ocs/v1.php/apps/files_sharing/api/v1/shares"
      get_req <- ownCloudRequest$new(
        type = "HTTP_GET", private$url, request,
        private$user, private$pwd, 
        namedParams = list(
          path = path,
          reshares = reshares,
          shared_with_me = shared_with_me,
          state = state,
          subfiles = subfiles
        ),
        logger = self$loggerType
      )
      get_req$execute()
      get_resp <- get_req$getResponse()
      names(get_resp)
      get_out <- get_resp$ocs$data
      if(!is.null(get_out)) if(pretty){
        get_out <- as.data.frame(do.call("rbind",lapply(get_out, unlist)))
      }
      return(get_out)
    }
  )
)
