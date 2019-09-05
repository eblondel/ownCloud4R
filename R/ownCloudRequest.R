#' ownCloudRequest
#'
#' @docType class
#' @export
#' @keywords owncloud request
#' @return Object of \code{\link{R6Class}} for modelling a generic 'ownCloud' web-service request
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(type, url, request, user, pwd, logger)}}{
#'    This method is used to instantiate a object for doing an 'ownCloud' web-service request
#'  }
#'  \item{\code{getRequest()}}{
#'    Get the request payload
#'  }
#'  \item{\code{getRequestHeaders()}}{
#'    Get the request headers
#'  }
#'  \item{\code{getStatus()}}{
#'    Get the request status code
#'  }
#'  \item{\code{getResponse()}}{
#'    Get the request response
#'  }
#'  \item{\code{getException()}}{
#'    Get the exception (in case of request failure)
#'  }
#'  \item{\code{getResult()}}{
#'    Get the result \code{TRUE} if the request is successful, \code{FALSE} otherwise
#'  }
#' }
#' 
#' @note Abstract class used internally by \pkg{ownCloud4R}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ownCloudRequest <- R6Class("ownCloudRequest",
  inherit = ownCloud4RLogger,
  #private methods
  private = list(
    url = NA,
    type = NA,
    request = NA,
	  requestHeaders = NA,
    namedParams = list(),
	  status = NA,
    response = NA,
    exception = NA,
    result = NA,
    
    user = NULL,
    pwd = NULL,
    token = NULL,
    auth_scheme = "Basic",

    #GET
    #---------------------------------------------------------------
    GET = function(url, request){
      req <- paste(url, request, sep = "/")
      self$INFO(sprintf("Fetching %s", req))
      
      #headers
      headers <- c()
      if(!is.null(private$token)){
        headers <- c(headers, "Authorization" = paste(private$auth_scheme, private$token))
      }
      
      r <- NULL
      if(self$verbose.debug){
        r <- with_verbose(GET(req, add_headers(headers)))
      }else{
        r <- GET(req, add_headers(headers))
      }
      responseContent <- content(r, type = "application/json", encoding = "UTF-8")
      response <- list(request = request, requestHeaders = headers(r),
                       status = status_code(r), response = responseContent)
      return(response)
    }
  ),
  
  #public methods
  public = list(
    #initialize
    initialize = function(type, url, request,
                          user = NULL, pwd = NULL, 
                          logger = NULL, ...) {
      super$initialize(logger = logger)
      private$type = type
      private$url = url
      private$request = request
      
      #authentication schemes
      if(!is.null(user) && !is.null(pwd)){
        #Basic authentication (user/pwd) scheme
        private$auth_scheme = "Basic"
        private$user = user
        private$pwd = pwd
        private$token = openssl::base64_encode(charToRaw(paste(user, pwd, sep=":")))
      }
    },
    
    #execute
    execute = function(){
      
      req <- switch(private$type,
        "GET" = private$GET(private$url, private$request)
      )
      
      private$request <- req$request
      private$requestHeaders <- req$requestHeaders
      private$status <- req$status
      private$response <- req$response
      
      if(private$type == "GET"){
        if(private$status != 200){
          private$exception <- sprintf("Error while executing request '%s'", req$request)
          self$ERROR(private$exception)
        }
      }
    },
    
    #getRequest
    getRequest = function(){
      return(private$request)
    },
    
    #getRequestHeaders
    getRequestHeaders = function(){
      return(private$requestHeaders)
    },
    
    #getStatus
    getStatus = function(){
      return(private$status)
    },
    
    #getResponse
    getResponse = function(){
      return(private$response)
    },
    
    #getException
    getException = function(){
      return(private$exception)
    },
    
    #getResult
    getResult = function(){
      return(private$result)
    },
    
    #setResult
    setResult = function(result){
      private$result = result
    }
    
  )
)