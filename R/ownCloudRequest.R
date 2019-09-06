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
    auth = NULL,

    #GET
    #---------------------------------------------------------------
    GET = function(url, request, namedParams){
      req <- paste(url, request, sep = "/")
      if(!endsWith(req,"?")) req <- paste0(req, "?")
      namedParams <- namedParams[!sapply(namedParams, is.null)]
      paramNames <- names(namedParams)
      namedParams <- lapply(namedParams, function(namedParam){
        if(is.logical(namedParam)) namedParam <- tolower(as(namedParam, "character"))
        return(namedParam)
      })
      params <- paste(paramNames, namedParams, sep = "=", collapse = "&")
      req <- paste0(req, params)
      self$INFO(sprintf("Fetching %s", req))
      
      if(is.null(private$auth)){
        errMsg <- "An user/paswword authentication request is required"
        self$ERROR(errMsg)
        stop(errMsg)
      }
      
      r <- NULL
      if(self$verbose.debug){
        r <- with_verbose(GET(req, private$auth))
      }else{
        r <- GET(req, private$auth)
      }
      responseContent <- content(r, type = "application/json", encoding = "UTF-8")
      response <- list(request = request, requestHeaders = headers(r),
                       status = status_code(r), response = responseContent)
      return(response)
    },
    
    #PROPFIND
    PROPFIND = function(url, request){
      uri <- paste(url, request, sep = "/")
      h <- new_handle()
      handle_setopt(h, customrequest = "PROPFIND")
      handle_setopt(h, username = private$user)
      handle_setopt(h, password = private$pwd)
      response <- curl_fetch_memory(uri, h)
      xml <- rawToChar(response$content)
      response <- xmlParse(xml, asText = TRUE)
      webdavNS <- c(d = "DAV:")
      base <- paste(paste("/", strsplit(uri, "/")[[1]][-1:-3], sep="", collapse=""), "/", sep="")
      nodes <- getNodeSet(response, "//d:response")
      result <- do.call("rbind", lapply(nodes, function(node){
        out_node <- data.frame(
          name = sub(base, "", URLdecode(xpathSApply(xmlDoc(node), "//d:href", namespaces = webdavNS, xmlValue))),
          resourceType = ifelse(length(xmlChildren(getNodeSet(xmlDoc(node), "//d:propstat/d:prop/d:resourcetype", namespaces = webdavNS)[[1]]))==0,"file","collection"),
          contentType = {
            ct <- xpathSApply(xmlDoc(node), "//d:propstat/d:prop/d:getcontenttype", namespaces = webdavNS, xmlValue)
            if(length(ct)==0) ct <- NA
            ct
          },
          size = {
            s = xpathSApply(xmlDoc(node), "//d:propstat/d:prop/d:getcontentlength", namespaces = webdavNS, xmlValue)
            s = as.numeric(s)
            s <- if(length(s)==0) NA else s/1048576
            s
          },
          quota = {
            q = xpathSApply(xmlDoc(node), "//d:propstat/d:prop/d:quota-used-bytes", namespaces = webdavNS, xmlValue)
            q = as.numeric(q)
            q <- if(length(q)==0) NA else q/1e6
            q
          },
          lastModified = {
            date = xpathSApply(xmlDoc(node), "//d:propstat/d:prop/d:getlastmodified", namespaces = webdavNS, xmlValue)
            date = gsub(" GMT", "", date)
            lctime <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
            date <- strptime(date, "%a, %d %b %Y %H:%M:%S")
            Sys.setlocale("LC_TIME", lctime)
            date
          },
          stringsAsFactors = FALSE
        )
        return(out_node)
      }))
      result <- result[result$name != "", ]
      
      response <- list(request = request, requestHeaders = NA,
                       status = NA, response = result)
      return(response)
    }
  ),
  
  #public methods
  public = list(
    #initialize
    initialize = function(type, url, request,
                          user = NULL, pwd = NULL,
                          namedParams = list(),
                          logger = NULL, ...) {
      super$initialize(logger = logger)
      private$type = type
      private$url = url
      private$request = request
      private$namedParams = namedParams
      private$namedParams$format = "json"
      
      #authentication schemes
      if(!is.null(user) && !is.null(pwd)){
        #Basic authentication (user/pwd) scheme
        private$auth = authenticate(user = user, password = pwd)
      }
    },
    
    #execute
    execute = function(){
      
      req <- switch(private$type,
        "GET" = private$GET(private$url, private$request, private$namedParams),
        "PROPFIND" = private$PROPFIND(private$url, private$request)
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