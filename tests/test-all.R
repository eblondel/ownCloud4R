library(testthat)
library(ownCloud4R)

#test environment
owncloud_url <- "http://localhost:8080"
owncloud_user <- "admin"
owncloud_pwd <- "admin"

owncloud <- ownCloudManager$new(
  url = "http://localhost:8080", 
  user = owncloud_user, 
  pwd = owncloud_pwd, 
  logger = "DEBUG"
)

if(is(owncloud, "ownCloudManager")){
  cat(sprintf("ownCloud '%s' configured with token. Running integration tests...\n", owncloud_url))
  test_check("ownCloud4R")
}else{
  cat("ownCloud '%s' not configured. Skipping integration tests...\n")
}