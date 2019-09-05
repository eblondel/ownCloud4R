# test_manager.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ownCloud manager
#=======================
require(ownCloud4R, quietly = TRUE)
require(testthat)

context("manager")

test_that("ownCloud manager - version",{
  version <- owncloud$getVersion()
  expect_equal(names(version), c("major", "minor", "micro", "string", "edition"))
})

test_that("owncloud manager - capabilities",{
  caps <- owncloud$getCapabilities()
  expect_equal(names(caps), c("core", "checksums", "files", "dav", "files_sharing", "notifications"))
})