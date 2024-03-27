#' @title optloadinstall2
#' @description load packages from character vector of package names while 
#' installing missing packages
#' @param x character vector containing package to load
#' @examples
#' packages <- c("purrr", "dplyr", "stringr", "readxl", "archive", "furrr", 
#' "tidyr", "future")
#' optloadinstall2(packages)

optloadinstall2 <- function(x) {
  
  installPack<-function(p){
  if(p %in% utils::installed.packages()) {
    library(p, character.only = TRUE)
  } else {
    install.packages(p)
    library(p, character.only = TRUE)
  }
  }
  
  suppressPackageStartupMessages(
    suppressWarnings(
      invisible(
        lapply(x,installPack)
        )
      )
    )
}