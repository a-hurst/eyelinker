#' Import ASC Files from EyeLink Eye Trackers
#'
#' Dealing with unprocessed ASC files from EyeLink eye trackers can be a pain.
#' This package aims to make importing and working with these files as fast and
#' easy as possible.
#'
#' For documentation of the structure of the returned data, see the "format" vignette:
#'
#' \code{vignette("format", package = "eyelinker")}
#'
#' For worked examples illustrating the package in action, see the "basics" vignette:
#'
#' \code{vignette("basics", package = "eyelinker")}
#' @docType package
#' @name eyelinker
NULL

#' @importFrom stats median
#' @importFrom utils tail
#' @importFrom stringr str_sub<- str_detect fixed
#' @importFrom tibble as_tibble add_column
#' @importFrom readr read_lines read_tsv read_table
#' @importFrom stringi stri_enc_toascii stri_split_regex stri_split_fixed stri_count_fixed
#' @importFrom intervals which_nearest distance_to_nearest Intervals
NULL
