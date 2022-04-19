library(data.table)
library(tidyverse)

#' Read DEGs
#'
#' @param file csv file containing degs
#'
#' @return
#' @export
#'
#' @examples
#' read_degs('path/to/file')
read_degs <- function(file) {
  degs <- fread(file)
  degs <- degs |> setnames("V1", "gene")
  degs
}

#' Filter DEGs
#'
#' @param degs Data table containing DEGs.  Expecting format like the output of read_degs()
#' @param bh_threshold maximum BH corrected p value
#' @param logfc_threshold Minimum log fold change value
#' @param excluded_patterns List of regex patterns to exlude genes
#'
#' @return
#' @export
#'
#' @examples
#''path/to/file' |> read_degs() |> filter_degs(logfc_threshold = 0.25, excluded_patterns = c("^RP", "^XIST$"))
filter_degs <- function(degs, bh_threshold = 0.01, logfc_threshold = 0.585, excluded_patterns = NULL) {
  # TODO: Find out why the following 3 lines work only when imported locally.
  # When imported as a library, data table seems to fail to work properly
  # Temporary fix is to use dataframe syntax here
  # degs_filtered <- degs[BH <= bh_threshold,]
  # degs_filtered <- degs_filtered[abs(avg_log2FC) >= logfc_threshold]
  # if(!is.null(excluded_patterns)){ degs_filtered <- degs_filtered[!excluded_patterns |> paste(collapse = '|') |> grep(gene)] }
  degs_filtered <- degs[degs$BH <= bh_threshold,]
  degs_filtered <- degs_filtered[abs(degs$avg_log2FC) >= logfc_threshold,]
  if(!is.null(excluded_patterns)){ degs_filtered <- degs_filtered[!excluded_patterns |> paste(collapse = '|') |> grep(degs_filtered$gene),] }
  degs_filtered
}

#' Parse DEG Filename
#'
#' @param deg_file csv file containing DEGs. Either full path or filename is acceptable
#'
#' @return
#' @export
#'
#' @examples
#' parse_deg_filename("path/to/file.csv")
parse_deg_filename <- function(deg_file) {
  filename <- deg_file |> strsplit('/') |> unlist() %>% .[[length(.)]]
  filename <- substr(filename, 1, nchar(deg_file) - 4)
  split_comparisons <- filename |> strsplit('_vs._') |> unlist()
  result <- split_comparisons |> lapply(\(comparison) {deg_file = "B_intermediate__als_slow_vs._B_intermediate__healthy_control.csv"
  split_again <- comparison |> strsplit('__') |> unlist()
  if (length(split_again) == 1) {
    list(cluster = 'all', group = split_again)
  } else {
    c(split_again[[1]], split_again[[2]])
  }
  }) |> unlist() |> c()
  names(result) <- c('cluster1', 'group1', 'cluster2', 'group2')
  result
}
