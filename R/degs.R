#' Read DEGs folder
#'
#' @import glue
#' @param degs_path path to folder or file containing DEGs
#'
#' @return
#' @export
#'
#' @examples
#' read_degs('path/to/folder')
read_degs <- function(degs_path) {
  if (dir.exists(degs_path)) { read_degs_folder(degs_path) }
  else if (file.exists(degs_path)) { read_degs_file(degs_path) }
  else { "No file or folder exists at {degs_path}" |> glue() |> stop() }
}

#' Read DEGs folder
#'
#' @import yaml
#' @import glue
#' @param degs_folder path to folder containing degs.csv and metadata.yml
#'
#' @return
#' @export
#'
#' @examples
#' read_degs_folder('path/to/folder')
read_degs_folder <- function(degs_folder) {
  degs <- list()
  degs$degs <- "{degs_folder}/degs.csv" |> glue() |> read_degs_file()
  degs$metadata <- "{degs_folder}/metadata.yml" |> glue() |> yaml::read_yaml()
  degs
}


#' Read DEGs file
#'
#' @import data.table
#' @param file csv file containing degs
#'
#' @return
#' @export
#'
#' @examples
#' read_degs_file('path/to/file.csv')
read_degs_file <- function(file) {
  degs <- fread(file)
  degs <- degs |> setnames("V1", "gene")
  degs
}

#' Filter DEGs
#'
#' @import data.table
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
filter_degs <- function(degs, bh_threshold = Inf, logfc_threshold = 0, excluded_patterns = NULL) {
  degs_filtered <- degs[BH <= bh_threshold,]
  degs_filtered <- degs_filtered[abs(avg_log2FC) >= logfc_threshold]
  if(!is.null(excluded_patterns)){ degs_filtered <- degs_filtered[!excluded_patterns |> paste(collapse = '|') |> grep(gene)] }
  degs_filtered
}

#' Parse DEG Filename
#'
#' @import magrittr
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
