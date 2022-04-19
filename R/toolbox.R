library(lubridate)
library(cowsay)
library(fortunes)
library(data.table)
library(tidyverse)

mkdir <- function(path) {
  if (file.exists(path)) {
    # "Directory/file exists at {path}.  Cannot create directory." |> glue() |> warning()
  }
  else{
    'Creating new directory {path}' |> glue() |> print()
    dir.create(path)
  }
  path
}

'%!in%' <- function(x,y)!('%in%'(x,y))

get_datetime_string <- function() {
  now(tzone = "US/Central") %>%
    as.character() %>%
    gsub(" ", "_", .) %>%
    gsub(":", ".", .)
}

save_object <- function(obj, dir, prefix = "") {
  filename <- '{dir}/{prefix}_{get_datetime_string()}.rds' |> glue()
  obj |> saveRDS(filename)
}


# merge_dfs <- function(files, ids) {
#   do.call(
#     rbind.data.frame,
#     c(Map(function(file, id) transform(fread(file), id = id), files, ids),
#       make.row.names=FALSE
#     )
#   )
# }


#' Merge DFs
#'
#' @param files List of tsv/csv files containing dataframes
#' @param ids List of ids in same length/order as files
#'
#' @return
#' @export
#'
#' @examples
#' merge_dfs(files, ids)
merge_dfs <- function(files, ids) {
  if (length(files) != length(ids)) {stop("'files' and 'ids' params are different sizes")}
  seq_along(files) |> lapply(\(i){
    df <- files[[i]] |> fread()
    df$id <- ids[[i]]
    df
  }) |> bind_rows()
}

merge_pdfs <- function(dir) {
  qpdf::pdf_combine(
    input = dir |> list.files(full.names = TRUE) |> as.character(),
    output = "{output_root}/!plots_combined.pdf" |> glue()
  )
}

# samples <- dirs$cellranger$tcr |> list.files()
# files <- "{dirs$cellranger$tcr}/{samples}/outs/filtered_contig_annotations.csv" |> glue()
# foo <- merge_dfs(files, samples)
cowtime <- function(expr, name = NA) {
  s_time <-  now(tzone = "US/Central")
  msg <- s_time |> as.character()
  # msg <- s_time |> as.character()
  if (!is.na(name)) {
    msg <- glue("\nstarting \"{name}\"\n{msg}")
  }
  say(msg, "chicken")
  expr
  e_time <- now(tzone = "US/Central")
  delta_time <- (e_time - s_time) %>% as.duration %>% as.character
  msg <- glue("Execution time: \n{delta_time}")
  if (!is.na(name)) {
    msg <- glue("Execution time for \"{name}\": \n{delta_time}")
  }
  say(msg, "cow")
}

distraction <- function() {
  say("fortune","random")
}
