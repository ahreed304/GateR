#' mkdir
#'
#' @import glue
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' mkdir('path/to/dir')
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

#' mkdirs
#'
#' @import glue
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' mkdirs('path/to/dir')
mkdirs <- function(path) {
  dir_split_base <- path |> strsplit('/') |> unlist()
  mkdir_recursive <- function(dir_split) {
    current_folder <- dir_split |> paste(collapse = '/') |> paste0('/')
    if(!dir.exists(current_folder)) {
      'Creating new directory {current_folder}' |> glue() |> print()
      dir.create(current_folder)
    }
    if (length(dir_split) < length(dir_split_base)) {
      mkdir_recursive(dir_split_base[1:(length(dir_split) + 1)])
    }
  }
  mkdir_recursive(dir_split_base[1])
  path
}
#' %!in%
#'
#' @param x Item to compare
#' @param y Item to compare
#'
#' @return
#' @export
#'
#' @examples
#' if(1 %!in! c(2, 3, 4)) {do_something()}
'%!in%' <- function(x,y){!('%in%'(x,y))}
#
# get_datetime_string <- function() {
#   now(tzone = "US/Central") %>%
#     as.character() %>%
#     gsub(" ", "_", .) %>%
#     gsub(":", ".", .)
# }
#
# save_object <- function(obj, dir, prefix = "") {
#   filename <- '{dir}/{prefix}_{get_datetime_string()}.rds' |> glue()
#   obj |> saveRDS(filename)
# }


# merge_dfs <- function(files, ids) {
#   do.call(
#     rbind.data.frame,
#     c(Map(function(file, id) transform(fread(file), id = id), files, ids),
#       make.row.names=FALSE
#     )
#   )
# }


#' Merge CSVs
#'
#' @param files List of tsv/csv files to be read as dataframes and combined.
#' @param ids List of ids in same length/order as files. If not provided, defaults to files param value.
#' @importFrom data.table fread
#' @return
#' @export
#'
#' @examples
#' path/to/files |> list.files() |> merge_csvs()
merge_csvs <- function(files, ids = files) {
  if (length(files) != length(ids)) {stop("'files' and 'ids' params are different sizes")}
  files |> lapply(fread) |> merge_dfs(ids)
}


#' Merge DFs
#'
#' @param dfs List of dataframes to be combined.
#' @param ids List of ids in same length/order as files. If not provided, defaults to names(dfs).
#' @importFrom dplyr bind_rows
#' @return
#' @export
#'
#' @examples
#' path/to/files |> list.files() |> merge_dfs()
merge_dfs <- function(dfs, ids = names(dfs)) {
  if (length(dfs) != length(ids)) {stop("'dfs' and 'ids' params are different sizes")}
  seq_along(dfs) |> lapply(\(i){
    dfs[[i]]$id <- ids[[i]]
    dfs[[i]]
  }) |> bind_rows()
}


#' Merge PDFs
#'
#' @import qpdf
#' @param dir Directory containing PDFs to combine
#'
#' @return
#' @export
#'
#' @examples
#' merge_pdfs('path/to/folder/containing/PDFs')
merge_pdfs <- function(dir) {
  pdf_combine(
    input = dir |> list.files(full.names = TRUE) |> as.character(),
    output = "{output_root}/!plots_combined.pdf" |> glue()
  )
}


#' Cowtime
#'
#' @import cowsay
#' @importFrom lubridate now
#' @param expr Expression to be timed.
#' @param name (Optional) Name of expression to be printed in console
#'
#' @return
#' @export
#'
#' @examples
#' cowtime(name = 'do thing', { do_thing() })
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

# distraction <- function() {
#   say("fortune","random")
# }



#' to_clipboard
#'
#' @importFrom clipr write_clip
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' to_clipboard('foo')
to_clipboard <- function(data) { clipr::write_clip(data) }


# generate_color_palette <- function(n) {
#   n |> iwanthue(0, 360, 36, 180, 13, 73)
# }
