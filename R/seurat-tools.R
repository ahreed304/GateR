#' Merge Seurat Objects
#'
#' @param seurat_object_list
#'
#' @return
#' @export
#'
#' @examples
#' merge_seurat_objects(seurat_object_list)
merge_seurat_objects <- function(seurat_object_list) {
  merge(seurat_object_list[[1]], seurat_object_list[2:length(seurat_object_list)])
}

#' Basic Umap
#'
#' @importFrom Seurat DimPlot
#' @param seurat seurat object
#' @param colors
#'
#' @return
#' @export
#'
#' @examples
#' basic_umap(seurat, colors)
basic_umap <- function(seurat, colors = NULL) {
  p <- DimPlot(
    seurat,
    raster = FALSE,
    shuffle = TRUE,
    reduction = "umap",
    pt.size = 0.01,
    cols = colors,
  )
  p <- p + guides(color = guide_legend(override.aes = list(size = 4), ncol = 1))
  p
}
