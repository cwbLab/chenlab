#' Seurat object version conversion
#'
#' @param seurat Seurat object
#' @param v Version of the assay output. Optional V5 and V3, default is 'V3'.
#'
#' @export
wb.seurat_5_3 <-function( seurat, v = 'V3'  ){
  new_seurat <- scCustomize::Convert_Assay( seurat_object = seurat , convert_to = v  )
  return( new_seurat )
}
