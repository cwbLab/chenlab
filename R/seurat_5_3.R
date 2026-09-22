#' Seurat object version conversion
#'
#' @param seurat Seurat object
#' @param v Version of the assay output. Optional V5 and V3, default is 'V3'.
#'
#'
#' @examples
#' testsc <- chenlab::chenlab_testsc
#' testsc_v3 <- ww.seurat_5_3( testsc  )
#'
#' @export
#'
#'
ww.seurat_5_3 <-function( seurat, v = 'V3'  ){
  #
  ww.package_install( "Seurat" , method = "I"  )

  #
  new_seurat <- scCustomize::Convert_Assay( seurat_object = seurat , convert_to = v  )
  return( new_seurat )
}
