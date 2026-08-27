
#' Identify cluster-specific outliers
#' @description
#' Detect outlier cells within each cluster based on the low-dimensional embedding matrix, thereby improving the visualization of dimensionality reduction results (e.g., UMAP plots).
#'
#' @param object A processed Seurat object with computed embeddings (e.g., PCA/UMAP).
#' @param group.by A column name in `object@meta.data` that specifies the group assignment of each cluster.
#' @param reduction Which dimensionality reduction to use.
#' @param dims Dimensions of reduction to use as input. Default to using all dimensions. Custom dimension values can be provided, such as 1:20.
#' @param method Outlier detection can be performed using either `lof` or `dbscan`, implemented through the [dbscan::lof] or [dbscan::dbscan] functions, respectively.
#' @param minLOF The threshold for outlier detection when `method = "lof"`. Cells with values above this threshold are considered outliers.
#' @param minPts Same as [dbscan::lof] or [dbscan::dbscan].
#' @param eps Same as [dbscan::dbscan].
#' @param plot Whether to plot the results to evaluate the filtering performance.
#'
#' @returns
#' A list object containing the outlier detection results and the corresponding ggplot2 objects.
#'
#' @export
#'
ww.sc.cluster_clean <- function( object , group.by ,  reduction = "umap" , dims = NULL , method = 'lof' , minLOF = 2 ,  minPts = 20 , eps = 0.8 , plot = T ){
  ww.package_install( "Seurat" , method = "I"  )
  ww.package_install( "dbscan" , method = "I"  )
  #
  ww.package_library( dplyr , patchwork ,  Seurat ,  data.table , ggplot2 , ggpubr  )

  #
  seurat.obj <- object
  seurat.obj$tempg <- as.character(  seurat.obj@meta.data[ , group.by ] )
  Idents( seurat.obj ) <- seurat.obj$tempg

  all_group <- base::unique( seurat.obj$tempg )
  #
  result <- ww.pblapply( all_group , function( cell ){

    ######lof
    s.seurat <- subset( seurat.obj , tempg == cell )
    title = paste0( cell, ' | pre' )
    subtitle = paste0( ncol( s.seurat  ) , ' cells'    )

    p1 <- Seurat::DimPlot( s.seurat ) +
      labs( title = title  , subtitle = subtitle ) +
      theme( legend.position = 'none' ,  plot.title = element_text( hjust = 0.5 ) , plot.subtitle = element_text( hjust = 0.5 )  )

    #
    emb <- Seurat::Embeddings(s.seurat, reduction )
    if( !is.null( dims ) ){ emb <- emb[, dims ] }

    if (  method == 'lof' ){
      lof.score <- dbscan::lof(emb, minPts = minPts )
      decision  <- ifelse( lof.score < minLOF , 'keep' , 'outlier'   )
      #
      outlier.result <- data.table(  group = cell , barcode =  rownames( emb ) , lof.score = lof.score , outlier = decision   )
    }
    if (  method == 'dbscan' ){
      myscore <- dbscan::dbscan( emb , eps = eps ,  minPts = minPts )
      myscore <- myscore$cluster
      decision  <- ifelse( myscore != 0 , 'keep' , 'outlier'   )
      #
      outlier.result <- data.table(  group = cell , barcode =  rownames( emb ) , dbscan.score = myscore , outlier = decision   )
    }

    ######plot
    s.res <- outlier.result
    s.res <- s.res[ match( colnames(s.seurat)  , s.res$barcode  ) ,   ]
    s.seurat$outlier <- s.res$outlier
    title = paste0( cell, ' | post' )

    keep.number <- length( which( s.res$outlier == 'keep' ) )
    subtitle = paste0(  keep.number , sprintf( ' cells (%s)' , sprintf( "%.3f%%" ,  (  keep.number ) / nrow( s.res ) * 100)      )    )
    p2 <- Seurat::DimPlot( subset(s.seurat , outlier == 'keep') ) +
      labs( title = title  , subtitle = subtitle  ) +
      theme( legend.position = 'none' ,  plot.title = element_text( hjust = 0.5 ) , plot.subtitle = element_text( hjust = 0.5 )  )

    #
    p <- ggpubr::ggarrange(  p1 , p2 , nrow = 1 )

    #
    if( plot ){  print( p )  }
    return(  list( result.outlier =  outlier.result ,  ps = p  )  )

  } )
  #
  result.outlier <- base::lapply( result  , function(x) x[[1]] ) %>% rbindlist()
  ps <- base::lapply( result  , function(x) x[[2]] )
  names(ps) <- all_group
  #
  return(  list( result.outlier = result.outlier ,  plots = ps  )   )

}


