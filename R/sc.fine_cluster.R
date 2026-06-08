#' Fine cluster for seurat object
#'
#' @description
#' Reassigns cluster labels in a Seurat object based on inter-cluster distances, such that clusters with higher transcriptomic similarity (i.e., shorter distances) are assigned consecutive numeric labels.
#'
#' This improves interpretability and facilitates downstream cell type annotation by ensuring that related clusters are grouped together in label space.
#'
#' @param object A processed Seurat object with computed embeddings (e.g., PCA/UMAP) and defined cluster identities accessible via the `Seurat::Idents` function.
#' @param reduction Which dimensionality reduction to use.
#' @param dims Dimensions of reduction to use as input. Default to using all dimensions. Custom dimension values can be provided, such as 1:20.
#' @param downsample Downsample clusters with a cell count greater than(>) this threshold to the specified value in order to accelerate computation.
#' @param seed Random seed used for downsampling.
#' @param plot Plot figures to compare clusters before and after conversion.
#'
#' @returns
#' A list object:
#'
#' (1) final_cluster: a vector of converted cluster labels for each cell, corresponding one-to-one to the column names (cell barcodes) of the Seurat object.
#'
#' (2) conversion_result: mapping between original clusters and converted clusters.
#'
#' @export
#'
#' @examples
#'
#' seurat.obj
#' p1 <- Seurat::DimPlot(  seurat.obj , label = T,repel = T , reduction = "umap" )
#'
#' ### rename
#' fine.res <- w.sc.fine_cluster( seurat.obj )
#'
#' #1
#' seurat.obj$new.cluster <- fine.res[['final_cluster']]
#' Idents( seurat.obj ) <- 'new.cluster'
#'
#' #2
#' new_name <- fine.res[['conversion_result']]
#' new.cluster <- new_name$new_cluster
#' names(new.cluster) <- new_name$raw_cluster
#' seurat.obj <- RenameIdents( seurat.obj, new.cluster )
#'
#' p2 <- Seurat::DimPlot(  seurat.obj , label = T,repel = T , reduction = "umap"  )
#'
#' ### plot
#' ( p1 | p2 ) + patchwork::plot_annotation( tag_levels = list(c('raw', 'fine'), '1') )
#'
w.sc.fine_cluster <- function( object , reduction = "umap" , dims = NULL , downsample = 200 , seed = 100 , plot = T ){
  w.packageCheck( "Seurat" , method = "I"  )

  #
  library(Seurat)
  library(dplyr)
  library(patchwork)
  library(data.table)
  #
  seurat.obj <- object
  #
  emb <- Embeddings(seurat.obj, reduction = reduction )
  emb <- emb + abs( min(emb) )
  if( !is.null( dims ) ){  emb <- emb[ ,  dims ]  }

  ##########get distance
  df <- as.data.frame( emb )
  group <- Idents( seurat.obj ) %>% as.character()
  group_unique <- unique( group )

  #downsample
  set.seed(seed)
  group_mat <- lapply( group_unique , function(x , max_n = downsample  ){
    sd <- df[ group == x , ]
    if( nrow(sd)  > max_n ){
      sd <- sd[  sample( 1:nrow(sd) , max_n , replace = F ) ,]
    }
    return( as.matrix(sd))
  } )
  names(group_mat) <- group_unique

  #calc
  calc_mean_dist_fast <- function(A, B) {
    #
    A2 <- rowSums(A^2)
    B2 <- rowSums(B^2)
    #
    D2 <- outer(A2, B2, "+") - 2 * tcrossprod(A, B)
    #
    D2[D2 < 0] <- 0
    #
    return( mean(sqrt(D2)) )
  }

  #
  group_names <- names(group_mat)
  k <- length(group_names)

  res <- matrix(NA, k, k, dimnames = list(group_names, group_names))
  for (i in 1:k) {
    for (j in i:k) {
      d <- calc_mean_dist_fast(group_mat[[i]], group_mat[[j]])
      res[i, j] <- d
      res[j, i] <- d
    }
  }
  #
  dist_mat <- data.frame( res ,check.names = F)

  ##########get number
  cell_counts <- table( Idents( seurat.obj ) ) %>% as.data.frame()
  cell_counts$Var1 <- as.character( cell_counts$Var1 )
  cell_counts <- cell_counts[  order( cell_counts$Freq , decreasing = T ) , ]

  trans_res <- data.frame( raw = cell_counts$Var1[1] , trans = 1  )
  cluster_number <- 1
  target_cell <- trans_res$raw

  for ( i in  1:nrow( cell_counts ) ){
    target_cell <- trans_res$raw[  cluster_number ]
    #
    dist_mat <- dist_mat[ ! (  rownames(dist_mat) %in% trans_res$raw )   , ]
    neighbors <- rownames( dist_mat )[ which.min( dist_mat[, which( colnames(dist_mat) == target_cell  )  ] ) ]
    neighbors <- neighbors[ !(neighbors %in% trans_res$raw )  ]
    #
    for( x in neighbors ){
      cluster_number <- cluster_number + 1
      trans_res <- rbind( trans_res , data.frame( raw = x , trans = cluster_number  ) )
    }
  }
  #
  trans_res <- trans_res[ !duplicated( trans_res$trans  )  ,  ]
  colnames(trans_res) <- c( "raw_cluster",  "new_cluster"   )

  ##########plot
  raw_cluster <- Idents( seurat.obj  )
  suppressMessages(
    p1 <- Seurat::DimPlot(  seurat.obj , label = T,repel = T , reduction = reduction )
  )

  new.cluster <- trans_res$new_cluster
  names(new.cluster) <- trans_res$raw_cluster
  seurat.obj <- RenameIdents( seurat.obj, new.cluster )
  suppressMessages(
    p2 <- Seurat::DimPlot(  seurat.obj , label = T,repel = T , reduction = reduction  )
  )
  new_cluster <- as.integer( Idents( seurat.obj ) )
  new_cluster <- factor(  new_cluster , levels = min(  new_cluster ) : max( new_cluster )    )

  if( plot ){ print( ( p1 | p2 ) + patchwork::plot_annotation(tag_levels = list(c('raw', 'fine'), '1') ) ) }

  ##########
  return( list(  final_cluster = new_cluster , conversion_result = trans_res  ) )
}

