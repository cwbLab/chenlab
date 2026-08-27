sc.metacell.cluster <- function( object , cluster ,
                            assay = NULL , layer = NULL,
                            method = 'supercell' , min.cluster = 50 , min.cluster.reserved = T, min.target = 50 , max.target = NULL ){
  #
  ww.package_library( dplyr , Seurat ,  data.table , Matrix  )

  #
  object@meta.data[[ cluster ]] <- as.character( object@meta.data[[ cluster ]]   )
  Idents( object ) <- cluster

  #min.cluster
  cell_number <- table( object@meta.data[[ cluster ]] ) %>% sort() %>% as.data.frame()
  cell_number$Var1 <- as.character(  cell_number$Var1  )
  cell_number <- cell_number[ cell_number$Freq > 1 ,  ]
  aba_cells <- cell_number$Var1[ cell_number$Freq < min.cluster  ] %>% as.character()
  cell_number <- cell_number[ ! cell_number$Var1 %in% aba_cells , ]
  if( nrow( cell_number ) < 1  ){  stop( "No clusters are retained for downstream analysis." )  }

  #
  if( length( aba_cells ) != 0  ){  aba_obj <- subset( object , idents = aba_cells   )   }
  object <- subset( object  , idents = unique( cell_number$Var1 ) )
  if(  nrow(object) < 1000  ){
    fvf <- rownames(object) %>% as.character()
  }else{
    fvf <- Seurat::VariableFeatures( Seurat::FindVariableFeatures(object, nfeatures = 1000 ,  verbose = F) )
  }


  #
  exp <- NULL
  meta.data <- NULL
  id_trans <- NULL
  #message( ww.log_time_title() , ww.log_text_coloured( 's' ) , 'Method: ' , method , '.'  )
  #
  if( method == 'supercell' ){
    #
    ww.package_install( "SuperCell" , method = "remotes::install_github('GfellerLab/SuperCell')"  )

    #
    target_ratio <- min(cell_number$Freq) /  min.target

    #
    GE <- Seurat::GetAssayData(  object =  object , assay = assay , layer = layer  )

    sc_result <- SuperCell::SCimplify(
      X = GE[ which( rownames(GE) %in% fvf  ) ,  ],
      cell.annotation = as.character( object@meta.data[[ cluster ]] ) ,
      gamma = target_ratio,
      num.threads = max( parallel::detectCores() - 1 , 1 )
    )
    SC.GE <- SuperCell::supercell_GE( ge = GE  , groups =  sc_result$membership ,
                                      mode = "average"   ) %>% as.matrix()
    #
    colnames( SC.GE  )  <- paste( 'MC' , sort(unique(sc_result[["membership"]])) , sep = '.'  )
    exp <- SC.GE
    meta.data <- data.frame( cell.id = colnames(SC.GE)  , cluster = sc_result[["SC.cell.annotation."]]  )
    id_trans <- data.frame( raw.id = colnames(GE)  , cell.id  = paste( 'MC' , sc_result[["membership"]] , sep = '.'  ) )
    #
    meta.data$new.id <- paste( meta.data$cluster , meta.data$cell.id ,sep ='.'  )
    colnames(exp) <- meta.data$new.id
    id_trans$cell.id <- lapply( id_trans$cell.id ,function(x) meta.data$new.id[ meta.data$cell.id == x ][1] ) %>% as.character()
    meta.data$cell.id <- meta.data$new.id
    meta.data$new.id <- NULL
    #
  }
  if(   method == 'mine'  ){
    #
    ww.package_install( "RSpectra" , method = "I"  )
    ww.package_install( "mbkmeans" , method = "B"  )

    #先生成SVD，再聚类，再生成均值
    GE <- Seurat::GetAssayData(  object =  object , assay = assay , layer = layer  )
    sv <- RSpectra::svds(  GE[ which( rownames(GE) %in% fvf  )   ,  ] , k = min(  dim( GE )  ,  max( 30 , nrow( cell_number  ) ) ),
                           opts = list( center = TRUE, scale = TRUE )
                         )
    pc <- sv$v %*% diag(sv$d)
    rownames( pc ) <- colnames(GE)
    #
    target_ratio <- min(cell_number$Freq) /  min.target
    cell_number$target_n <- round( cell_number$Freq / target_ratio )
    cell_number$target_n[  which( cell_number$target_n < min.target )  ] <- min.target
    if( !is.null( max.target ) ){   cell_number$target_n[  which( cell_number$target_n > max.target )  ] <- max.target  }

    mtx_res <- ww.pblapply( cell_number$Var1  , function(x){
      #
      s.matrix <- GE[ , object@meta.data[[ cluster ]]  == x  ]

      tdata <- pc[ object@meta.data[[ cluster ]]  == x  , ]
      tn <- cell_number$target_n[ cell_number$Var1 == x ]

      cluster_res <- mbkmeans::mbkmeans(
        x = t(tdata) ,
        clusters = max( 1 , min( tn , nrow( tdata ) - 2 ) ) ,
        batch_size = min(tn, nrow( tdata ) ),
        max_iters = 100
      )
      #
      id_trans <- data.frame( raw.id = rownames(tdata)  , cell.id  =  cluster_res[["Clusters"]] )
      groups <- factor( cluster_res[["Clusters"]] , levels = sort(unique(cluster_res[["Clusters"]])  ) )

      M <- Matrix::sparseMatrix(i = seq_along(groups), j = as.integer(groups), x = 1)
      sum_expr <- s.matrix %*% M
      n_cell <- Matrix::colSums(M)
      metacell <- base::sweep( sum_expr, 2, n_cell, "/"  )

      rownames(metacell) <- rownames(s.matrix)
      colnames(metacell) <- paste0( sprintf( "%s.MC." , x ), levels(groups) )
      #
      id_trans$cell.id <- paste0( sprintf( "%s.MC." , x ), id_trans$cell.id  )
      return(  list( meta = metacell , trans = id_trans  ) )
    } , time = F )
    names( mtx_res ) <- as.character(cell_number$Var1)
    #
    id_trans <- lapply(mtx_res, function(x) x$trans  ) %>% bind_rows()
    mtx_res <- lapply( mtx_res , function(x) x$meta  )
    names( mtx_res ) <- as.character(cell_number$Var1)
    #
    meta.data <- lapply(names(mtx_res), function(x){
      return( data.frame( cell.id = colnames(  mtx_res[[x]] )  , cluster = x )   )
    }) %>% bind_rows()
    exp <- do.call( base::cbind, mtx_res )
    #
  }

  ######ABA
  exp <- Matrix::Matrix( exp , sparse = TRUE )
  meta.data <- base::data.frame( meta.data  )
  id_trans <- base::data.frame( id_trans  )

  if( length( aba_cells ) != 0 &  min.cluster.reserved  ){
    #
    aba_exp <-  Seurat::GetAssayData(  object = aba_obj , assay = assay , layer = layer  )
    aba_meta <- data.frame( cell.id = colnames(aba_exp)  , cluster = as.character(  Idents( aba_obj ) ) )
    aba_trans <- data.frame(  raw.id =  aba_meta$cell.id   , cell.id  = aba_meta$cell.id  )
    #
    exp <- Matrix::cbind2( exp , aba_exp )
    meta.data <- base::rbind( meta.data , aba_meta )
    id_trans <- base::rbind( id_trans , aba_trans )
    #
  }
  #
  #message( ww.log_time_title() , ww.log_text_coloured( 'c' )   )
  #
  meta.data <- meta.data[ match( colnames(exp) , meta.data$cell.id ) , ]
  id_trans <- id_trans[ match( colnames(exp) , id_trans$cell.id ) , ]
  #
  return( list( mtx = exp , meta.data = meta.data , method = method , matched.id = id_trans,
                non.metacell = aba_cells , non.metacell.reserved = min.cluster.reserved  )   )
}



sc.metacell.group <- function( object , cluster , group.by ,
                         assay = NULL , layer = NULL,
                         method = 'supercell' , min.cluster = 50 , min.cluster.reserved = T, min.target = 50 , max.target = NULL ){
  #
  ww.package_library( dplyr , Seurat ,  data.table , Matrix  )

  #
  meta.data <- data.frame(object@meta.data)

  #
  stat.res <- table(  meta.data[[group.by]] ,  meta.data[[cluster]] ) %>% as.data.frame()
  colnames(stat.res) <- c( 'group' , 'cluster' , 'cell.number'  )

  aba_df <- stat.res[ stat.res$cell.number < 2, ]
  stat.res <- stat.res[ stat.res$cell.number > 1, ]
  stat.res$group <- as.character( stat.res$group  )
  stat.res$cluster <- as.character( stat.res$cluster  )

  #
  if( nrow( stat.res  ) == 0  ){
    mtx = NULL
    meta.data = NULL
    matched.id = NULL
  }else{
    #
    all_group <- unique( stat.res$group )
    metacell_res <- lapply( all_group , function(xg){
      #
      message( sprintf( '[ %s/%s ] Group: %s ' , which( all_group == xg ) , length( all_group ) , xg ) )
      #
      x  <-  stat.res[  stat.res$group == xg , ]
      #
      scells <- meta.data[ which( meta.data[[group.by]] == xg & meta.data[[cluster]] %in% x$cluster ) , ] %>% rownames()

      suppressWarnings(
        #
        res <- sc.metacell.cluster( object = subset( object , cells = scells  )  , cluster = cluster ,
                                    assay = assay , layer =  layer,
                                    method = method , min.cluster = min.cluster , min.cluster.reserved = min.cluster.reserved, min.target = min.target ,
                                    max.target = max.target
        )
        #
      )

      res$meta.data$group <- xg
      res$meta.data$cell.id <- paste(  xg, res$meta.data$cell.id , sep = '.'  )

      res$matched.id$group <- xg
      res$matched.id$cell.id <- paste(  xg, res$matched.id$cell.id , sep = '.'  )

      colnames(res$mtx) <- paste(  xg, colnames(res$mtx) , sep = '.'  )

      return( res )
      #
    })
    #
    meta.data <- lapply( metacell_res , function(x) x$meta.data ) %>% bind_rows()
    matched.id <- lapply( metacell_res , function(x) x$matched.id ) %>% bind_rows()
    mtx <- lapply( metacell_res, function(x) x$mtx  )
    mtx <- do.call( base::cbind, mtx )
    #
    meta.data <- meta.data[ match( colnames(mtx) , meta.data$cell.id ) , ]
    matched.id <- matched.id[ match( colnames(mtx) , matched.id$cell.id ) , ]
  }
  #
  return(  list(  mtx = mtx , meta.data = meta.data , method = method ,
                  matched.id = matched.id ,
                  non.metacell.reserved = min.cluster.reserved
        ) )

}


#' Metacell for seurat object
#'
#' @description
#' Generates a metacell expression matrix for a Seurat object, where each metacell is constructed by aggregating multiple single cells and its expression is defined as the mean expression across the constituent cells.
#'
#' @param object A processed Seurat object.
#' @param cluster The column name in the Seurat object's meta.data that indicates cell types.
#' @param group.by The column name in the Seurat object's meta.data that indicates the group. If specified, the object will be split by this column prior to metacell generation, creating metacells independently for each subset.
#' @param assay Same as [Seurat::GetAssayData].
#' @param layer Same as [Seurat::GetAssayData].
#' @param method Metacell generation algorithms:
#' \itemize{
#'   \item mine: using our algorithm, we first hierarchically clustered all single cells and then aggregated highly similar cells into metacells. The proportional scaling strategy is used, with the maximum number of metacells determined by the max.target parameter.
#'   For large-scale datasets, this method achieves higher computational efficiency than SuperCell and other existing approaches, with a more substantial improvement when the `max.target` parameter is applied.
#'   \item supercell: use the `SuperCell` package with the same scale factor.
#'   \item hdwgcna: use the `hdWGCNA` package
#' }
#' @param min.cluster Clusters containing more cells than this value are used for metacell generation. Recommendation: 50.
#'
#' The value of `min.cluster` cannot be smaller than `min.target`!!!
#' @param min.cluster.reserved Whether to retain clusters containing fewer cells than min.cluster. When TRUE, the original expression values of these clusters are retained rather than the metacell values. Clusters with only 1 cell are always excluded.
#' @param min.target The expected number of cells for the smallest cluster after metacell generation. This value is used to calculate the scaling factor. If the smallest cluster contains an actual number of N cells, the scaling factor is calculated as N / min.target. All other clusters are scaled according to this factor.
#' @param max.target Available only when method = "mine". This parameter defines the maximum number of cells in the largest cluster after proportional scaling. Clusters larger than this limit will no longer be scaled proportionally.
#'
#' @returns
#' A list containing: (1) the metacell expression matrix (averaged), and (2) the cluster assignment of each metacell.
#'
#' @export
#'
ww.sc.metacell <- function( object , cluster , group.by = NULL ,
                            assay = NULL , layer = NULL,
                            method = 'mine' , min.cluster = 50 , min.cluster.reserved = T, min.target = 50 , max.target = NULL ){

  #
  if( min.cluster < min.target ){
    stop( "The value of `min.cluster` cannot be smaller than `min.target`." )
  }
  message( ww.log_time_title() , ww.log_text_coloured( 's' ) , 'Method: ' , method , '.'  )
  #
  res <- NULL
  if( is.null( group.by  ) ){
    suppressWarnings(
      res <- sc.metacell.cluster( object = object , cluster = cluster ,
                           assay = assay , layer =  layer,
                           method = method , min.cluster = min.cluster , min.cluster.reserved = min.cluster.reserved, min.target = min.target ,
                           max.target = max.target
                           )
    )
  }else{
    suppressWarnings(
    res <- sc.metacell.group( object = object , cluster = cluster , group.by = group.by ,
                               assay = assay , layer =  layer,
                               method = method , min.cluster = min.cluster , min.cluster.reserved = min.cluster.reserved, min.target = min.target ,
                               max.target = max.target
                              )
    )

  }
  #
  message( ww.log_time_title() , ww.log_text_coloured( 'c' )   )

  #
  return( res )
}

