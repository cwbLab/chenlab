
#' Marker decision
#'
#' @description
#' Determine the most representative marker genes for each cluster based on the GMT file and differential expression of markers.
#'
#'
#' @param object Two methods:
#' 1. Seurat object. A processed Seurat object with computed embeddings (e.g., PCA/UMAP) and defined cluster identities accessible via the `Seurat::Idents` function.
#'
#' 2. The raw output returned by this function. It can be provided as input to a subsequent run, in which case the existing `DEG` results will be reused for downstream analyses without re-computing differential expression.
#' @param gmt An absolute file path to a GMT file containing candidate marker genes for each cell type.
#' @param top Provide an integer specifying the number of top-ranked positive markers (sorted by avg_log2FC) to retain for downstream analysis.
#' @param level1.threshold Differential expression threshold that Level 1 markers must satisfy.
#'
#' @returns
#' A list object. The final results are available in the `markers` component.
#'
#' This function comprehensively evaluates the differential expression and expression specificity of marker genes to assign confidence levels. The `level` column indicates the confidence level, with level 1 being the highest and level 4 the lowest.
#'
#' Levels 1 and 2 represent high-confidence markers. Levels 3 and 4 are provided for reference only and should be manually reviewed. Level 1 markers are retained without a strict limit whenever possible. For levels 2–4, no more than ten markers are returned.
#'
#' @examples
#'
#' marker1 <- ww.sc.markers_decision( object = seurat.obj , gmt = 'test.gmt' , top = 20   )
#' result1 <- marker1$markers
#'
#' marker2 <- ww.sc.markers_decision( object =  marker1 , gmt = 'test.gmt' , top = 40   )
#' result2 <- marker2$markers
#'
#' marker3 <- ww.sc.markers_decision( object =  marker2 , gmt = 'new.gmt' , top = 40   )
#' result3 <- marker3$markers
#'
#' #result4 produces the same results as result3.
#' marker4 <- ww.sc.markers_decision( object =  seurat.obj , gmt = 'new.gmt' , top = 40   )
#' result4 <- marker4$markers
#'
#' @export
#'
ww.sc.markers_decision <- function( object , gmt , top = 20 , level1.threshold = 'avg_log2FC > 0.3 & p_val_adj < 0.05 & pct.1 > 0.3' ){

  #
  ww.package_install( "Seurat" , method = "I"  )

  #
  ww.package_library( dplyr , clusterProfiler ,  Seurat ,  data.table   )

  ######
  get_marker_used <- function( gmt_data, marker_res , top , aveExp , level1.threshold = level1.threshold ){
    #
    cells <- unique(marker_res$cluster) %>% as.character()
    marker_res$pct.diff <- marker_res$pct.1  - marker_res$pct.2
    marker_res$cluster <- as.character( marker_res$cluster )

    #1
    marker_res$marker_y <- ww.smc( 1:nrow( marker_res  ), function(x){
      c = marker_res$cluster[x] ; g = marker_res$gene[x]
      #
      my = 'N'
      s = gmt_data$gene[ gmt_data$term == c  ]
      if( g %in% s ){ my = 'Y'  }
      #
      return(my)
    } ) %>% unlist() %>% as.character()

    #2.based top
    marker_res_top30 <- base::subset( marker_res , eval(parse(text = level1.threshold ) )  )
    marker_res_top30 <- marker_res %>%
      group_by(cluster) %>%
      arrange(desc( avg_log2FC  ), .by_group=T) %>%
      top_n(n = top, wt = avg_log2FC )
    s_marker_res_top30 <- marker_res_top30[ marker_res_top30$marker_y == 'Y'  ,  ]
    markers_number <- table( marker_res_top30$gene  ) %>% as.data.frame()
    s_marker_res_top30$cluster.number <- lapply(s_marker_res_top30$gene,
                                                function(x) markers_number$Freq[markers_number$Var1 == x  ]
    )
    #
    marker_used <- lapply(cells, function(x){

      sd <- s_marker_res_top30[ s_marker_res_top30$cluster == x  ,   ]
      sd <- arrange(  sd , -pct.diff , cluster.number )
      #
      sd1 <- sd[ sd$cluster.number == 1,  ]
      #
      if( nrow(sd1) < 2 ){
        return( NULL )
      }else{
        return(   data.table( cell = x , gene = sd1$gene , pct.diff = sd1$pct.diff  , level = 1   ) )
      }
    })
    marker_used <- marker_used[ !( lapply( marker_used , is.null ) %>% unlist() )    ] %>% rbindlist()

    ###3. Deal with NA
    marker_res_top100 <- marker_res %>%
      filter( avg_log2FC > 0  & marker_y == 'Y' ) %>%
      group_by(cluster) %>%
      arrange(desc( pct.diff ), .by_group=T) %>%
      top_n(n = 100, wt = pct.diff )
    s_marker_res_top100 <- marker_res_top100[ marker_res_top100$marker_y == 'Y'  ,  ]
    markers_number <- table( marker_res_top100$gene  ) %>% as.data.frame()
    s_marker_res_top100$cluster.number <- lapply(s_marker_res_top100$gene,
                                                 function(x) markers_number$Freq[markers_number$Var1 == x  ]
    )

    marker_used2 <- lapply(cells, function(x){

      sd <- s_marker_res_top100[ s_marker_res_top100$cluster == x  ,   ]
      sd <- arrange(  sd , -pct.diff , cluster.number )

      #
      #sd1 <- sd[ sd$cluster.number == 1,  ]
      sd1 <- sd
      op = NULL
      #
      if( nrow(sd1) < 2 ){
        if( nrow(sd)  != 0 ){
          op = data.table( cell = x , gene = sd1$gene , pct.diff = sd1$pct.diff ,level = 3   )
          if( nrow(op) > 10 ){
            op = data.table( cell = x , gene = sd1$gene[1:10] , pct.diff = sd1$pct.diff[1:10] ,level = 3   )
          }
        }else{
          #
          sm <- marker_res %>% filter( marker_y == 'Y' & cluster == x )
          op = data.table( cell = x , gene = sm$gene, pct.diff = sm$pct.diff,level = 4   )
          #
        }
      }else{
        if( nrow(sd1) > 10 ){
          op = data.table( cell = x , gene = sd1$gene[1:10] , pct.diff = sd1$pct.diff[1:10], level =2     )
        }else{
          op = data.table( cell = x , gene = sd1$gene , pct.diff = sd1$pct.diff,  level = 2   )
        }
      }
      #
      return(op)

    })
    marker_used2 <- marker_used2[ !( lapply( marker_used2 , is.null ) %>% unlist() ) ] %>% rbindlist()
    #
    marker_used <- rbind( marker_used , marker_used2 )

    #
    number <- table( marker_used$gene  ) %>% as.data.frame()
    marker_used$hited_celltype <-  lapply(  marker_used$gene ,
                                            function(x) number$Freq[ number$Var1 == x  ]
    ) %>% as.character()

    marker_used$temp <- paste(  marker_used$cell , marker_used$gene , sep = '..|..' )
    marker_used <- marker_used[ order( marker_used$level , decreasing = F  )   ,  ]
    marker_used <- marker_used[ !duplicated( marker_used$temp  )  ,   ]
    marker_used$temp <- NULL

    #4. Additional Information
    aveExp <- as.matrix(  aveExp[[1]]  ) %>% as.data.frame()
    add_info  <- lapply( 1:nrow( marker_used ) , function(x){
      x = unlist( marker_used[x , ]  )
      t1 = marker_res$pct.1[ marker_res$cluster == x[[1]] & marker_res$gene == x[[2]] ]
      t2 = marker_res$pct.2[ marker_res$cluster == x[[1]] & marker_res$gene == x[[2]] ]
      t3 = aveExp[   rownames(aveExp) == x[[2]]  ,  colnames(aveExp) == x[[1]]   ]
      #
      return( c( pct.1 = t1, pct.2 = t2 , aveExp = t3 )  )
    }) %>% data.frame() %>% t() %>% data.frame()
    colnames(add_info) <- c(  "pct.1", "pct.2", "aveExp" )
    marker_used <- cbind( marker_used , add_info  )

    #
    marker_used <- marker_used[  , c(  1, 2, 8, 6, 7 , 3 , 4 ,5  )  ]

    #
    return( marker_used )
  }

  ######run
  if( 'list' %in% class( object ) & 'aveExp' %in% names(object) & 'DEGs' %in% names(object) ){
    #
    message( 'The differentially expressed genes (DEGs) and aveExp have already been identified and will be used directly in the downstream analyses without re-evaluation.'  )
    mymarkers <- object$DEGs
    aveExp <- object$aveExp
  }else{
    #get markers
    mymarkers <- FindAllMarkers(object = object,logfc.threshold = 0, return.thresh = 2,
                                min.pct = 0, only.pos = F , min.cells.feature = 0,
                                min.cells.group = 0
    )
    #
    suppressMessages(
      aveExp <- AverageExpression( object, features =  as.character( rownames( object ) )  )
    )
  }

  #gmt
  mygmt <- clusterProfiler::read.gmt( gmt )
  mygmt$term <- as.character( mygmt$term  )

  #run
  markers_used <- get_marker_used( gmt_data = mygmt ,
                                   marker_res = mymarkers ,
                                   top = top,
                                   aveExp = aveExp,
                                   level1.threshold = level1.threshold
  )

  ######
  return( list( markers =  markers_used , DEGs =  mymarkers , top = top , aveExp = aveExp , gmt = gmt  , level1.threshold = level1.threshold ) )
}

