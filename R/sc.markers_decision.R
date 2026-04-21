
#' Marker decision
#'
#' @description
#' Determine the most representative marker genes for each cluster based on the GMT file and differential expression of markers.
#'
#'
#' @param object A processed Seurat object with computed embeddings (e.g., PCA/UMAP) and defined cluster identities accessible via the `Seurat::Idents` function.
#' @param gmt An absolute file path to a GMT file containing candidate marker genes for each cell type.
#' @param top Provide an integer specifying the number of top-ranked positive markers (sorted by avg_log2FC) to retain for downstream analysis.
#'
#' @returns
#' A data.frame object. The `level` column indicates the confidence level, with level 1 being the highest and level 4 the lowest.
#'
#' 1. This function comprehensively evaluates the differential expression and expression specificity of marker genes to assign confidence levels. Levels 1 and 2 indicate high confidence, while levels 3 and 4 are for reference only and are recommended to be manually reviewed and filtered. Markers at level 1 should be retained as much as possible. For other levels, return no more than ten markers.
#'
#' 2. Up to five specific marker genes are provided for each cell type.
#'
#'
#' @export
#'
wb.sc.markers_decision <- function( object , gmt , top = 30 ){
  #
  library(dplyr);library(clusterProfiler)
  library(Seurat);library(data.table)

  #gmt
  mygmt <- clusterProfiler::read.gmt( gmt )
  mygmt$term <- as.character( mygmt$term  )

  #get markers
  mymarkers <- FindAllMarkers(object = object,logfc.threshold = 0, return.thresh = 2,
                              min.pct = 0, only.pos = F , min.cells.feature = 0,
                              min.cells.group = 0
  )

  ####
  get_marker_used <- function( gmt_data, marker_res , top  ){
    #
    cells <- unique(marker_res$cluster) %>% as.character()
    marker_res$pct.diff <- marker_res$pct.1  - marker_res$pct.2
    marker_res$cluster <- as.character( marker_res$cluster )

    #1
    marker_res$marker_y <- wb.smc( 1:nrow( marker_res  ), function(x){
      c = marker_res$cluster[x] ; g = marker_res$gene[x]
      #
      my = 'N'
      s = gmt_data$gene[ gmt_data$term == c  ]
      if( g %in% s ){ my = 'Y'  }
      #
      return(my)
    } ) %>% unlist() %>% as.character()

    #2.based top
    marker_res_top30 <- marker_res %>%
      filter( avg_log2FC > 0.2  & p_val_adj < 0.05  & pct.1  > 0.3 ) %>%
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
      sd1 <- sd[ sd$cluster.number == 1,  ]
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
    #
    return( marker_used )
  }

  #run
  markers_used <- get_marker_used( gmt_data = mygmt ,
                                     marker_res = mymarkers ,
                                     top = top
                                     )

  #
  return( markers_used )
}
