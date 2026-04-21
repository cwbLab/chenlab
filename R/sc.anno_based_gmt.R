#' Title
#' @description
#' This function annotates clusters based on marker gene lists provided in GMT format and the differentially expressed genes of each cluster.
#'
#'
#' @param object A processed Seurat object with computed embeddings (e.g., PCA/UMAP) and defined cluster identities accessible via the `Seurat::Idents` function.
#' @param gmt An absolute file path to a GMT file.
#' @param markers The original output results of Seurat's `FindAllMarkers` function.
#' @param top Provide an integer specifying the number of top-ranked positive markers (sorted by avg_log2FC) to retain for downstream analysis.
#' @param write Whether to save the analysis results locally.
#' @param source Provide a string representing the local experiment ID. It has no computational significance and is used for reference or labeling purposes only.
#'
#' @returns
#' This function returns a data.frame with 10 columns:
#' 1. cell_type: The annotated cell type.
#' 2. markers: Differentially expressed markers supporting the corresponding `cell_type`.
#' 3. markers_number: The number of differentially expressed markers.
#' 4. cluster: The original cluster.
#' 5. cluster_same_as: Clusters that share overlapping differentially expressed markers.
#' 6. not_0_cell_proportion: For the markers in `column 2`, the proportion of non-zero cells in each cluster.
#' 7. not_0_cell_mean_abundance: The mean expression of each marker in non-zero cells.
#' 8. avg_log2FC: The original `avg_log2FC` values corresponding to each marker.
#' 9. hit_markers_num_2_all_markers_num: The ratio of `markers_number` to the `total number of markers` for the corresponding cell_type.
#' 10. marker_source: For annotation purposes only.
#'
#' @export
#'
#'
wb.sc.anno_based_gmt <- function( object , gmt , markers , top = 20 , write = T , source  = 'gmt' ){
  #
  ###load required packages
  library(stringr);library(dplyr);library(GSEABase)
  library(Seurat);library(data.table)

  mysc <- object
  mygmt <- gmt

  #1
  mysc$raw_cluster <- Idents(mysc)

  #2
  marker_data_pos <- subset( markers , avg_log2FC > 0 )
  marker_data_pos <- group_by(marker_data_pos,cluster)
  top_pos <- top_n( marker_data_pos , n = top, wt = avg_log2FC)

  #3
  my_markers_anno <- function(seurat_class,top_res,markers_gmt,source,write){
    ###get input data
    mymarkers <- GSEABase::getGmt(markers_gmt);genes <- split(top_res$gene,top_res$cluster)
    ###get the result of markers annotation
    res <- lapply(genes,function(x){
      cell_res <- lapply(mymarkers, function(y){
        markers <- y@geneIds[which(y@geneIds != '')];cell_name <- y@setName
        hit_markers <- markers[markers %in% x]
        if (length(hit_markers) != 0){return(c(cell_name, paste(sort(hit_markers),collapse  = ';'),length(hit_markers),length(hit_markers)/length(markers),source))}})
      if(length(cell_res[!unlist(lapply(cell_res,is.null))]) == 0){return(NULL)}else{return(cell_res[!unlist(lapply(cell_res,is.null))])}})
    ###estimate the number of clusters which are marked
    if(length(res[!unlist(lapply(res,is.null))]) ==0 ){print('No cluster can be marked.');return(NA)}else{
      ###If at least one cluster can be marked, the program continues to run
      ult_res <- data.frame()
      for (q in names(res)){
        if (length(res[[q]]) != 0){
          values=as.data.frame(t(as.data.frame(lapply(res[[q]], function(x){append(x,q) }))))
          ult_res <- rbind(ult_res,values)}}
      colnames(ult_res) <- c('cell_type','markers','markers_number','hit_markers_num_2_all_markers_num','marker_source','cluster')
      ult_res$cluster_same_as <- apply(ult_res,1,function(x){
        clusters<-ult_res$cluster[ult_res$cell_type==unlist(x)[[1]]]
        clusters<-ifelse(length(clusters) != 1,clusters[clusters != unlist(x)[[6]]] %>% paste(collapse = ';'),NA)})
      ult_res <- dplyr::select(ult_res,-marker_source,marker_source)
      #estimate not 0 cell proportion and abundance of markers for each cluster
      split_data <- SplitObject(seurat_class)
      split_res <- lapply(split_data, function(x){ as.data.frame( GetAssayData( x, layer = 'data' ) )})
      new_res <- wb.smc(1:nrow(ult_res),function(x){
        x = ult_res[x,] %>% unlist() %>% as.character()
        genes <- str_split(x[[2]],';')[[1]]
        proportin_res <- lapply(genes, function(y){
          markders_exp <- unlist(split_res[[x[[5]]]][y,])
          not_0_exp <- markders_exp[markders_exp!=0]
          not_0_cell_proportion <- length(not_0_exp) / length(markders_exp)
          return(not_0_cell_proportion)}) %>% unlist()
        abun_res <- lapply(genes, function(y){
          markders_exp <- unlist(split_res[[x[[5]]]][y,])
          not_0_exp <- markders_exp[markders_exp!=0]
          not_0_cell_mean_abundance <- sum(not_0_exp) / length(not_0_exp)
          return(not_0_cell_mean_abundance)}) %>% unlist()
        avg_log2FC <-lapply(genes, function(y){
          return( top_res$avg_log2FC[top_res$cluster == x[[5]] &  top_res$gene == y ])}) %>% unlist()
        #
        myorder <- order( proportin_res ,decreasing = T  )
        proportin_res <- proportin_res[myorder]
        abun_res <- abun_res[myorder]
        avg_log2FC <- avg_log2FC[myorder]
        genes  <- genes[ myorder]
        #
        op <- as.list(x)
        op2 <- list(paste(proportin_res,collapse = ';'), max(proportin_res),paste(abun_res,collapse = ';'),
                    paste(avg_log2FC,collapse = ';') , paste(  genes  , collapse = ';' )  )

        return( c(op , op2) )
      })
      new_res <- rbindlist( new_res ) %>% as.data.frame()
      colnames( new_res ) <- c( colnames( ult_res )  ,
                                'not_0_cell_proportion' , 'not_0_cell_proportion_max' , 'not_0_cell_mean_abundance' ,
                                'avg_log2FC' , 'genes'  )
      new_res$markers <- new_res$genes
      new_res$genes <- NULL
      ###sort by cluster , markers_number , not_0_cell_proportion_max
      ult_res <- dplyr::select(new_res,-marker_source,-hit_markers_num_2_all_markers_num,hit_markers_num_2_all_markers_num,marker_source)

      ult_res$cluster <- as.character(ult_res$cluster);ult_res$markers_number <- as.integer(ult_res$markers_number)
      ult_res <- dplyr::group_by(ult_res,cluster) %>% dplyr::arrange(desc(markers_number),desc(not_0_cell_proportion_max),.by_group=T) %>% as.data.frame()
      ult_res$cluster <- as.character(ult_res$cluster)
      ult_res <- ult_res[,-which(colnames(ult_res) == 'not_0_cell_proportion_max' )]
      ###output the result
      if( write ){
        write.table(ult_res,file= paste0( source , '.markers_annotation_result.txt' ) ,sep = '\t',quote = F,row.names = F,col.names = T)
      }
      return(ult_res)}
  }

  #4
  anno.res <- my_markers_anno(seurat_class=mysc,top_res=top_pos,markers_gmt=mygmt,source=source,write = write )

  #
  return( anno.res )
}
