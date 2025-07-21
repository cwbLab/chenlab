#' 转换基因ID
#'
#' @description
#' 转换基因ID。仅用于一种ID的转换，返回第一个匹配的结果。
#' 结果为一个数据框，与输入的基因向量一一对应。
#'
#' @param genes 基因向量名
#' @param ref 参考的org对象
#' @param ip.type 输入基因向量的类型
#' @param op.type 输出基因向量的类型
#'
#' @export
convert_id <- function( genes, ref ,ip.type, op.type ){
  #
  ci <- AnnotationDbi::mapIds(x = ref,
                              keys=genes,
                              column=op.type,
                              keytype = ip.type,
                              multiVals = 'first'
  )
  #
  res <- data.frame( raw = genes, op.type = ci  )
  colnames(res) <- c('raw', op.type )
  #
  return(  res  )
}




#' 处理重复的行
#' @description
#' 转换ID后，可能会有多个行对应同一ID的情况，可以使用该函数进行处理，保留唯一行。
#'
#'
#' @param exp 表达矩阵
#' @param raw_row 原始的基因向量（行名）
#' @param convert_name 转换后的基因向量（与raw_row对应）。
#' @param type 保留数据的方式。可以选max、min、mean。为mean时，返回多行数据的均值。
#' @param mc.cores 默认调用最多线程
#'
#' @export

exp_matrix_unique_row <- function( exp  , raw_row , convert_name , type = 'max' ,  mc.cores = NULL  ){
  #
  library(  pbmcapply  )
  library( dplyr )
  library( data.table )


  #
  if( is.null( mc.cores  ) ){  mc.cores = parallel::detectCores()  }

  exp$Description <- convert_name
  #统计convert基因的个数
  gene_counts <- table( exp$Description  ) %>% as.data.frame()
  gene_counts <- gene_counts[ order(gene_counts$Freq,decreasing = T)  ,]


  #先选择没有重复的基因，sdata1
  samples <- colnames(sdata1)
  sdata1 <- exp[ exp$Description %in% gene_counts$Var1[gene_counts$Freq == 1]  , ]

  #处理有重复的基因，sdata2
  dup_genes <-  gene_counts$Var1[gene_counts$Freq != 1]
  sdata2 <- pbmclapply(dup_genes, function(x){
    sd <- exp[ exp$Description == x ,   ]
    sd2 <- sd[,-ncol(sd)]
    sd2 <- sd2 %>% apply(1,as.numeric) %>% as.data.frame()%>% t() %>% as.data.frame()
    if( type == 'max' ){
      uniq_data <-  sd[ which.max(rowSums(sd2)) , ]
    }else if( type == 'min' ){
      uniq_data <-  sd[ which.min(rowSums(sd2)) , ]
    }else if( type == 'mean'  ){
      uniq_data <- data.table(matrix( c( colMeans( sd2 ) , x  ) , nrow = 1   ))
    }

    return( data.table(uniq_data)  )
  } ,mc.cores = mc.cores ) %>% rbindlist() %>% as.data.frame()
  colnames(sdata2) <- samples

  #合并最终的数据
  final_data <- rbind( sdata1 , sdata2   ) %>% as.data.frame()
  rownames(final_data) <- final_data$Description
  final_data <- final_data[ ,  -ncol( final_data ) ] %>% as.data.frame()

  #把数据转换成 数值
  genes <- rownames(final_data)
  numeric_data <- apply(  final_data , 2 , as.numeric ) %>% as.data.frame()
  rownames(numeric_data) <- genes

  #返回数据
  return(   numeric_data   )

}





