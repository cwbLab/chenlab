#' Convert gene IDs
#'
#' @description
#' This function is a wrapper around mapIds and select from AnnotationDbi, used for converting gene IDs.
#'
#' @param genes Names of the gene vector
#' @param ref Reference org annotation object
#' @param ip.type A vector containing one or more types can be provided
#' @param op.type A vector containing one or more types can be provided
#' @param type Output mode. Options are 'first' or 'any'. When 'first', the result corresponds one-to-one with the input gene vector. When 'any', a single gene may produce multiple rows in the result.
#' @param mc.cores Number of cores used for parallel computation. By default, the maximum computing resources are used
#'
#' @export
wb.convert_id <- function( genes, ref ,ip.type, op.type , type = 'first' ,mc.cores = NULL ){
  library(pbmcapply)
  library(data.table)
  #
  if( is.null( mc.cores  ) ){  mc.cores = parallel::detectCores()   }
  #
  re.type = type
  if( type == 'any' ){ re.type = 'list'   }

  op <- lapply( ip.type , function(type){
    #
    if( length(op.type) == 1 ){
      #
      ci <- AnnotationDbi::mapIds(x = ref,
                                  keys=genes,
                                  column=op.type,
                                  keytype = type,
                                  multiVals = re.type
      )
      #
      if( re.type == 'first' ){
        res <- data.frame( raw = genes, op.type = ci  )
        colnames(res) <- c( type , op.type )
      }else{
        res <- pbmclapply(1:length(ci), function(x){
          temp = data.table( raw = names(ci)[x], op.type = ci[[ x ]]  )
          return( temp )
        },mc.cores = mc.cores )
        res <- data.frame(rbindlist( res  ))
        colnames(res) <- c( type , op.type )
      }
    }else{
      #
      res <- AnnotationDbi::select(x = ref,
                                   keys=genes,
                                   columns=op.type,
                                   keytype = type
      )
      res <- data.frame( res  )
    }
    #
    return(res)
  })
  names(op) <- ip.type
  #output
  return(  op  )
}




#' Process duplicate row names in the input matrix
#' @description
#' After ID conversion, multiple rows may correspond to the same ID. This function can be used to process them and retain unique rows
#'
#'
#' @param exp Expression matrix
#' @param raw_row Original gene vector (usually the row names)
#' @param convert_name onverted gene vector (corresponding to raw_row)
#' @param type Method for retaining data. Options are 'max', 'min', or 'mean'. When 'mean' is selected, the mean of multiple rows is returned
#' @param mc.cores Number of cores used for parallel computation. By default, the maximum computing resources are used
#'
#' @export

wb.mtx_unique_row <- function( exp  , raw_row , convert_name , type = 'max' ,  mc.cores = NULL  ){
  #
  library(  pbmcapply  )
  library( dplyr )
  library( data.table )


  #
  if( is.null( mc.cores  ) ){  mc.cores = parallel::detectCores()  }

  exp$Description <- convert_name
  #
  gene_counts <- table( exp$Description  ) %>% as.data.frame()
  gene_counts <- gene_counts[ order(gene_counts$Freq,decreasing = T)  ,]


  #sdata1
  samples <- colnames(sdata1)
  sdata1 <- exp[ exp$Description %in% gene_counts$Var1[gene_counts$Freq == 1]  , ]

  #sdata2
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

  #merge
  final_data <- rbind( sdata1 , sdata2   ) %>% as.data.frame()
  rownames(final_data) <- final_data$Description
  final_data <- final_data[ ,  -ncol( final_data ) ] %>% as.data.frame()

  #numeric
  genes <- rownames(final_data)
  numeric_data <- apply(  final_data , 2 , as.numeric ) %>% as.data.frame()
  rownames(numeric_data) <- genes

  #return
  return(   numeric_data   )

}





