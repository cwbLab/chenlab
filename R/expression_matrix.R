#' Convert gene IDs
#'
#' @description
#' This function encapsulates the mapIds and select functions from the AnnotationDbi package and the homologene function, providing a unified interface for gene identifier conversion.
#'
#' @param genes A vector of gene names.
#' @param ref The referenced Bioconductor annotation object typically starts with org and commonly contains the following columns:
#'
#' ACCNUM,
#'
#' ALIAS,
#'
#' ENSEMBL,
#'
#' ENSEMBLPROT,
#'
#' ENSEMBLTRANS,
#'
#' ENTREZID,
#'
#' ENZYME,
#'
#' EVIDENCE,
#'
#' EVIDENCEALL,
#'
#' GENENAME,
#'
#' GENETYPE,
#'
#' GO,
#'
#' GOALL,
#'
#' IPI,
#'
#' MAP,
#'
#' OMIM,
#'
#' ONTOLOGY,
#'
#' ONTOLOGYALL,
#'
#'
#' PATH,
#'
#' PFAM,
#'
#' PMID,
#'
#' PROSITE,
#'
#' REFSEQ,
#'
#' SYMBOL,
#'
#' UCSCKG,
#'
#' UNIPROT.
#' @param ip.type A vector of one or more gene types. Gene types must exist in the object specified by ref. Defaults to NULL, which enables automatic detection.
#' @param op.type Similar to the ip.type, it indicates the gene identifier type to be output.
#' @param type Output mode. Options are 'first' or 'any'. When set to "first" and the length of op.type is 1, the result corresponds one-to-one with the input gene vector. When 'any', a single gene may produce multiple rows in the result.
#' @param homologous_species A vector of species IDs for homology conversion. If specified, homologous gene IDs from the specified species will also be returned after gene ID conversion. Available species IDs can be obtained via homologene::taxData, e.g. 9606 (Homo sapiens), 10090 (Mus musculus).
#' @param threads Number of cores used for parallel computation. By default, the maximum computing resources are used.
#'
#' @examples
#' gene_convert <- wb.convert_id( genes = c( 'ENSG00000001167', 'ENSG00000002079', 'ENO2', 'NKG2A'),
#'		ref = org.Hs.eg.db,
#'		ip.type = c('ENSEMBL', 'ALIAS'), op.type = c('SYMBOL', 'ENTREZID'),
#'		homologous_species =  10090 )
#' @export
#'
wb.convert_id <- function( genes,ref,ip.type = NULL,op.type,
                           type = 'first', homologous_species = NULL,threads = NULL ){
  library(parallel)
  library(data.table)
  library(dplyr)
  library(AnnotationDbi)
  #
  if( is.null( threads  ) ){  threads = max(1, parallel::detectCores() - 1)   }
  #
  re.type = type
  if( type == 'any' ){ re.type = 'list'   }

  #
  if( 'NULL' %in% class( ip.type )  ){  ip.type = AnnotationDbi::columns( ref ) ; ip.type = ip.type[ !ip.type %in% op.type ]  }

  op <- lapply( ip.type , function(type){

    hit_error <-tryCatch({
      suppressMessages(
        ci <- AnnotationDbi::mapIds(x = ref,
                                    keys=genes,
                                    column='ENTREZID',
                                    keytype = type,
                                    multiVals = re.type
        )
      )
    },
    error = function(e) { NULL }
    )
    #
    if ( ! "NULL" %in% class( hit_error )  ){
      #
      if( length(op.type) == 1 ){
        #
        suppressMessages(
          ci <- AnnotationDbi::mapIds(x = ref,
                                      keys=genes,
                                      column=op.type,
                                      keytype = type,
                                      multiVals = re.type
          )

        )

        #
        if( re.type == 'first' ){
          res <- data.frame( raw = genes, op.type = ci  )
          colnames(res) <- c( type , op.type )
        }else{
          res <- mclapply(1:length(ci), function(x){
            temp = data.table( raw = names(ci)[x], op.type = ci[[ x ]]  )
            return( temp )
          },mc.cores = threads )
          res <- data.frame(rbindlist( res  ))
          colnames(res) <- c( type , op.type )
        }
      }else{
        #
        suppressMessages(
          res <- AnnotationDbi::select(x = ref,
                                       keys=genes,
                                       columns=op.type,
                                       keytype = type
          )
        )
        #
        res <- data.frame( res  )
      }

      #
      if(  all(is.na(res[,-1]))  ){ res <- NULL }

    }else{
      res <- NULL
    }

    #
    return(res)
  })

  ip.type <- ip.type[ !sapply(op, is.null) ]
  op <- op[!sapply(op, is.null)]

  names(op) <- ip.type

  ######homologene
  if( !is.null( homologous_species ) ){
    raw_spe <- AnnotationDbi::taxonomyId(ref)
    #
    op <- lapply(ip.type, function(x){
      data  =  op[[x]]
      suppressMessages(
        temp_id <- AnnotationDbi::mapIds(x = ref,
                                         keys= data[[1]] ,
                                         column= 'ENTREZID',
                                         keytype = x,
                                         multiVals = re.type
        )
      )

      #
      all_ids <- unlist( temp_id  ) %>% as.integer()
      all_ids <- all_ids[ !is.na( all_ids ) ]

      #
      homo_gs <- lapply(  homologous_species , function(y){
        homo_res <- homologene::homologene( genes = all_ids,  inTax = raw_spe , outTax = y )
        #
        colnames(homo_res) <- stringr::str_replace_all( colnames(homo_res) ,'ID','ENTREZID' )
        colnames(homo_res)[c(1,2)] <- paste( colnames(homo_res)[c(1,2)] , 'SYMBOL' ,sep='_'  )
        #
        return( homo_res  )
      })

      ###merge result
      merged_res <-  mclapply( 1:nrow(data) , function(y){
        #
        ids = temp_id[[  data[y,][[1]]   ]] %>% as.integer()

        test <- lapply( ids ,function(id){
          raw_data <- data[y , ]
          #
          if ( !is.na( id ) ){
            #
            a <- lapply( homo_gs  , function(z){
              #
              sd = z[ z[[3]] == id ,  ]
              if ( nrow(sd) == 0 ){
                raw_data <<- data.table( raw_data , matrix( rep( NA, 4 ) , nrow = 1  ) )
              }else{
                raw_data <<- data.table( raw_data , sd  )
              }
            })
          }else{
            raw_data <- data.table( raw_data , matrix( rep( NA, length( homo_gs ) * 4 ) , nrow = 1  ) )
          }
          #
          return( raw_data )
        } ) %>% rbindlist( use.names = F )
        #
        return( test )

      } , mc.cores = threads ) %>% rbindlist( use.names = F )

      #
      merged_res <- setDF( merged_res )
      new_colnames <- c( colnames(data) , lapply(homo_gs, function(y) colnames(y) ) %>% unlist() )
      merged_res <- merged_res[  , !duplicated(  new_colnames ) ]
      colnames(merged_res) <- new_colnames[   !duplicated(  new_colnames )   ]
      merged_res <- merged_res[, c(1:(  ncol(data) + 1  ) , (  ncol(data) + 3  ), (  ncol(data) + 2  ), (  ncol(data) + 4  ):ncol(merged_res))]
      #
      return( merged_res )

    })
    #
    names(op) <- ip.type
  }

  #output
  final_res <- lapply(names(op), function(type){
    #
    x <- op[[type]]
    x <- cbind( Raw_Type = type , x  )
    colnames(x)[2] <- 'Raw_ID'
    #
    return(x)
  })
  names(final_res) <- names(op)
  #
  return(  final_res  )
}



#' Process duplicate row names in the input matrix
#' @description
#' After ID conversion, multiple rows may correspond to the same ID. This function can be used to process them and retain unique rows.
#'
#'
#' @param exp Expression matrix.
#' @param raw_row Original gene vector (usually the row names).
#' @param convert_name onverted gene vector (corresponding to raw_row).
#' @param type Method for retaining data. Options are 'max', 'min', 'sum', 'median', or 'mean'. When 'mean' is selected, the mean of multiple rows is returned.
#'
#' @export
wb.mtx_unique_row <- function( exp, raw_row, convert_name, type = 'max' ){
  library(data.table)

  #
  if(length(raw_row) != nrow(exp) | length(convert_name) != nrow(exp)){
    stop("Length of raw_row and convert_name must equal nrow(exp)")
  }

  #
  dt <- as.data.table(exp)
  dt[, target_id := convert_name]

  #
  method <- match.arg(type, c("max", "min", "sum", "mean", "median"))
  func <- switch(method,
                 "max"    = function(x) max(x, na.rm = TRUE),
                 "min"    = function(x) min(x, na.rm = TRUE),
                 "sum"    = function(x) sum(x, na.rm = TRUE),
                 "mean"   = function(x) mean(x, na.rm = TRUE),
                 "median" = function(x) stats::median(x, na.rm = TRUE)
  )


  #
  message(paste0("Merging duplicate rows using ", method, " method..."))
  dt_agg <- dt[, lapply(.SD, func), by = target_id]

  #
  dt_agg <- dt_agg[!is.na(target_id) & target_id != ""]

  #
  res_mat <- as.data.frame( as.matrix(dt_agg[, -1, with = FALSE]) )
  rownames(res_mat) <- dt_agg$target_id

  return(res_mat)
}








