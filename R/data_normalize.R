
#' 计算CPM、TPM、FPKM
#' @description
#' 基于featurecounts算法的原始输出结果，分别计算CPM、TPM、FPKM。
#'
#' @param count_file featurecounts原始输出文件的绝对路径
#' @param op.dir 保存结果文件的路径
#'
#' @export
#'

data_normalize <- function( count_file , op.dir = '.' ){
  library(scater)
  library(stringr)

  ###start
  #input
  raw_data<- read.table(count_file,sep = '\t',header = T,row.names = 1,check.names = F)
  raw_data<- raw_data[rownames(raw_data) != '',]
  raw_title <- unlist(str_split(readLines(myfile,n=2)[2],'\\t'))[-(1:6)]

  #get reference information
  ref_data <- raw_data[,c(1:5)]
  ref_data$gene <- rownames(ref_data)
  ref_data <- dplyr::select(ref_data,gene,everything())
  write.table(ref_data,
              file = paste( op.dir,'gene_information_by_featurecounts.txt',sep='/' ),
              sep = '\t',quote = F,row.names = F)

  #calculate
  lengths <- as.integer(raw_data$Length)
  count <- as.data.frame(apply(raw_data[,-c(1:5)],2,as.integer))
  rownames(count) <- rownames(raw_data)
  sce <- SingleCellExperiment(assays=list(counts=count))
  tpm(sce) <- calculateTPM(sce,lengths=lengths)
  fpkm(sce)<- calculateFPKM(sce,lengths=lengths)
  cpm(sce) <- calculateCPM(sce)

  #output
  for ( i in names(sce@assays@data)){
    data <- as.data.frame(assay(sce,i))
    colnames(data) <- raw_title
    data$Genes <- rownames(data)
    data <- data[,c(ncol(data),1:(ncol(data)-1))]
    write.table(data,
                file =  paste( op.dir  ,paste(i,'.chenlab_output.txt',sep = '_') ,sep='/' ),
                sep = '\t',quote = F,row.names = F)
  }
  print('Mission Finished')
  ###end
}
