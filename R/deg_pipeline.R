
#' Perform differential gene expression analysis
#' @description
#' Automatically calls the DESeq2, edgeR, and limma packages to identify differentially expressed genes
#'
#'
#' @param counts Data frame object containing the counts matrix
#' @param tpm Data frame object containing the TPM matrix
#' @param group Grouping vector for samples (up to two groups)
#' @param contrast Comparison vector, e.g., c('M', 'N'). Here, 'N' is the control, and fold change (FC) is calculated as M/N
#' @param p.adj P-value threshold
#' @param FC Fold change (FC) threshold
#' @param write If set to TRUE, the results of differential gene expression analysis from each algorithm will be automatically saved locally
#'
#' @export
#'
wb.deg_pipeline <- function( counts,tpm,group,contrast,p.adj=0.05,FC=2,write = F){
  ipcounts <- counts
  iptpm <- tpm
  ipgroups <- group
  ipcontrast <- contrast
  ippadj <- p.adj
  ipFC <- FC


  #packages
  library(dplyr)
  library(DESeq2)
  library(ggpubr)
  library(edgeR)
  library(limma)
  library(data.table)

  #get contrast
  x=ipcontrast
  print(paste(x[1],'vs',x[2],sep = '.'))

  #get counts
  genes <- rownames(ipcounts)
  scounts <- apply(ipcounts,2,as.integer) %>% as.data.frame()
  rownames(scounts) <- genes
  stpm <- apply(iptpm,2,as.numeric) %>% as.data.frame()
  rownames(stpm) <- genes

  ###DESeq2
  condition<- factor(ipgroups,levels = x)
  coldata=data.frame(condition,row.names = colnames(scounts))
  #
  dds <- DESeqDataSetFromMatrix(scounts,colData = coldata,design = ~condition)
  dds <- DESeq(dds)
  #PCA for samples
  rlog_data <- rlog(dds, blind = T)
  p <- plotPCA(rlog_data,intgroup = "condition",returnData =F,ntop = 1000)
  ggexport(p,filename = paste(x[1],'vs',x[2],'pca.jpg',sep = '.'),res = 300,height = 5*300,width = 5*300)
  #
  rlog_cor <-rlog_data %>% assay() %>% cor()
  pheatmap::pheatmap(rlog_cor,filename =  paste(x[1],'vs',x[2],'corelation.heatmap.jpg',sep = '.' ))
  ####
  res <- results(dds,contrast = c('condition',x[1],x[2])) %>% as.data.frame()
  ###
  res$group <- paste(x[1],'vs',x[2],sep = '.')
  #
  mpadj=ippadj;myfc= log2(ipFC)
  res$sign <- apply(res,1,function(y){
    sign='none'
    if(  !is.na(y[6])  ){
      if( as.numeric(y[6]) < mpadj){
        if( abs(as.numeric(y[2])) > myfc ){
          if( as.numeric(y[2]) > 0  ){
            sign='increasing'
          }else if(  as.numeric(y[2]) < 0  ){
            sign='decreasing'
          }
        }
      }
    }
    return(sign)
  })
  #
  scounts_2 <- scounts
  stpm_2 <- stpm
  mynormcounts <- counts(dds, normalized = T)  %>% as.data.frame()
  colnames(mynormcounts) <- paste('normCounts',colnames(mynormcounts),sep = '.')
  colnames(scounts_2) <- paste('counts',colnames(scounts_2),sep = '.')
  colnames(stpm_2) <- paste('tpm',colnames(stpm_2),sep = '.')
  opres <- cbind(res,scounts_2)
  opres <- cbind(opres,mynormcounts)
  opres <- cbind(opres,stpm_2)
  opres$gene <- rownames(opres)
  opres <- dplyr::select(opres,gene,everything())
  opres[is.na(opres)] <- 'NA'
  if( write ){ fwrite(opres,file = paste(x[1],'vs',x[2],'deseq2.txt',sep = '.'),sep = '\t') }
  #
  sign_res <- opres[which(opres$sign != 'none'),]
  if( write ){ fwrite(sign_res,file = paste(x[1],'vs',x[2],'deseq2.sign.txt',sep = '.'),sep = '\t') }

  deseq_res <- opres

  ###edgeR
  groups <- ipgroups
  dgelist <- DGEList(counts = scounts,group = groups)
  mydesign <- model.matrix(~groups)
  rownames(mydesign) <- colnames(dgelist)
  dgelist <- calcNormFactors(dgelist, method = 'TMM')
  dge <- estimateDisp(dgelist, mydesign, robust = TRUE)
  res <- exactTest(dge,pair=c(x[2],x[1])) %>% as.data.frame()
  res$p.adj <- p.adjust(res$PValue, method ='BH')
  #
  res$group <- paste(x[1],'vs',x[2],sep = '.')
  #
  mpadj=ippadj;myfc= log2(ipFC)
  res$sign <- apply(res,1,function(y){
    sign='none'
    if(  !is.na(y[4])  ){
      if( as.numeric(y[4]) < mpadj){
        if( abs(as.numeric(y[1])) > myfc ){
          if( as.numeric(y[1]) > 0  ){
            sign='increasing'
          }else if(  as.numeric(y[1]) < 0  ){
            sign='decreasing'
          }
        }
      }
    }
    return(sign)
  })
  #
  scounts_2 <- scounts
  stpm_2 <- stpm
  colnames(scounts_2) <- paste('counts',colnames(scounts_2),sep = '.')
  colnames(stpm_2) <- paste('tpm',colnames(stpm_2),sep = '.')
  opres <- cbind(res,scounts_2)
  opres <- cbind(opres,stpm_2)
  opres$gene <- rownames(opres)
  opres <- dplyr::select(opres,gene,everything())
  opres[is.na(opres)] <- 'NA'
  if( write ){ fwrite(opres,file = paste(x[1],'vs',x[2],'edgeR.txt',sep = '.'),sep = '\t') }
  #
  sign_res <- opres[which(opres$sign != 'none'),]
  if( write ){ fwrite(sign_res,file = paste(x[1],'vs',x[2],'edgeR.sign.txt',sep = '.'),sep = '\t') }

  edger_res <- opres


  ###limma
  design <- model.matrix(~0+groups)
  contrast.matrix<-makeContrasts(contrasts=c(paste('groups',x,sep='',collapse = '-')),levels = design)

  v <- voom(scounts,design)
  fit <- lmFit(v,design)
  fit2 <- contrasts.fit(fit, contrast.matrix)
  fit3 <- eBayes(fit2)
  res <- topTable(fit3, sort.by = "none", number = Inf)
  #
  res$group <- paste(x[1],'vs',x[2],sep = '.')
  #
  mpadj=ippadj;myfc= log2(ipFC)
  res$sign <- apply(res,1,function(y){
    sign='none'
    if(  !is.na(y[5])  ){
      if( as.numeric(y[5]) < mpadj){
        if( abs(as.numeric(y[1])) > myfc ){
          if( as.numeric(y[1]) > 0  ){
            sign='increasing'
          }else if(  as.numeric(y[1]) < 0  ){
            sign='decreasing'
          }
        }
      }
    }
    return(sign)
  })
  #
  scounts_2 <- scounts
  stpm_2 <- stpm
  colnames(scounts_2) <- paste('counts',colnames(scounts_2),sep = '.')
  colnames(stpm_2) <- paste('tpm',colnames(stpm_2),sep = '.')
  opres <- cbind(res,scounts_2)
  opres <- cbind(opres,stpm_2)
  opres$gene <- rownames(opres)
  opres <- dplyr::select(opres,gene,everything())
  opres[is.na(opres)] <- 'NA'
  if( write ){ fwrite(opres,file = paste(x[1],'vs',x[2],'limma.txt',sep = '.'),sep = '\t') }
  #
  sign_res <- opres[which(opres$sign != 'none'),]
  if( write ){ fwrite(sign_res,file = paste(x[1],'vs',x[2],'limma.sign.txt',sep = '.'),sep = '\t') }

  limma_res <- opres

  ###limma tpm
  design <- model.matrix(~0+groups)
  contrast.matrix<-makeContrasts(contrasts=c(paste('groups',x,sep='',collapse = '-')),levels = design)

  fit <- lmFit(stpm,design)
  fit2 <- contrasts.fit(fit, contrast.matrix)
  fit3 <- eBayes(fit2)
  res <- topTable(fit3, sort.by = "none", number = Inf)
  #
  res$group <- paste(x[1],'vs',x[2],sep = '.')
  #
  mpadj=ippadj;myfc= log2(ipFC)
  res$sign <- apply(res,1,function(y){
    sign='none'
    if(  !is.na(y[5])  ){
      if( as.numeric(y[5]) < mpadj){
        if( abs(as.numeric(y[1])) > myfc ){
          if( as.numeric(y[1]) > 0  ){
            sign='increasing'
          }else if(  as.numeric(y[1]) < 0  ){
            sign='decreasing'
          }
        }
      }
    }
    return(sign)
  })
  #
  scounts_2 <- scounts
  stpm_2 <- stpm
  colnames(scounts_2) <- paste('counts',colnames(scounts_2),sep = '.')
  colnames(stpm_2) <- paste('tpm',colnames(stpm_2),sep = '.')
  opres <- cbind(res,scounts_2)
  opres <- cbind(opres,stpm_2)
  opres$gene <- rownames(opres)
  opres <- dplyr::select(opres,gene,everything())
  opres[is.na(opres)] <- 'NA'
  if( write ){ fwrite(opres,file = paste(x[1],'vs',x[2],'limma_tpm.txt',sep = '.'),sep = '\t') }
  #
  sign_res <- opres[which(opres$sign != 'none'),]
  if( write ){ fwrite(sign_res,file = paste(x[1],'vs',x[2],'limma_tpm.sign.txt',sep = '.'),sep = '\t') }

  limma_tpm_res <- opres

  ###limma log2(tpm+1)
  design <- model.matrix(~0+groups)
  contrast.matrix<-makeContrasts(contrasts=c(paste('groups',x,sep='',collapse = '-')),levels = design)

  fit <- lmFit(log2(stpm+1),design)
  fit2 <- contrasts.fit(fit, contrast.matrix)
  fit3 <- eBayes(fit2)
  res <- topTable(fit3, sort.by = "none", number = Inf)
  #
  res$group <- paste(x[1],'vs',x[2],sep = '.')
  #
  mpadj=ippadj;myfc= log2(ipFC)
  res$sign <- apply(res,1,function(y){
    sign='none'
    if(  !is.na(y[5])  ){
      if( as.numeric(y[5]) < mpadj){
        if( abs(as.numeric(y[1])) > myfc ){
          if( as.numeric(y[1]) > 0  ){
            sign='increasing'
          }else if(  as.numeric(y[1]) < 0  ){
            sign='decreasing'
          }
        }
      }
    }
    return(sign)
  })
  #
  scounts_2 <- scounts
  stpm_2 <- stpm
  colnames(scounts_2) <- paste('counts',colnames(scounts_2),sep = '.')
  colnames(stpm_2) <- paste('tpm',colnames(stpm_2),sep = '.')
  opres <- cbind(res,scounts_2)
  opres <- cbind(opres,stpm_2)
  opres$gene <- rownames(opres)
  opres <- dplyr::select(opres,gene,everything())
  opres[is.na(opres)] <- 'NA'
  if( write ){ fwrite(opres,file = paste(x[1],'vs',x[2],'limma_log_tpm.txt',sep = '.'),sep = '\t') }
  #
  sign_res <- opres[which(opres$sign != 'none'),]
  if( write ){ fwrite(sign_res,file = paste(x[1],'vs',x[2],'limma_log_tpm.sign.txt',sep = '.'),sep = '\t') }

  limma_log_tpm_res <- opres

  #
  oplist=list( deseq_res=deseq_res,edger_res=edger_res,limma_res=limma_res,
               limma_tpm_res=limma_tpm_res,limma_log_tpm_res=limma_log_tpm_res
  )
  return(oplist)
}
