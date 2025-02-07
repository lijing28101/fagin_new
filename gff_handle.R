get_gff_from_txdb <- function(x, ...){
  synder::as_gff(
    x,
    id=GenomicRanges::mcols(x)$tx_name,
    ...
  )
}


MakeGI <- function(starts, stops, scaffolds, strands=NULL, metadata=NULL, seqinfo=NULL){
  if(is.null(strands)){
    strands=rep("*", length(starts))
  } else {
    strands <- gsub('\\.', '*', strands)
  }
  g <- GenomicRanges::GRanges(
    seqnames = scaffolds,
    ranges   = IRanges::IRanges(starts, stops),
    strand   = strands,
    seqinfo  = seqinfo
  )
  if(!is.null(metadata)){
    GenomicRanges::mcols(g) <- metadata
  }
  g
}


load_gene_models <- function(file, seqinfo_=NULL){
  
  raw_gff_ <-rtracklayer::readGFF(file) %>% {
    .$Parent <- unlist(lapply(.$Parent,function(x) if(identical(x,character(0))) NA else x))
    .$Parent[.$Parent=="-"] <- NA
    .
  } %>% {
    
    if(!("Name" %in% colnames(.))){
      .$Name <- .$ID
    }
    
    if(!("ID" %in% colnames(.))){
      .$ID <- .$name
    }
    .
    
  } %>% {
    
    "Load GFF into a GenomicRanges object"
    
    gi <- MakeGI(
      starts    = .$start,
      stops     = .$end,
      scaffolds = as.character(.$seqid),
      strands   = .$strand,
      metadata  = .[,c('phase', 'ID','Name','Parent')]
    )
    
    GenomicRanges::mcols(gi)$type <- as.character(.$type)
    
    gi
    
  } %>% {
    
    "Set the Seqinfo"
    
    # Will fail loadly if any sequence in `.` is not in the seqinfo file
    # This step is needed since seqinfo() will not create new levels.
    si <- seqinfo_
    GenomeInfoDb::seqlevels(.) <- unique(GenomeInfoDb::seqnames(si))
    GenomicRanges::seqinfo(.) <- si
    .
    
  } %>% {
    
    "
    From the GenomicRanges object, create a transcript database as a TxDb object
    "
    
    meta <- GenomicRanges::mcols(.)
    
    # ************************* Abominable hack!!! ****************************
    # The TxDb objects do not store phase, see my issue report:
    # https://support.bioconductor.org/p/101245/
    # To get around this, I encode the phase in the CDS Name metadata vector.
    # Then I can extract it later when I need it. This is, of course, an
    # utterly sinful thing to do. 
    GenomicRanges::mcols(.)$Name <-
      ifelse(meta$type == "CDS", as.character(meta$phase), meta$Name)
    # *************************************************************************
    
    # NOTE: This cannot just be `is_trans <- meta$type == "mRNA"` because some
    # exons are recorded as direct children of a "gene" feature. So I list as a
    # transcript anything that is the parent of an exon or CDS.
    is_trans <- meta$ID %in% meta$Parent[meta$type %in% c("exon", "CDS")]
    
    GenomicRanges::mcols(.)$type = ifelse(is_trans, "mRNA", meta$type)
    
    # Stop if any mRNA or gene IDs are missing
    missing_IDs <- is.na(meta$ID) & meta$type %in% c("mRNA", "gene")
    if(any(missing_IDs)){
      gff_stop(sprintf(
        "%s of %s mRNAs or genes are missing an ID",
        sum(missing_IDs),
        length(missing_IDs)
      ))
    }
    
    gff_stop <- function(msg) stop("In GFF: ", msg)
    gff_warning <- function(msg) warning("In GFF: ", msg)
    
    # Stop if any mRNA IDs are duplicated
    duplicants <- meta$ID[duplicated(.$ID) & meta$type == "mRNA"]
    if(length(duplicants) > 0){
      msg <- "mRNA IDs are not unique. The following IDs map to multiple entries: [%s]"
      gff_stop(sprintf(msg, paste(duplicants, collapse=", ")))
    }
    
    # Warn if any gene IDs are duplicated
    duplicants <- meta$ID[duplicated(.$ID) & meta$type == "gene"]
    if(length(duplicants) > 0){
      msg <- "gene IDs are not unique. This may by OK, since mRNA, not gene,
      IDs are used as unique labels. The following IDs map to multiple entries:
      [%s]"
      gff_warning(sprintf(msg, paste(duplicants, collapse=", ")))
    }
    
    GenomicFeatures::makeTxDbFromGRanges(.)
    
  }
}


