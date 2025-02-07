load_species <- function(species_name, con){
  genomeDB <- load_dna(con@input@fna[[species_name]])
  summary_genome <- summarize_dna(genomeDB)
  genomeSeq <- convert_FaFile_to_XStringSet(genomeDB)
  seqinfo <- make_seqinfo(genomeSeq, species_name)
  nstring <- derive_nstring(genomeSeq)
  summary_nstring <- summarize_nstring(nstring)
  
  gffDB <- load_gene_models(file=con@input@gff[[species_name]], seqinfo_=seqinfo)
  summary_gff <- summarize_gff(gffDB)
  mRNA <- GenomicFeatures::transcripts(gffDB) %>% 
    get_gff_from_txdb(x=., seqinfo_=seqinfo, type="mRNA")
  CDS <- GenomicFeatures::cds(gffDB) %>% 
    get_gff_from_txdb(x=., seqinfo_=seqinfo, type="CDS")
  exon <- GenomicFeatures::exons(gffDB) %>% 
    get_gff_from_txdb(x=., seqinfo_=seqinfo, type="exon")
  
  orfgff <- derive_genomic_ORFs(genomeSeq, con) %>% 
    convert_GRanges_to_SynderGFF
  summary_orfgff <- summarize_granges(orfgff)
  orffaa <- extract_with_complements(dna=genomeDB, gff=orfgff) %>% 
    fuzzy_translate(., label=species_name)
  summary_orffaa <- summarize_faa(orffaa)
  
  cdsRangeList <- GenomicFeatures::cdsBy(gffDB, by="tx", use.names=TRUE)
  aa_model_phase <- extract_phase_from_GRangesList(cdsRangeList, label=species_name)
  check_for_incomplete_models(aa_model_phase, label=species_name)
  cdsRanges <- trim_CDS_with_non_zero_phase(cdsRangeList, label=species_name) %>%
    filter_with_warning__unnamed_entries(label=species_name)
  faa <- GenomicFeatures::extractTranscriptSeqs(x=genomeDB, transcripts=cdsRanges) %>%
    Biostrings::translate(if.fuzzy.codon="solve") %>% 
    filter_with_warnings__zero_length_proteins(label=species_name)
  summary_aa <- summarize_faa(faa)
  check_for_internal_stops(summary_aa,label=species_name)
  summary_phase <- summarize_phase(phases = aa_model_phase, aa = faa)
  
  transcriptomeDB <- GenomicFeatures::exonsBy(gffDB, by="tx", use.names=TRUE) %>%
    get_trans_dna(x=genomeDB, transcripts=., species_name=species_name)
  summary_transfna <- summarize_dna(transcriptomeDB)
  
  transcriptomeSeq <- convert_FaFile_to_XStringSet(transcriptomeDB)
  transorfgff <- transcriptomeSeq %>%
    derive_transcript_ORFs(con) %>%
    convert_GRanges_to_SynderGFF
  summary_transorfgff <- summarize_granges(transorfgff)
  check_protein_transcript_match(aa_summary=summary_aa, trans_summary=summary_transfna)
  transorfaa <- extract_with_complements(dna=transcriptomeDB,gff=transorfgff) %>%
    fuzzy_translate(label=species_name)
  summary_transorfaa <- summarize_faa(transorfaa)
  
  out <- list(seqinfo=seqinfo, mRNA=mRNA, nstring=nstring, orfgff=orfgff, 
            faa=faa, orffaa=orffaa, transorfaa=transorfaa, transorfgff=transorfgff,
            transcriptomeSeq=transcriptomeSeq, genomeDB=genomeDB, 
            CDS=CDS, exon=exon, mRNA=mRNA, 
            # Belows are object will not be used in future
            summary_aa=summary_aa, summary_nstring=summary_nstring, summary_gff=summary_gff, 
            summary_genome=summary_genome)
  saveRDS(out,paste0(con@archive,"/",species_name,"_data.rds"))
  out
}

