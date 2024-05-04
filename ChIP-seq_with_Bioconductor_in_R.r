# Print the 'reads' object to obtain a summary of the data
print(reads)

# Get the *start* position of the first read
start_first <- start(reads)[1]

# Get the *end* position of the last read
end_last <- end(reads)[length(reads)]

# Compute the number of reads covering each position in the selected region
cvg <- coverage(reads)

# Print a summary of the 'peaks' object
print(peaks)

# Use the score function to find the index of the highest scoring peak
max_idx <- which.max(score(peaks))

# Extract the genomic coordinates of the highest scoring peak using the `chrom` and `ranges` functions
max_peak_chrom <- chrom(peaks)[max_idx]
max_peak_range <- ranges(peaks)[max_idx]

# Create a vector of colors to label groups (there are 2 samples per group)
group <- c(primary = rep("blue", 2), TURP = rep("red", 2))

# Plot the sample correlation matrix `sample_cor` as a heat map
# Use the group colors to label the rows and columns of the heat map
heatmap(sample_cor, ColSideColors = group, RowSideColors = group,
        cexCol = 0.75, cexRow = 0.75, symm = TRUE)

# Create a heat map of peak read counts
# Use the group colors to label the columns of the heat map
heatmap(read_counts, ColSideColors = group, labRow = "", cexCol = 0.75)

# Take a look at the full gene sets
print(ar_sets)

# Visualise the overlap between the two groups using the `upset` function
upset(fromList(ar_sets))

# Print the genes with differential binding
print(db_sets)

# Visualise the overlap of differentially bound peaks between the two groups using the `upset` function
upset(fromList(db_sets))

# Load reads form chr20_bam file
reads <- readGAlignments(chr20_bam)

# Create a `BamViews` object for the range 29805000 - 29820000 on chromosome 20
bam_views <- BamViews(chr20_bam, bamRanges=GRanges("chr20", IRanges(start=29805000, end=29820000)))

# Load only the reads in that view
reads_sub <- readGAlignments(bam_views)

# Inspect the `reads_sub` object
str(reads_sub)

# Load peak calls from chr20_peaks
peaks <- import.bed(chr20_peaks)

# Create a BamViews object
bam_views <- BamViews(chr20_bam, bamRanges=peaks)

# Load the reads
reads <- readGAlignments(bam_views)

# Create annotation track
peak_track <- AnnotationTrack(peak_calls, name="Peaks")

# Create data track
cover_track <- DataTrack(cover_ranges, window=10500, type="polygon", name="Coverage",
                         fill.mountain=c("lighgrey", "lightgrey"), col.mountain="grey")

# Produce plot
plotTracks(list(ideogram, cover_track, peak_track, GenomeAxisTrack()), chromosome="chr20", from=start_pos, to=end_pos)

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
tx <- GeneRegionTrack(TxDb.Hsapiens.UCSC.hg19.knownGene, name="Genes")
plotTracks(list(ideogram, cover_track,peak_track, tx, 
           GenomeAxisTrack()), chromosome="chr20", 
           from = start_pos, to=end_pos)

# Find all overlaps between peaks and blacklisted regions
blacklisted <- findOverlaps(peaks, blacklist.hg19, type="within")

# Create a plot to display read coverage together with peak calls and blacklisted regions in the selected region
cover_track <- DataTrack(cover, window=10500, type="polygon", name="Coverage",
                         fill.mountain=c("lighgrey", "lightgrey"), col.mountain="grey")

# Calculate peak_track and region_track, plot plotTracks
peak_track <- AnnotationTrack(peaks, name="Peaks", fill="orange")
region_track <- AnnotationTrack(region, name="Blacklist")
plotTracks(list(ideogram, cover_track, peak_track, region_track, GenomeAxisTrack()),
           chromosome="chr21", from=start(region)-1000, to=end(region)+1000)

# Remove all blacklisted peaks
clean_peaks <- peaks[-from(blacklisted)]

# Load reads with mapping qualities by requesting the "mapq" entries
reads <- readGAlignments(bam_file, param=ScanBamParam(what="mapq"))

# Identify good quality alignments
high_mapq <- mcols(reads)$mapq >= 20

# Examine mapping quality distribution for high and low quality alignments
boxplot(mcols(reads)$mapq ~ high_mapq, xlab="good quality alignments", ylab="mapping quality")

# Remove low quality alignments
reads_good <- subset(reads, high_mapq)

# Extend reads to the average fragment length of 183 bp
reads_ext <- resize(reads_gr, width=183)

# Compute coverage
cover <- coverage(reads_gr)

# Prepare read counts for plotting by organising them in data frames
peak_scores <- data.frame(source="peaks", fragments=peak_bins$score)
bl_scores <- data.frame(source="blacklist", fragments=bl_bins$score)
bkg_scores <- data.frame(source="background", fragments=bkg_bins$score)
scores <- rbind(peak_scores, bl_scores, bkg_scores)

# Create a boxplot of the read counts by bin type
ggplot(scores, aes(y=fragments, x=source)) + geom_boxplot()

# Compute the pairwise distances between samples using `dist`
cover_dist <- dist(t(cover))

# Use `hclust()` to create a dendrogram from the distance matrix
cover_dendro <- hclust(cover_dist)

# Plot the dendrogram
plot(cover_dendro)

# Print the `peaks` object
print(peaks)

# Obtain the coordinates of the merged peaks
merged_peaks <- peaks$merged

# Extract the number of peaks present in the data
peak_count <- nrow(merged_peaks)

# Create a heatmap using the `dba.plotHeatmap()` function
dba.plotHeatmap(peaks, maxSites = peak_count, correlations = FALSE)

#As you have seen in the video, you have to create a set of consensus peak calls before you can test for differential binding.
ar_counts <- dba.count(ar_peaks, summits=200)

# Examine the ar_binding object
print(ar_binding)

# Identify the category corresponding to the tumor type contrast
contrast <- DBA_CONDITION

# Establish the contrast to compare the two tumor types
dba_peaks <- dba.contrast(ar_binding, categories=contrast, minMembers=2)

# Examine the dba_peaks object to confirm that the contrast has been added
print(dba_peaks)

# Examine the `ar_binding` object to confirm that it contains the required contrast
print(ar_binding)

# Run the differential binding analysis
ar_diff <- dba.analyze(ar_binding)

# Examine the result
print(ar_diff)

# Create a PCA plot using all peaks
dba.plotPCA(ar_diff, DBA_CONDITION)

# Create a PCA plot using only differentially bound peaks
dba.plotPCA(ar_diff, DBA_CONDITION, contrast = 1)

# Create a heatmap using all peaks
dba.plotHeatmap(ar_diff, DBA_CONDITION, correlations = FALSE, maxSites = 440)

# Create a heatmap using only differentially bound peaks
dba.plotHeatmap(ar_diff, DBA_CONDITION, contrast=1, correlations = FALSE)

# Create an MA plot
dba.plotMA(ar_diff)

# Create a volcano plot
dba.plotVolcano(ar_diff)

# Create a box plot of the peak intensities
compare_groups <- dba.plotBox(ar_diff, notch=FALSE)

# Inspect the returned p-values
print(compare_groups)

# Extract peaks from ChIPQCexperiment object
peak_calls <- peaks(ar_calls)

# Only keep samples that passed QC
peak_passed <- peak_calls[qc_pass]

# Find overlaps between peak sets
peaks_combined <- findOverlapsOfPeaks(peak_passed[[1]], peak_passed[[2]], peak_passed[[3]], peak_passed[[4]], maxgap=50)

# Examine merged peak set
print(peaks_combined$mergedPeaks)

# Obtain gene symbols
gene_symbol <- select(org.Hs.eg.db, keys=human_genes$gene_id, columns="SYMBOL", keytype="ENTREZID")

# Examine the structure of the returned annotations
str(gene_symbol)

# Add gene symbols to gene coordinates
human_genes$symbol <- gene_symbol$SYMBOL

# Examine output
print(human_genes)

# Annotate peaks with closest gene
peak_anno <- annoPeaks(peaks_merged, human_genes, bindingType="startSite", bindingRegion=c(-5000,5000))

# How many peaks were found close to genes?
length(peak_anno)

# Where are peaks located relative to genes?
table(peak_anno$insideFeature)

# Create Venn diagram
dba.plotVenn(ar_diff, mask=1:4)

# Convert the matrix of called peaks into a data frame
called_peaks <- as.data.frame(ar_diff$called)

# Create UpSet plot
upset(called_peaks, keep.order = TRUE, sets=colnames(ar_diff$called), order.by="freq")

# Plot distribution of distances between peaks and transcription start sites
plot_dist_to_tss(peaks, genome = "hg19")

# Plot relationship between gene length and presence of peaks
plot_chipenrich_spline(peaks, genome = "hg19", mappability=50)

# Select all peaks with higher intensity in treatment resistant samples
turp_peaks <- peaks_binding[, "GSM1598218"] + peaks_binding[, "GSM1598219"] < peaks_binding[, "GSM1598223"] + peaks_binding[, "GSM1598225"]

# Run enrichment analysis
enrich_turp <- chipenrich(peaks_comb[turp_peaks, ], genome="hg19", 
                          genesets = "hallmark",out_name = NULL, 
                          locusdef = "nearest_tss", qc_plots=FALSE)

# Print the results of the analysis
print(enrich_turp$results)

# Examine the top gene sets
head(enrich_primary$results)

# Extract the gene IDs for the top ranking set
genes <- enrich_primary$results$Geneset.Peak.Genes[1]

# Split gene IDs into a vector
gene_ids <- strsplit(genes, ', ')[[1]]

# Convert gene IDs to gene symbols
gene_symbol <- select(org.Hs.eg.db, keys=gene_ids, columns="SYMBOL", keytype="ENTREZID")

# Print the result
print(gene_symbol)

# This is the base URL for all KEGG pathways
base_url <- "https://www.kegg.jp/pathway/"

# Add pathway ID to URL
path_url <- paste0(base_url, top_path)

# Collapse gene IDs into selection string
gene_select <- paste(genes, collapse="+")

# Add gene IDs to URL
path_url <- paste(path_url, gene_select, sep="+")

