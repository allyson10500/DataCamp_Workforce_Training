# Load the BiocManager package
library(BiocManager)

# Explicitly check the Bioconductor version
version()

# Load the BSgenome package
library(BSgenome)

# Check the versions of the packages loaded in the session
sessionInfo()

showClass("BSgenome")

# Investigate the a_genome using show()
show(a_genome)

# Investigate the a_genome using show()
show(a_genome)

# Investigate some other accesors 
organism(a_genome)
provider(a_genome)
seqinfo(a_genome)

# Load the yeast genome
library(BSgenome.Scerevisiae.UCSC.sacCer3)

# Assign data to the yeastGenome object
yeastGenome <- BSgenome.Scerevisiae.UCSC.sacCer3

# Get the head of seqnames and tail of seqlengths for yeastGenome
head(seqnames(yeastGenome))
tail(seqlengths(yeastGenome))

# Print chromosome M, alias chrM
print(yeastGenome$chrM)

# Count characters of the chrM sequence
nchar(yeastGenome$chrM)

# Load the yeast genome
library(BSgenome.Scerevisiae.UCSC.sacCer3)

# Assign data to the yeastGenome object
yeastGenome <- BSgenome.Scerevisiae.UCSC.sacCer3

# Get the first 30 bases of chrM
print(head(yeastGenome$chrM, 30))

available.genomes()

# Load packages
library(Biostrings)

# Check the alphabet of the zikaVirus
alphabet(zikaVirus)

# Check the alphabetFrequency of the zikaVirus
alphabetFrequency(zikaVirus)

# Check alphabet of the zikaVirus using baseOnly = TRUE
alphabet(zikaVirus, baseOnly = TRUE)

class(zikaVirus)

# Unlist the set, select the first 21 letters, and assign to dna_seq
dna_seq <- subseq(unlist(zikaVirus), end = 21)
dna_seq

# Transcribe dna_seq into an RNAString object and print it
rna_seq <- RNAString(dna_seq) 
rna_seq

# Translate rna_seq into an AAString object and print it
aa_seq <- translate(rna_seq)
aa_seq

# Unlist the set, select the first 21 letters, and assign to dna_seq
dna_seq <- subseq(unlist(zikaVirus), end = 21)
dna_seq

# Transcribe and translate dna_seq into an AAString object and print it
aa_seq <- translate(dna_seq)
aa_seq

# Create zikv with one collated sequence using zikaVirus
zikv <- unlist(zikaVirus)

# Check the length of zikaVirus and zikv
length(zikaVirus)
length(zikv)

# Check the width of zikaVirus
width(zikaVirus)

# Subset zikv to only the first 30 bases
subZikv <- subseq(zikv, end = 30)
subZikv

subseq(zikaSet, 
        start = c(20, 40, 2), 
        end = c(50, 45, 22)
     )

# Reverse the zikv sequence
reverse(zikv)

# Complement the zikv sequence
complement(zikv)

# Reverse complement the zikv sequence
reverseComplement(zikv)

# Translate the zikv sequence
translate(zikv)

# For Sets
vmatchPattern(pattern = "ACATGGGCCTACCATGGGAG", 
              subject = zikaVirus, max.mismatch = 1)
# For single sequences
matchPattern(pattern = "ACATGGGCCTACCATGGGAG", 
              subject = zikv, max.mismatch = 1)

# Find palindromes in zikv
findPalindromes(zikv)

# Print rnaframesZikaSet
rnaframesZikaSet

# Translate rnaframesZikaSet
AAzika6F <- translate(rnaframesZikaSet)
AAzika6F

# Count NS5 protein matches in AAzika6F, allowing 15 mismatches
vcountPattern(pattern = NS5, subject = AAzika6F, max.mismatch = 15)

# Subset the frame that contains the match from AAzika6F
selectedSet <- AAzika6F[3] 
  
# Convert selectedSet into a single sequence
selectedSeq <- unlist(selectedSet)

# Use vmatchPattern() with the set
vmatchPattern(pattern = ns5, subject = selectedSet, max.mismatch = 15)

# Use matchPattern() with the single sequence
matchPattern(pattern = ns5, subject = selectedSeq, max.mismatch = 15)

# Load IRanges package
library(IRanges)

# IRnum1: start - vector 1 through 5, end - 100 
IRnum1 <- IRanges(start = 1:5, end = 100)

# IRnum2: end - 100, width - 89 and 10
IRnum2 <- IRanges(end = 100, width = c(89, 10))

# IRlog1: start = Rle(c(F, T, T, T, F, T, T, T))
IRlog1 <- IRanges(start = Rle(c(F, T, T, T, F, T, T, T)))

# Print objects in a list
print(list(IRnum1 = IRnum1, IRnum2 = IRnum2, IRlog1 = IRlog1))

# Create the first sequence seq_1
seq_1 <- IRanges(start = 10, end = 37)

# Create the second sequence seq_2
seq_2 <- IRanges(start = c(5, 35, 50),
                 end = c(12, 39, 61),
                 names = LETTERS[1:3])

# Check the width of seq_1 and seq_2
width(seq_1)
width(seq_2)

# Create the first sequence seq_1
seq_1 <- IRanges(start = 10, end = 37)

# Create the second sequence seq_2
seq_2 <- IRanges(start = c(5, 35, 50),
                 end = c(12, 39, 61),
                 names = LETTERS[1:3])

# Check the width of seq_1 and seq_2
lengths(seq_1)
lengths(seq_2)

# Load GenomicRanges package
library(GenomicRanges)

# Print seq_intervals      
seq_intervals

# Create myGR
myGR <- as(seq_intervals, "GRanges")

# Print myGR
myGR

# Load GenomicRanges
library(GenomicRanges)

# Print the seqinfo of myGR
seqinfo(myGR)

# Check the metadata
mcols(myGR)

# Load human reference genome hg38
library(TxDb.Hsapiens.UCSC.hg38.knownGene)

# Assign hg38 to hg, then print it
hg <- TxDb.Hsapiens.UCSC.hg38.knownGene
hg

# Extract all the genes in chromosome X as hg_chrXg, then print it
hg_chrXg <- genes(hg, filter = list(tx_chrom = c("chrX")))
hg_chrXg

# Extract all positive stranded genes in chromosome X, assign to hg_chrXgp, then sort it
hg_chrXgp <- genes(hg, filter = list(tx_chrom = c("chrX"), tx_strand = "+"))
sort(hg_chrXgp)

ranges(ABCD1)

overlapsAny(ABCD1, hg_chrX)

# Store the overlapping range in rangefound
rangefound <- subsetByOverlaps(hg_chrX, ABCD1)

# Print names of rangefound
names(rangefound)

# Print the gene of interest 
ABCD1

# Print rangefound
rangefound

# Load the human transcripts DB to hg
library(TxDb.Hsapiens.UCSC.hg38.knownGene)
hg <- TxDb.Hsapiens.UCSC.hg38.knownGene

# Prefilter chromosome X "chrX" using seqlevels()
seqlevels(hg) <- c("chrX")

# Get all transcripts by gene and print it
hg_chrXt <- transcriptsBy(hg, by = "gene")
hg_chrXt

# Select gene `215` from the hg_chrXt
hg_chrXt$`215`

# Unlist hg_ChrX and save result as myGR
myGR <- unlist(hg_ChrX)

# Compare classes of hg_ChrX and myGR
class(hg_ChrX)
class(myGR)

# Compare length of hg_ChrX and myGR
length(hg_ChrX)
length(myGR)

# Load ShortRead
library(ShortRead)

# Print fqsample
fqsample

# Check class of fqsample
class(fqsample)

# Check class sread fqsample
class(sread(fqsample))

# Check ids of fqsample
id(fqsample)

# Load ShortRead
library(ShortRead)

# Set a seed for sampling
set.seed(1234)

# Use FastqSampler with f and select 100 reads
fs <- FastqSampler(con = f, n = 100)

# Generate new sample yield
my_sample <- yield(fs)

# Print my_sample
my_sample

# load ShortRead
library(ShortRead)

# Check quality
quality(fqsample)

# Check encoding of quality
encoding(quality(fqsample))

# Check baseQuality
qaSummary[["baseQuality"]]

# Glimpse nucByCycle
glimpse(nucByCycle)

# Create a line plot of cycle vs. count
nucByCycle %>% 
  # Gather the nucleotide letters in alphabet and get a new count column
  pivot_longer(-cycle, names_to = "alphabet", values_to = "count") %>% 
  ggplot(aes(x = cycle, y =  count, color = alphabet)) +
  geom_line(size = 0.5 ) +
  labs(y = "Frequency") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

# Load package ShortRead
library(ShortRead)

# Check class of fqsample
class(fqsample)

# Filter reads into selectedReads using myStartFilter
selectedReads <- fqsample[myStartFilter(fqsample)]

# Check class of selectedReads
class(selectedReads)

# Check detail of selectedReads
detail(selectedReads)

# Check reads of fqsample
sread(fqsample)

# Create myFil using polynFilter
myFil <- polynFilter(threshold = 3, nuc = c("A"))

# Check myFil
myFil

# Apply your filter to fqsample 
filterCondition <- myFil(fqsample)

# Use myFil with fqsample
filteredSequences <- fqsample[filterCondition]

# Check reads of filteredSequences
sread(filteredSequences)

# Load package Rqc
library(Rqc)

# Average per cycle quality plot
rqcCycleAverageQualityPlot(qa)

# Average per cycle quality plot with white background
rqcCycleAverageQualityPlot(qa) + theme_minimal()

# Read quality plot with white background
rqcReadQualityPlot(qa) + theme_minimal()













