# Create a boxplot of the first gene in the expression matrix
boxplot(x[1, ] ~ p[, "Disease"], main = f[1, 
"symbol"])

# Load package
library(Biobase)

# Create ExpressionSet object
eset <- ExpressionSet(assayData = x,
                      phenoData = AnnotatedDataFrame(p),
                      featureData = AnnotatedDataFrame(f))

# View the number of features (rows) and samples (columns)
dim(eset)

# Subset to only include the first 10 samples (columns)
eset_sub <- eset[,1:10]

# Check the dimensions of the subset
dim(eset_sub)

# Create a boxplot of the 1000th gene in eset_sub
boxplot(exprs(eset_sub)[1000, ] ~ pData(eset_sub)[, "Disease"],
        main = fData(eset_sub)[1000, "symbol"])

# Create design matrix for leukemia study
design <- model.matrix(~Disease, data = pData(eset))

# Count the number of samples modeled by each coefficient
colSums(design)

# Load package
library(limma)

# Fit the model
fit <- lmFit(eset, design)

# Calculate the t-statistics
fit <- eBayes(fit)

# Summarize results
results <- decideTests(fit[, "Diseasestable"])
summary(results)

# Create design matrix with no intercept
design <- model.matrix(~0 + Disease, data = pData(eset))

# Count the number of samples modeled by each coefficient
colSums(design)

# Load package
library(limma)

# Create a contrasts matrix
cm <- makeContrasts(status = Diseaseprogres. - Diseasestable,
                    levels = design)

# View the contrasts matrix
cm

# Load package
library(limma)

# Fit the model
fit <- lmFit(eset, design)

# Fit the contrasts
fit2 <- contrasts.fit(fit, contrasts = cm)

# Calculate the t-statistics for the contrasts
fit2 <- eBayes(fit2)

# Summarize results
results <- decideTests(fit2)
summary(results)

# Create design matrix with no intercept
design <- model.matrix(~0 + oxygen, data = pData(eset))

# Count the number of samples modeled by each coefficient
colSums(design)

# Load package
library(limma)

# Create a contrasts matrix
cm <- makeContrasts(ox05vox01 = oxygenox05 - oxygenox01,
                    ox21vox01 = oxygenox21 - oxygenox01,
                    ox21vox05 = oxygenox21 - oxygenox05,
                    levels = design)

# View the contrasts matrix
cm

# Load package
library(limma)

# Fit the model
fit <- lmFit(eset, design)

# Fit the contrasts
fit2 <- contrasts.fit(fit, contrasts = cm)

# Calculate the t-statistics for the contrasts
fit2 <- eBayes(fit2)

# Summarize results
results <- decideTests(fit2)
summary(results)

# Create single variable
group <- with(pData(eset), paste(type, water, sep = "."))
group <- factor(group)

# Create design matrix with no intercept
design <- model.matrix(~0 + group)
colnames(design) <- levels(group)

# Count the number of samples modeled by each coefficient
colSums(design)

# Load package
library(limma)

# Create a contrasts matrix
cm <- makeContrasts(type_normal = nm6.normal - dn34.normal,
                    type_drought = nm6.drought - dn34.drought,
                    water_nm6 = nm6.drought - nm6.normal,
                    water_dn34 = dn34.drought - dn34.normal,
                    interaction = (nm6.drought - nm6.normal) - (dn34.drought - dn34.normal),
                    levels = design)

# View the contrasts matrix
cm

# Load package
library(limma)

# Fit the model
fit <- lmFit(eset, design)

# Fit the contrasts
fit2 <- contrasts.fit(fit, contrasts = cm)

# Calculate the t-statistics for the contrasts
fit2 <- eBayes(fit2)

# Summarize results
results <- decideTests(fit2)
summary(results)

library(limma)

# Create new ExpressionSet to store normalized data
eset_norm <- eset_raw

# View the distribution of the raw data
plotDensities(eset_norm, legend = FALSE)

# Log tranform
exprs(eset_norm) <- log(exprs(eset_norm))
plotDensities(eset_norm, legend = FALSE)

# Quantile normalize
exprs(eset_norm) <- normalizeBetweenArrays(exprs(eset_norm))
plotDensities(eset_norm, legend = FALSE)

library(limma)

# Create new ExpressionSet to store filtered data
eset <- eset_norm

# View the normalized gene expression levels
plotDensities(eset, legend = FALSE); abline(v = 5)

# Determine the genes with mean expression level greater than 5
keep <- rowMeans(exprs(eset)) > 5
sum(keep)

# Filter the genes
eset <- eset[keep,]
plotDensities(eset, legend = FALSE)

# Load package
library(limma)

# Plot principal components labeled by treatment
plotMDS(eset, labels = pData(eset)[, "treatment"], gene.selection = "common")

# Plot principal components labeled by batch
plotMDS(eset, labels = pData(eset)[, "batch"], gene.selection = "common")

# Load package
library(limma)

# Remove the batch effect
exprs(eset) <- removeBatchEffect(eset, batch = pData(eset)[,"batch"])

# Plot principal components labeled by treatment
plotMDS(eset, labels = pData(eset)[, "treatment"], gene.selection = "common")

# Plot principal components labeled by batch
plotMDS(eset, labels = pData(eset)[, "batch"], gene.selection = "common")

# Obtain the summary statistics for every gene
stats <- topTable(fit2, number = nrow(fit2), sort.by = "none")

# Plot a histogram of the p-values
hist(stats[,"P.Value"])

# Create a volcano plot. Highlight the top 5 genes
volcanoplot(fit2, highlight = 5, names = fit2$genes[, "symbol"])

# Extract the entrez gene IDs
entrez <- fit2$genes[, "entrez"]

# Test for enriched KEGG Pathways
enrich_kegg <- kegga(fit2, geneid = entrez, species = "Hs")

# View the top 20 enriched KEGG pathways
topKEGG(enrich_kegg, number = 20)

# Extract the entrez gene IDs
entrez <- fit2$genes[, "entrez"]

# Test for enriched GO categories
enrich_go <- goana(fit2[1:500, ], geneid = "entrez", species = "Hs")

# View the top 20 enriched GO Biological Processes
topGO(enrich_go, ontology = "BP", number = 20)

# Create a new ExpressionSet to store the processed data
eset <- eset_raw

# Log transform
exprs(eset) <- log(exprs(eset))
plotDensities(eset,  group = pData(eset)[, "genotype"], legend = "topright")

# Quantile normalize
exprs(eset) <- normalizeBetweenArrays(exprs(eset))
plotDensities(eset,  group = pData(eset)[, "genotype"], legend = "topright")

# Determine the genes with mean expression level greater than 0
keep <- rowMeans(exprs(eset)) > 0
sum(keep)

# Filter the genes
eset <- eset[keep, ]
plotDensities(eset, group = pData(eset)[, "genotype"], legend = "topright")

# Find the row which contains Top2b expression data
top2b <- which(fData(eset)[, "symbol"] == "Top2b")

# Plot Top2b expression versus genotype
boxplot(exprs(eset)[top2b, ] ~ pData(eset)[, "genotype"],
        main = fData(eset)[top2b, ])

# Plot principal components labeled by genotype
plotMDS(eset, labels = pData(eset)[, "genotype"], gene.selection = "common")

# Plot principal components labeled by treatment
plotMDS(eset, labels = pData(eset)[, "treatment"], gene.selection = "common")

# Create single variable
group <- with(pData(eset), paste(genotype, treatment, sep = "."))
group <- factor(group)

# Create design matrix with no intercept
design <- model.matrix(~0 + group)
colnames(design) <- levels(group)

# Count the number of samples modeled by each coefficient
colSums(design)

# Create a contrasts matrix
cm <- makeContrasts(dox_wt = wt.dox - wt.pbs,
                    dox_top2b = top2b.dox - top2b.pbs,
                    interaction = (top2b.dox - top2b.pbs) - (wt.dox - wt.pbs),
                    levels = design)

# View the contrasts matrix
cm

# Fit the model
fit <- lmFit(eset, design)

# Fit the contrasts
fit2 <- contrasts.fit(fit, contrasts = cm)

# Calculate the t-statistics for the contrasts
fit2 <- eBayes(fit2)

# Summarize results
results <- decideTests(fit2)
summary(results)

# Create a Venn diagram
vennDiagram(results)

# Obtain the summary statistics for the contrast dox_wt
stats_dox_wt <- topTable(fit2, coef = "dox_wt", number = nrow(fit2),
                         sort.by = "none")
# Obtain the summary statistics for the contrast dox_top2b
stats_dox_top2b <- topTable(fit2, coef = "dox_top2b", number = nrow(fit2),
                            sort.by = "none")
# Obtain the summary statistics for the contrast interaction
stats_interaction <- topTable(fit2, coef = "interaction", number = nrow(fit2),
                              sort.by = "none")

# Create histograms of the p-values for each contrast
hist(stats_dox_wt[, "P.Value"])
hist(stats_dox_top2b[, "P.Value"])
hist(stats_interaction[, "P.Value"])

# Extract the gene symbols
gene_symbols <- fit2$genes[, "symbol"]

# Create a volcano plot for the contrast dox_wt
volcanoplot(fit2, coef = "dox_wt", highlight = 5, names = gene_symbols)

# Create a volcano plot for the contrast dox_top2b
volcanoplot(fit2, coef = "dox_top2b", highlight = 5, names = gene_symbols)

# Create a volcano plot for the contrast interaction
volcanoplot(fit2, coef = "interaction", highlight = 5, names = gene_symbols)

# Extract the entrez gene IDs
entrez <- fit2$genes[, "entrez"]

# Test for enriched KEGG Pathways for contrast dox_wt
enrich_dox_wt <- kegga(fit2, coef = "dox_wt", geneid = entrez, species = "Mm")

# View the top 5 enriched KEGG pathways
topKEGG(enrich_dox_wt, number = 5)

# Test for enriched KEGG Pathways for contrast interaction
enrich_interaction <- kegga(fit2, coef = "interaction", geneid = entrez, species = "Mm")

# View the top 5 enriched KEGG pathways
topKEGG(enrich_interaction, number = 5)

