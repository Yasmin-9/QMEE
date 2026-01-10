library(dplyr)
getwd()
# Load the blast output text file
file = "Assignment 1/data/blast_laevis_long_cleaned.txt"
df = read.delim(file, header = FALSE, quote = "", stringsAsFactors = FALSE)

head(df)

# Add header to df with appropriate col names
colnames(df) = c("qseqid", "sseqid", "pident", "length", "mismatch", "gapopen", "evalue", "bitscore")
head(df)
str(df)

# Use RegEx to separate the gene name from the qseqid column - perhaps clean it up a little bit
# so it only contains the gene name itself
library(stringr)
df[c("gene1", "gene2", "gene3", "gene4", "gene5")] <- str_split_fixed(df$qseqid, '_', 5)
str(df)

# Drop gene columns other than gene3 and qseqid column
df[, c("qseqid", "gene1", "gene2", "gene4", "gene5")] <- list(NULL)
str(df)

# Reorder columns 
df <- df[c("gene3", "sseqid", "pident", "length", "mismatch", "gapopen", "evalue", "bitscore")]

# Change column names
colnames(df) = c("gene_name", "contig", "pident", "length", "mismatch", "gapopen", "evalue", "bitscore")
str(df)

colnames(df)
# Filter for entries >=95 pident (percent identity) or evalue <= 1e-5
df <- filter(df, pident >= 95 & evalue <= 1e-5)
nrow(df) # check how many observations now


# Calculate metrics for each gene (min/median/max bitscores and min evalue):

# min bit scores
min_bitscore_by_gene <- summarise(group_by(df, gene_name),bitscore_min = min(bitscore))
head(min_bitscore_by_gene)
nrow(min_bitscore_by_gene)
n_distinct(df$gene_name) # confirm that the groupby was performed correctly  by comparing these two numbers 

# max bitscore 
max_bitscore_by_gene <- summarise(group_by(df, gene_name),bitscore_max = max(bitscore))
nrow(max_bitscore_by_gene)

# median bitscore
median_bitscore_by_gene <- summarise(group_by(df, gene_name),bitscore_median = median(bitscore))
nrow(median_bitscore_by_gene)

# min evalue
min_evalue_by_gene <- summarise(group_by(df, gene_name),evalue_min = min(evalue))
nrow(min_evalue_by_gene)

# Create a summary gene metrics table for easier merging later if needed
gene_metrics <- merge(min_bitscore_by_gene, max_bitscore_by_gene, by = "gene_name", all.x=TRUE)
gene_metrics <- merge(gene_metrics, median_bitscore_by_gene, by = "gene_name", all.x=TRUE)
gene_metrics <- merge(gene_metrics, min_evalue_by_gene, by = "gene_name", all.x=TRUE)
head(gene_metrics)

# Load the contig length table 
bed_file = "Assignment 1/data/contig_length.bed"
contig_length = read.delim(bed_file, header = FALSE, quote = "", stringsAsFactors = FALSE, skip =1)
colnames(contig_length) = c("contig", "con_length_mb")

# Merge the contig length on the contig col 
df_merged <- merge(df, contig_length, by= "contig", all.x=TRUE)
head(df_merged)

# Calculate density of genes for each contig:

# calculate the unique gene count per contig
gene_count_contig <- summarise(group_by(df_merged, contig), gene_count=n_distinct(gene_name))
head(gene_count_contig)

# create contig stats table 
contig_stats <- merge(gene_count_contig, contig_length, by = "contig")
contig_stats$gene_density <- contig_stats$gene_count * 1e6 / contig_stats$con_length #do the length in Mb 
head(contig_stats)

# Plot the density against the length
library(ggplot2)
plot <- ggplot(contig_stats, aes(x=con_length_mb, y=gene_density)) + geom_point() + scale_x_log10() + labs (x = "Log(contig length) in Mb", y= "Gene Density",title = " Gene density vs. contig length")
print(plot)
# The outliers on the plot highligh high gene density (compred to their length)
# Extracting such outlier contigs and exploring whether other tests and data types (such as associations tests or heterozygosity ratios) point towards the same contigs 

