library(dplyr)
library(ggplot2)
library(forcats)

# Read the angsd association output file
file = "Assignment_3/data/out_additive_F1.lrt0"
df <- read.table(file, header=TRUE)
clean_df <- filter(df, Frequency >= 0.1)
# save clean df as RDS object for easier usability

# Read the contig length file
bed_file = "Assignment_3/data/contig_length.bed"
contig_length = read.delim(bed_file, header = FALSE, quote = "", stringsAsFactors = FALSE, skip =1)
colnames(contig_length) = c("Contig", "Length")

# Rename the 'Chromosome' column in clean_df to Contig
clean_df <- rename(clean_df, Contig = Chromosome)

# Merge the two dataframes together
df_merged <- merge(clean_df, contig_length, by= "Contig", all.x=TRUE)

# Create a SNP count 
(SNP_count <- clean_df
  %>% group_by(Contig)
  %>% summarize(SNP_count = n())
)

# Merge SNP count into the df_merged
df_count <- merge(df_merged, SNP_count, by= "Contig", all.x=TRUE)

# Calculate the -log(P) 
df_final <- mutate(df_count, Neg_log_P = -log10(P))

# Plot the length vs. -log10(P)
plot1 <- (ggplot(df_final, aes(x=Length, y=Neg_log_P))
          + geom_point()
          + ggtitle("Length(integer) vs. -log10(P) of All Contigs")
)
print(plot1)

# From this plot, I would like to narrow down the points with -log10(P) > 5 
print(plot1 + geom_hline(yintercept = 5))

# Plot the length vs. -log10(P) for the SNPs with > 5 only 
df_sig <- df_final %>% filter(Neg_log_P >=5)

plot_sig <- (ggplot(df_sig, aes(x=Length, y=Neg_log_P))
             + geom_point()
             + ggtitle("Length(integer) vs. -log10(P) of Interesting Contigs")
)
print(plot_sig)

# Alternate the colors of the contigs to get a sense of how many contigs we're dealing with 
# 
# # assign in the df_sig a column that alternates 0 and 1 by contig. 
# sig_contigs <- data.frame(unique(df_sig$Contig)) #I'll need to sort them by length before assigning it flags
# sig_contigs <- rename(sig_contigs, Contig = unique.df_sig.contig.)
# 
# # Merge with the length table
# contig_len_merged <- merge(sig_contigs, contig_length, by= "Contig", all.x=TRUE) 
# 
# # Sort by Length
# contig_len_merged <- arrange(contig_len_merged, Length) # using arrange instead of order resets the index
# 
# # Get the indices/positions of the elements in the Contigs list
# indices = seq_along(contig_len_merged$Contig) 
# 
# # Assign them 0 or 1 for even vs. odd 
# odd <- contig_len_merged$Contig[indices %% 2 != 0] #defining even OR odd is sufficient 
# 
# # Create a Flag column with the designations
# df_sig$Flag <- ifelse(df_sig$Contig %in% odd, 1L, 0L)


# Now add another layer to the plot_sig that changes the color based on Flag
# plot_color <- (ggplot(df_sig, aes(x=Length, y=Neg_log_P, color = Flag))
#                + geom_point()
# )
# print(plot_color)

# The plot_color didn't yield a better visualizations so I'll try other alternatives - I kept the plot_color anyway but commented

# Alternative 1: using geom_point() with Length as factor where each tick/vertical line will represent a contig

# Order the Length in df_sig by increasing order- to maintain order when converting to factor
sorted_df_sig <- arrange(df_sig, Length)
str(sorted_df_sig)

# Convert the Length in the into factor, use forcats to maintain the order 
sorted_df_sig$Length <- as.character(sorted_df_sig$Length) #must convert Length to character first

sorted_df_sig <- sorted_df_sig %>%
  mutate(
    Length = fct_inorder(Length)
)

# Now repeat the plot_sig plot but now since Length is a factor ideally it'll look cleaner to distinguish the different contigs 
plot_sig_len <- (ggplot(sorted_df_sig, aes(x=Length, y=Neg_log_P))
             + geom_point()
             + ggtitle("Length(factor) vs. -log10(P)")
)
print(plot_sig_len + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Showcasing the points - which represent SNPs - for each contig with its Length as the x-axis allow us to see the distribution of
# 'interesting' SNPs on the 'interesting' contigs and their corresponding -log10(P) values

# Alternative 2:Plot the Contigs on the x axis and apply scale_size flag in ggplot to reflect size of the contig

# Plot it using Contigs on x-axis 
plot_sig_contig <- (ggplot(sorted_df_sig, aes(x=Contig, y=Neg_log_P))
               + geom_point()
               + ggtitle("Contig vs. -log10(P)")
)
print(plot_sig_contig + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# With this plot, the name of contigs we deem 'interesting' to explore their gene content can be pulled 

# Add a layer that changes the size of the point by size

# While technically it was producing the same plot, converting Length back to integer seemed sensible
sorted_df_sig$Length <- as.integer(as.character(sorted_df_sig$Length))
str(sorted_df_sig)


plot_sig_size <- (ggplot(sorted_df_sig, aes(x=Contig, y=Neg_log_P, size = Length))
                  + geom_point()
                  + ggtitle("Contig vs. -log10(P) with Length as guide")
                  
)

print(plot_sig_size + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      + guides(size = "none")) # to remove the legend for size


# From this plot, we can get a rough idea of the density of the SNPs in contigs 
# For example, contig_10157 is the only contig that is long and has two SNPs with relatively high -log10(P)


# Next steps would be repeating it for all contigs ~30,000 of them 
# The reason is to re-explore all contigs now with this visualization and judge (roughly)
# if any other contigs can be considered interesting enough to explore gene content
# This can be achieved using facet_wrap to meaningfully display such a large no. of contigs



