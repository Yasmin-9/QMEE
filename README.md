# QMEE_2026
BIO 708 class repo

# Assignment 1 
For this week's assignment, I will use the output of a blast query of Xenopus laevis coding sequences against Xenopus longipes whole genome (organized in contigs)

The biological relevance of it is that I'd like to explore the gene content from the laevis query of the longipes contigs. This is one of the exploratory methods to help candidate contigs for sex-linkage.

BMB: can you be more specific about "explore"? Which contigs are most interesting? High gene density?

Calculations to perform: The final goal of the computations is to calculate the gene density of the contigs. Several steps are needed to achieve this, including merging the blast output with the full contig lengths. Other metrics on the blast hits were computed for reference in future work.

Code is reproducible using the Assignment_1.R script.

Data files were too large (>300 MB) to upload and instead can be accessed on OneDrive using the following link: https://mcmasteru365-my.sharepoint.com/:f:/g/personal/bsatay_mcmaster_ca/IgAYWpki92rSTba19Oxq0XcwAdYKtdcHXSGgiD6b1uk7c6U?e=kVD4Zg. 
How to use data: 'Data' folder should be placed within the Assignment 1 directory
**BMB:** it's actually 'data' (lowercase 'd'), according to your script

### References:
- https://www.geeksforgeeks.org/r-language/how-to-add-header-to-dataframe-in-r/
- https://www.geeksforgeeks.org/r-language/how-to-split-column-into-multiple-columns-in-r-dataframe/
- https://dplyr.tidyverse.org/reference/filter.html
- https://dplyr.tidyverse.org/reference/summarise.html#:~:text=summarise()%20creates%20a%20new,and%20summarize()%20are%20synonyms.
- https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/merge
- https://r-graph-gallery.com/272-basic-scatterplot-with-ggplot2.html


# Assignment 2
For this assignment, I'll be exploring the output of ANGSD association from mapped pair reads of Xenopus longipes. ANGSD stands for Analysis of Next Generation Sequencing Data
Similar to the previous assignment, the overarching  goal is to identify candidate contigs that could contain sex determining genes.
Specifically, ANGSD association is another yet important approach to do, as it uses genotype likelihoods to perform genetic association. 
For each polymorphic position, the minor allele frequency is computed and the likelihood ratio statistic (LRT) and Pvalue is also generated.
From this information, other parameters such as SNP density or the Negative Log(P) will be computer to to distinguish candidate contigs and prepare the data for future exploration.

### How to find the scripts
Scripts can be found in the Assignment 2 directory inside the scripts folder:
script_1.R contains data examination and data cleaning 
script_2.R contains computations more relevant to the biological question
Note: run script_1.R prior to script_2.R

JD: You should say which directory scripts need to be _run_ from as well. Also, please avoid spaces in filepaths: I renamed your directories and hope it does not cause inconvenience.

### Future work
The next steps would be 'annotating' the candidate contigs and using the X.laevis genome (specifically the blast output from Assignment 1) to highlight what genes they contain

### References: 
- https://www.popgen.dk/angsd/index.php/Association
- https://www.datacamp.com/tutorial/make-histogram-basic-r
- https://www.datacamp.com/doc/r/subset


# Assignment 3
For this assignment, I would like to visualize the -log(P) distribution of my contigs and use the ggplot package to discern and highlight outliers or interesting contigs. 
Previously I was performing this using filtering and sub-setting dataframes. Now using visualization perhaps it's more efficient to do a rough exploration across all contigs. 
Such contigs are 'interesting' because their high -log10(P) suggests a possible sex-linkage or sex-biased expressed. 

### Note: 
1) Lines 53-79 is a plot I experimented with which didn't produce much information and may in fact confuse 
I left it just to show my process but it doesn't have to be considered. 

2) In some dataframe names and plots I referred to the plotted contigs as 'sig' short for significant 
This does not indicate that I consider them significant, they are merely interesting
The 'sig' convention is just more convenient.

### How to find the scripts
There is only one main script: main.R that can be ran in the Assignment_3 directory. 
Data is available within the data folder

### References: 
- https://www.geeksforgeeks.org/r-language/rotating-x-axis-labels-and-changing-theme-in-ggplot2/
- https://ggplot2.tidyverse.org/
- https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/ifelse
- https://www.geeksforgeeks.org/r-language/size-of-points-in-ggplot2-comparable-across-plots-in-r/


# Assignment 4
For this assignment, I discussed the soundness of the statistical methods and reporting of the study  by Kosmopoulos et al. titled Co-inoculation with novel nodule-inhabiting bacteria reduces the benefits of legume-rhizobium symbiosis

### References:
- Kosmopoulos, J. C., Batstone-Doyle, R. T., & Heath, K. D. (2024). Co-inoculation with novel nodule-inhabiting bacteria reduces the benefits of legume-rhizobium symbiosis. Canadian journal of microbiology, 70(7), 275–288. https://doi.org/10.1139/cjm-2023-0209

JD: This is interesting and thoughtful. I am not sure I agree with everything, though. In particular, I think that language of “no clear impact” vs. “significant effect”, is an improvement on the state of most fields (although I agree that being consistent would be even better). I agree that selecting plants to go forward based on “significant impact” is problematic; no inferences (specifically unimportance) should be based on high P value alone. I'm not sure I agree about percentages vs. units. I'm also not against reporting P>0.1 – what do you learn from the exact P value that you wouldn't learn from the CI? Maybe say that they should report the CI (as I think they do here), and if they _can't_ then the actual P value gives us some idea of uncertainty.

JD: Try using wider margins and bigger fonts: text is easiest to read when there are <80 characters per line (70-75 is best, I think).

Grade: 2/3

# Assignment 5
For this assignment, I described my data and the questions I'll be asking about the data using measurement theory guided by the steps in Voje et al. 2023 Figure 1. I also described one comparison I'll be performing and we will decide a difference is 'important'


# Assignment 6
For this assignment, I used the Dry Eye Disease Dataset from Kaggle - source can be found in the references below - to explore the correlation between Dry Eye Disease and predictive parameters. 
The hypothesis follows as: The instances of dry eye disease increase with average screen time which varies with sleep duration
To test this hypothesis, I used a Generalized Linear  Model (GLM) because the response variable - presence or absence of Dry Eye Disease - is a binomial variable. 
I decided to use an interaction model to explore the effect of sleep duration and average screen time (not independently) on occurance of dry eye disease

### How to use the scripts 
cleanup_script.R contains the clean up of the dataset that will produce an RDS object 
model_script.R reads in the RDS object and performs the modeling and diagnostic steps. 

### References: 
- Dataeset: https://www.kaggle.com/datasets/dakshnagra/dry-eye-disease
- https://github.com/florianhartig/DHARMa/issues/396
- https://steffilazerte.ca/posts/dharma/
- https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm
- https://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/





