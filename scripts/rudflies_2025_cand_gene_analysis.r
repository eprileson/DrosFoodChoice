#########################################################################################
############# Candidate Gene Analysis for Food Choice ############################
################### Testing between timepoints w/ 2025 Allele frequencies of candidate genes ##########
######### Removed of Rolling windows analysis, 2023 data showed very minimal change w/ 20 bp window######
########################################################################################################################

### In R ###
##packages and libraries
packages <- c("lubridate", "pwr", "pwrss", "reshape2", "devtools", "stats", "TMB", 
              "MARSS", "datasets", "magrittr", "tidyr", "dplyr", "forecast", 
              "ggplot2", "viridis", "MASS", "AICcmodavg", "glmmTMB", "lme4", "nlme",
              "ggeffects", "emmeans", "DHARMa", "car", "boot", "geepack", "cowplot", "forcats", "visreg",
              "outliers", "EnvStats", "data.table", "matrixStats","Rcpp", "stringi", "foreach",
              "ggpubr", "stats", "ggfortify", "zoo", "rstatix", 'slider', "RColorBrewer")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
#load packages:
invisible(lapply(packages, library, character.only = TRUE))

##setwd 
setwd('/data/lab/rudman/Eric/Food_choice')


#################################
### Load required input files ###
#################################

## Variant Effect Predictor (VEP) annotation file with appended FLYCADD scores
vep <- read.table('filtered-all.annot.vcf.FLYCADD.tsv', header = TRUE) 

## More detailed gene information from gff
snp.gff.overlap <- read.delim("rudflies_2025_hafpipe_loci_genic_overlap_info.bed", sep="\t", header=TRUE)
## Sample metadata table
haf.meta <- read.csv("GHE_meta.csv")
## Hafpipe imputed allele frequency table
haf.freq <- read.delim("rudflies_2025_hafpipe.csv.gz", header=TRUE, sep = ",")

## Do a bit of reformatting for the frequency table header
names(haf.freq) <- gsub("[.]af","",names(haf.freq))
names(haf.freq) <- gsub("X","",names(haf.freq))
names(haf.freq)[1]="CHROM"
names(haf.freq)[2]="POS"

## Removing samples from other experiments
haf.freq <- haf.freq[, 1:74] #removes Storage Effect and Spino experiments
haf.freq <- haf.freq[, -c(3:5, 11:26, 30:34, 63:65)] #removes Greenhouse treatments


## Filter meta data for founder samples "E", 
haf.meta.filt <- haf.meta[haf.meta$Population == "E100",]
haf.meta.filt <- haf.meta.filt[-c(6:29),] #remove greenhouse cages

## Find matching sample names in the frequency table, post metadata filtering
haf.freq.filt <- haf.freq[, which((names(haf.freq) %in% haf.meta.filt$Well_plate_ID)==TRUE)]
## And run the reciprocal filter
haf.meta.filt <- haf.meta.filt[which((haf.meta.filt$Well_plate_ID %in% names(haf.freq.filt))==TRUE),]

## additional filter to remove low variance loci
haf.sites.filt <- na.omit(haf.freq[rowVars(as.matrix(haf.freq.filt))>0.001,c(1:2)])
haf.freq.filt <- na.omit(haf.freq.filt[rowVars(as.matrix(haf.freq.filt))>0.001,])

## tables need a bit of reformatting for some downstream plotting
haf.meta.filt$tpt <- as.factor(haf.meta.filt$Time.Point)

############################################
### Load E time-point GLM contrast table ###
############################################

glm.tpt <- na.omit(read.table("rudflies_2025_E_tpt.wLoci.GLMcontrast.txt", header=TRUE))

head(glm.tpt)

## Filter and sort
glm.tpt <- glm.tpt[glm.tpt$CHROM != "4",] #remove chromsome 4
glm.tpt <- glm.tpt %>%
  		arrange(CHROM,POS) #sort by locus

#change nondescript tpt headers
colnames(glm.tpt) <- c("CHROM", "POS", "T0-T1", "T0-T2", "T0-T3", "T0-T4", "T1-T2", "T1-T3", "T1-T4", "T2-T3", "T2-T4", "T3-T4")

## How many sites remain after merging all filtered GLM results tables?
dim(glm.tpt) #chrom, pos, and the 10 time point pairwise comparisons
#[1] 1171665       12

## P-value correction
for(i in c(3:12)) { #start after loci columns and append FDR cols at the end of the table
  glm.tpt <- cbind(glm.tpt, p.adjust(glm.tpt[,i], method = "fdr"))
  colnames(glm.tpt)[i+10] <- paste(names(glm.tpt)[i], ".fdr", sep="")
}

## Double check number of cols after adding FDR cols
dim(glm.tpt)
#[1] 1171665      22

## Calculate -log10 for p-values
for(i in 3:12) { #start after loci columns and append logp cols at the end of the table
  glm.tpt <- cbind(glm.tpt, as.numeric(-log10(glm.tpt[,i])))
  colnames(glm.tpt)[i+20] <- paste(names(glm.tpt)[i], ".logp", sep="") #index of 20 because of where the indexing for adjustment starts
}


## Merge annotations for filtering and highlighting
glm.tpt.annot <- merge(glm.tpt, vep, by=c("CHROM","POS"))
head(glm.tpt.annot)
## Load food-choice candidate gene list
food.cand <- read.table("Candidate_gene.txt", header=FALSE)
names(food.cand) <- "Gene"

## Now merge to find All SNPs in and around candidate genes
glm.tpt.food <- merge(food.cand, glm.tpt.annot, by="Gene")
head(glm.tpt.food)

####################################
### Hypergeometric overlap tests ###
####################################

## Calculate minimum p-values and FDR values per annotated food candidate genes. We will 
## assign the most significant FDR value overlapping the gene, plus its immediate upstream 
## and downstream regions, to represent the gene in comparison with background gene sets.

## Pull FDR columns only for food candidates
head(glm.tpt.food)
glm.tpt.fdr.food <- unique(cbind(glm.tpt.food[,c(1:3)],
                                                glm.tpt.food$Consequence,
                                                select(glm.tpt.food,contains("fdr"))))

## Loop through each GLM-results FDR column to calculate food candidate minimum values
glm.tpt.fdr.food.min <- c() # create an object
#loop through all cols fdr values for candidate genes
for(g in 5:ncol(glm.tpt.fdr.food)) {
  tryCatch({
  	#create 2-column table with gene name and one FDR column at a time
    temp.min <- as.data.frame(cbind(glm.tpt.fdr.food$Gene,glm.tpt.fdr.food[,g]))
    #name columns
    colnames(temp.min)[1] <- "Gene" 
    colnames(temp.min)[2] <- "fdr"
    #assign minimum FDR values
    glm.tpt.fdr.food.min <- cbind(glm.tpt.fdr.food.min,
    	temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(fdr),
        	list(min = min)) %>%
        	pull(min))
  }, error=function(e){})
}
## Rename columns of new table
colnames(glm.tpt.fdr.food.min) <- names(glm.tpt.fdr.food)[c(5:ncol(glm.tpt.fdr.food))]
## Add gene names to the table
glm.tpt.fdr.food.min <- cbind(Gene=temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(fdr),
        	list(min = min)) %>%
        	pull(Gene),glm.tpt.fdr.food.min)
head(glm.tpt.fdr.food.min)        	
## Save minimum FDR results for downstream use
write.table(glm.tpt.fdr.food.min, file="rudflies_2025.glm.tpt.minfdr.food.txt", quote = FALSE, sep = "\t", row.names = F)
        	
## Pull FDR columns only for all genes/features
head(glm.tpt.annot)
glm.tpt.annot.fdr <- unique(cbind(Gene=glm.tpt.annot$Gene,glm.tpt.annot[,c(1,2)],Consequence=glm.tpt.annot$Consequence,select(glm.tpt.annot,contains("fdr"))))

## Loop through each GLM-results FDR column to calculate minimum values for all genes
glm.tpt.annot.fdr.min <- c() # create an object
#loop through all cols fdr values for candidate genes
for(g in 5:ncol(glm.tpt.annot.fdr)) { #nrow(haf.freq.cand)
  tryCatch({
  	#create 2-column table with gene name and one FDR column at a time
    temp.min <- as.data.frame(cbind(glm.tpt.annot.fdr$Gene,glm.tpt.annot.fdr[,g]))
    #name columns
    colnames(temp.min)[1] <- "Gene" 
    colnames(temp.min)[2] <- "fdr"    
    #assign minimum FDR values
    glm.tpt.annot.fdr.min <- cbind(glm.tpt.annot.fdr.min,
    	temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(fdr),
        	list(min = min)) %>%
        	pull(min))
  }, error=function(e){})
}

## Rename columns of new table
colnames(glm.tpt.annot.fdr.min) <- names(glm.tpt.annot.fdr)[c(5:ncol(glm.tpt.annot.fdr))]
## Add gene names to the table
glm.tpt.annot.fdr.min <- cbind(Gene=temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(fdr),
        	list(min = min)) %>%
        	pull(Gene),glm.tpt.annot.fdr.min)


### Redo above with missense SNPs in food candidates only ###

## Merge to find missense SNPs in and around food candidate genes
glm.tpt.food.missense <- merge(food.cand, glm.tpt.annot[glm.tpt.annot$Consequence=="missense_variant",], by="Gene")

## Pull FDR columns only for food candidate missense SNPs
glm.tpt.fdr.food.missense <- unique(cbind(glm.tpt.food.missense[,c(1:3)],glm.tpt.food.missense$Consequence,select(glm.tpt.food.missense,contains("fdr"))))

## Loop through each GLM-results FDR column to calculate food candidate minimum values
glm.tpt.fdr.food.missense.min <- c() # create an object
#loop through all cols fdr values for candidate genes
for(g in 5:ncol(glm.tpt.fdr.food.missense)) { #nrow(haf.freq.cand)
  tryCatch({
  	#create 2-column table with gene name and one FDR column at a time
    temp.min <- as.data.frame(cbind(glm.tpt.fdr.food.missense$Gene,glm.tpt.fdr.food.missense[,g]))
    #name columns
    colnames(temp.min)[1] <- "Gene" 
    colnames(temp.min)[2] <- "fdr"    
    #assign minimum FDR values
    glm.tpt.fdr.food.missense.min <- cbind(glm.tpt.fdr.food.missense.min,
    	temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(fdr),
        	list(min = min)) %>%
        	pull(min))
  }, error=function(e){})
}
## Rename columns of new table
colnames(glm.tpt.fdr.food.missense.min) <- names(glm.tpt.fdr.food.missense)[c(5:ncol(glm.tpt.fdr.food.missense))]
## Add gene names to the table
glm.tpt.fdr.food.missense.min <- cbind(Gene=temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(fdr),
        	list(min = min)) %>%
        	pull(Gene),glm.tpt.fdr.food.missense.min)

## Save minimum FDR results from missense SNPs for downstream use
write.table(glm.tpt.fdr.food.missense.min, file="rudflies_2025_redo.glm.tpt.minfdr.food.missense.txt", quote = FALSE, sep = "\t", row.names = F)


## Pull FDR columns only for all genes/features, filtered for missense SNPs only
glm.tpt.fdr.missense <- glm.tpt.annot.fdr[glm.tpt.annot.fdr$Consequence == "missense_variant",]

## Loop through each GLM-results FDR column to calculate minimum values for all genes
glm.tpt.fdr.missense.min <- c() # create an object
#loop through all cols fdr values for candidate genes
for(g in 5:ncol(glm.tpt.fdr.missense)) {
  tryCatch({
  	#create 2-column table with gene name and one FDR column at a time
    temp.min <- as.data.frame(cbind(glm.tpt.fdr.missense$Gene,glm.tpt.fdr.missense[,g]))
    #name columns
    colnames(temp.min)[1] <- "Gene" 
    colnames(temp.min)[2] <- "fdr"    
    #assign minimum FDR values
    glm.tpt.fdr.missense.min <- cbind(glm.tpt.fdr.missense.min,
    	temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(fdr),
        	list(min = min)) %>%
        	pull(min))
  }, error=function(e){})
}
## Rename columns of new table
colnames(glm.tpt.fdr.missense.min) <- names(glm.tpt.fdr.missense)[c(5:ncol(glm.tpt.fdr.missense))]
## Add gene names to the table
glm.tpt.fdr.missense.min <- cbind(Gene=temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(fdr),
        	list(min = min)) %>%
        	pull(Gene),glm.tpt.fdr.missense.min)


## Now we've found minimum FDR values for candidate gene and background gene lists, 
## let's run hypergeometric tests on the overlap of candidates and GLM outliers.

## run hypergeometric overlap test per GLM contrast
## First with all gene-related/adjacent SNPS
## Significance threshold FDR<0.05

dim(glm.tpt.annot.fdr.min)
dim(glm.tpt.fdr.food.min)

phyper_all_contrasts05 <- c() #create empty object
q_all <- c() #candidate - outlier overlap gene count
m_all <- c() #candidate gene count
n_all <- c() #non-candidate gene count
k_all <- c() #outlier gene count
## Loop through all GLM columns
for(contrast in 2:ncol(glm.tpt.fdr.food.min)) {
  q <- length(unique(glm.tpt.fdr.food.min[glm.tpt.fdr.food.min[,contrast] < 0.05,1]))
  m <- length(unique(glm.tpt.fdr.food.min[,1]))
  n <- length(unique(glm.tpt.annot.fdr.min[,1])) 
  k <- length(unique(glm.tpt.annot.fdr.min[glm.tpt.annot.fdr.min[,contrast] < 0.05,1]))
  ## Fill in Values below, and make sure to subtract 1 from overlap for phyper
  phyper_all_contrasts05 <- append(phyper_all_contrasts05,phyper(q-1, m, n, k, lower.tail = FALSE, log.p = FALSE))
  ## save results as we go
  q_all <- append(q_all,q)
  m_all <- append(m_all,m)
  n_all <- append(n_all,n)
  k_all <- append(k_all,k)    
}

## Create results summary table
phyper_foodCand_FDR05 <- cbind(contrast=colnames(glm.tpt.fdr.food.min[,2:ncol(glm.tpt.fdr.food.min)]),phyper_pval=phyper_all_contrasts05,phyper_sig=phyper_all_contrasts05 < 0.05,q_all,m_all,k_all,n_all)

## Significance threshold FDR<0.01
phyper_all_contrasts01 <- c() #create empty object
q_all <- c() #candidate - outlier overlap gene count
m_all <- c() #candidate gene count
n_all <- c() #non-candidate gene count
k_all <- c() #outlier gene count
## Loop through all GLM columns
for(contrast in 2:ncol(glm.tpt.fdr.food.min)) {
  q <- length(unique(glm.tpt.fdr.food.min[glm.tpt.fdr.food.min[,contrast] < 0.01,1]))
  m <- length(unique(glm.tpt.fdr.food.min[,1]))
  n <- length(unique(glm.tpt.annot.fdr.min[,1])) - m
  k <- length(unique(glm.tpt.annot.fdr.min[glm.tpt.annot.fdr.min[,contrast] < 0.01,1]))
  ## Fill in Values below, and make sure to subtract 1 from overlap for phyper
  phyper_all_contrasts01 <- append(phyper_all_contrasts01,phyper(q-1, m, n, k, lower.tail = FALSE, log.p = FALSE))
  ## save results as we go
  q_all <- append(q_all,q)
  m_all <- append(m_all,m)
  n_all <- append(n_all,n)
  k_all <- append(k_all,k)  
}

## Create results summary table
phyper_foodCand_FDR01 <- cbind(contrast=colnames(glm.tpt.fdr.food.min[,2:ncol(glm.tpt.fdr.food.min)]),phyper_pval=phyper_all_contrasts01,phyper_sig=phyper_all_contrasts01 < 0.05,q_all,m_all,k_all,n_all)


### Next with only missense SNPS ###

## Significance threshold FDR<0.05
phyper_missense_contrasts05 <- c() #create empty object
q_all <- c() #candidate - outlier overlap gene count
m_all <- c() #candidate gene count
n_all <- c() #non-candidate gene count
k_all <- c() #outlier gene count
## Loop through all GLM columns
for(contrast in 2:ncol(glm.tpt.fdr.food.missense.min)) {
  q <- length(unique(glm.tpt.fdr.food.missense.min[glm.tpt.fdr.food.missense.min[,contrast] < 0.05,1]))
  m <- length(unique(glm.tpt.fdr.food.missense.min[,1]))
  n <- length(unique(glm.tpt.fdr.missense.min[,1])) - m
  k <- length(unique(glm.tpt.fdr.missense.min[glm.tpt.fdr.missense.min[,contrast] < 0.05,1]))
  ## Fill in Values below, and make sure to subtract 1 from overlap for phyper
  phyper_missense_contrasts05 <- append(phyper_missense_contrasts05,phyper(q-1, m, n, k, lower.tail = FALSE, log.p = FALSE))
  ## save results as we go
  q_all <- append(q_all,q)
  m_all <- append(m_all,m)
  n_all <- append(n_all,n)
  k_all <- append(k_all,k)  
}

## Create results summary table
phyper_missense_foodCand_FDR05 <- cbind(contrast=colnames(glm.tpt.fdr.food.missense.min[,2:ncol(glm.tpt.fdr.food.missense.min)]),phyper_pval=phyper_missense_contrasts05,phyper_sig=phyper_missense_contrasts05 < 0.05,q_all,m_all,k_all,n_all)

## Significance threshold FDR<0.01
phyper_missense_contrasts01 <- c() #create empty object
q_all <- c() #candidate - outlier overlap gene count
m_all <- c() #candidate gene count
n_all <- c() #non-candidate gene count
k_all <- c() #outlier gene count
## Loop through all GLM columns
for(contrast in 2:ncol(glm.tpt.fdr.food.missense.min)) {
  q <- length(unique(glm.tpt.fdr.food.missense.min[glm.tpt.fdr.food.missense.min[,contrast] < 0.01,1]))
  m <- length(unique(glm.tpt.fdr.food.missense.min[,1]))
  n <- length(unique(glm.tpt.fdr.missense.min[,1])) - m
  k <- length(unique(glm.tpt.fdr.missense.min[glm.tpt.fdr.missense.min[,contrast] < 0.01,1]))
  ## Fill in Values below, and make sure to subtract 1 from overlap for phyper
  phyper_missense_contrasts01 <- append(phyper_missense_contrasts01,phyper(q-1, m, n, k, lower.tail = FALSE, log.p = FALSE))
  ## save results as we go
  q_all <- append(q_all,q)
  m_all <- append(m_all,m)
  n_all <- append(n_all,n)
  k_all <- append(k_all,k)  
}

## Create results summary table
phyper_missense_foodCand_FDR01 <- cbind(contrast=colnames(glm.tpt.fdr.food.missense.min[,2:ncol(glm.tpt.fdr.food.missense.min)]),phyper_pval=phyper_missense_contrasts01,phyper_sig=phyper_missense_contrasts01 < 0.05,q_all,m_all,k_all,n_all)

## Save results table for foodsyn candidates vs GLM outliers (FDR < 0.05)
write.table(phyper_foodCand_FDR05, file="rudflies_2025.phyper_allSNPs_foodCand_FDR05.txt", quote = FALSE, sep = "\t", row.names = F)

## Save results table for foodsyn candidates vs GLM outliers (FDR < 0.01)
write.table(phyper_foodCand_FDR01, file="rudflies_2025.phyper_allSNPs_foodCand_FDR01.txt", quote = FALSE, sep = "\t", row.names = F)

## Save results table for foodsyn candidates vs GLM outliers (FDR < 0.05), missense only
write.table(phyper_missense_foodCand_FDR05, file="rudflies_2025.phyper_missenseSNPs_foodCand_FDR05.txt", quote = FALSE, sep = "\t", row.names = F)

## Save results table for foodsyn candidates vs GLM outliers (FDR < 0.01), missense only
write.table(phyper_missense_foodCand_FDR01, file="rudflies_2025.phyper_missenseSNPs_foodCand_FDR01.txt", quote = FALSE, sep = "\t", row.names = F)

### Which contrasts show significant overlap, prior to p-value correction

## food candidates vs GLM outliers (FDR < 0.05), all SNPs
phyper_foodCand_FDR05[phyper_foodCand_FDR05[,2]<0.05,]
## food candidates vs GLM outliers (FDR < 0.01), all SNPs
phyper_foodCand_FDR01[phyper_foodCand_FDR01[,2]<0.05,]
## food candidates vs GLM outliers (FDR < 0.05), missense SNPs
phyper_missense_foodCand_FDR05[phyper_missense_foodCand_FDR05[,2]<0.05,]
## food candidates vs GLM outliers (FDR < 0.01), missense SNPs
phyper_missense_foodCand_FDR01[phyper_missense_foodCand_FDR01[,2]<0.05,]


#############################################################
### Examine candidate gene p-values via Wilcoxon rank sum ###
#############################################################

## Hypergeometric overlap analysis relies on GLM outlier cutoff decisions, which can 
## impact results. Wilcoxon Rank Sum tests are non-parametric and rely on rank order of
## all genes in the dataset. Therefore, this test is more sensitive to more modest 
## AF changes across a broader gene network or pathway.

### Remake annotation stat summary lists for pvalues instead of fdr values ###

## Pull raw p-value columns only for food candidates
head(glm.tpt.pval.food)
#need to append .pval to the raw pval contrasts
colnames(glm.tpt.food)[4:13] <- paste(names(glm.tpt.food)[4:13], ".pval", sep="")
glm.tpt.pval.food <- unique(cbind(glm.tpt.food[,c(1:3)],Consequence=glm.tpt.food$Consequence,select(glm.tpt.food,contains(".pval"),-contains("fdr"),-contains("logp"))))

## Loop through each GLM-results pval column to calculate food candidate minimum values
glm.tpt.pval.food.min <- c() # create an object
#loop through all cols p-values for candidate genes
for(g in 5:ncol(glm.tpt.pval.food)) {
  tryCatch({
  	#create 2-column table with gene name and one pval column at a time
    temp.min <- as.data.frame(cbind(glm.tpt.pval.food$Gene,glm.tpt.pval.food[,g]))
    #name columns
    colnames(temp.min)[1] <- "Gene" 
    colnames(temp.min)[2] <- "pval"
    #assign minimum p-values    
    glm.tpt.pval.food.min <- cbind(glm.tpt.pval.food.min,
    	temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(pval),
        	list(min = min)) %>%
        	pull(min))
  }, error=function(e){})
}
## Rename columns of new table
colnames(glm.tpt.pval.food.min) <- names(glm.tpt.pval.food)[c(5:ncol(glm.tpt.pval.food))]
## Add gene names to the table
glm.tpt.pval.food.min <- cbind(Gene=temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(pval),
        	list(min = min)) %>%
        	pull(Gene),glm.tpt.pval.food.min)
        	
## Save minimum pval results for downstream use
write.table(glm.tpt.pval.food.min, file="rudflies_2025.glm.tpt.minpval.food.txt", quote = FALSE, sep = "\t", row.names = F)
        	
        	
## Pull pval columns only for all genes/features
head(glm.tpt.annot)
#need to append .pval to the raw pval contrasts
colnames(glm.tpt.annot)[3:12] <- paste(names(glm.tpt.annot)[3:12], ".pval", sep="")

glm.tpt.annot.pval <- unique(cbind(Gene=glm.tpt.annot$Gene,glm.tpt.annot[,c(1,2)],Consequence=glm.tpt.annot$Consequence,select(glm.tpt.annot,contains(".pval"),-contains("fdr"),-contains("logp"))))
glm.tpt.annot.pval <- na.omit(glm.tpt.annot.pval)
head(glm.tpt.annot.pval)
## Loop through each GLM-results pval column to calculate minimum values for all genes
glm.tpt.annot.pval.min <- c() # create an object
#loop through all cols p-values for candidate genes
for(g in 5:ncol(glm.tpt.annot.pval)) {
  tryCatch({
    #create 2-column table with gene name and one pval column at a time
    temp.min <- as.data.frame(cbind(glm.tpt.annot.pval$Gene,glm.tpt.annot.pval[,g]))
    #name columns
    colnames(temp.min)[1] <- "Gene" 
    colnames(temp.min)[2] <- "pval"  
    #assign minimum p-values  
    glm.tpt.annot.pval.min <- cbind(glm.tpt.annot.pval.min,
    	temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(pval),
        	list(min = min)) %>%
        	pull(min))
  }, error=function(e){})
}
head(glm.tpt.annot.pval.min)
## Rename columns of new table
colnames(glm.tpt.annot.pval.min) <- names(glm.tpt.annot.pval)[c(5:ncol(glm.tpt.annot.pval))]
## Add gene names to the table
glm.tpt.annot.pval.min <- cbind(Gene=temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(pval),
        	list(min = min)) %>%
        	pull(Gene),glm.tpt.annot.pval.min)

## Calculate medians instead for plotting
glm.tpt.annot.pval.med <- unique(glm.tpt.annot.pval[,1])
glm.tpt.annot.pval.med <- glm.tpt.annot.pval.med[order(glm.tpt.annot.pval.med) ]
#loop through all cols p-values for candidate genes
for(g in c(3,5:ncol(glm.tpt.annot.pval))) {
  tryCatch({
    temp.med <- as.data.frame(cbind(glm.tpt.annot.pval$Gene,glm.tpt.annot.pval[,g]))
    colnames(temp.med)[1] <- "Gene" 
    colnames(temp.med)[2] <- "pval" 
    #assign median p-values
    glm.tpt.annot.pval.med <- cbind(glm.tpt.annot.pval.med,
    	temp.med %>%
			group_by(Gene) %>% 
			summarise(median = median(as.numeric(pval), na.rm = TRUE)) %>%
        	pull(median))
  }, error=function(e){})
}
## Rename columns of new table
colnames(glm.tpt.annot.pval.med)[c(2:ncol(glm.tpt.annot.pval.med))] <- names(glm.tpt.annot.pval)[c(3,5:ncol(glm.tpt.annot.pval))]
colnames(glm.tpt.annot.pval.med)[1]="Gene"

## Add chromosome info to table for plotting
chrom.list <- unique(glm.tpt.annot.pval[,c(1:2)])
chrom.list <- chrom.list[order(chrom.list$Gene),]
chrom.list <- chrom.list[-c(1:5),]
glm.tpt.annot.pval.med <- merge(chrom.list, glm.tpt.annot.pval.med, by="Gene")
        	
        	
## Pull pval columns only for all genes/features, filtered for missense SNPs only
head(glm.tpt.food.missense)
#need to append .pval to the raw pval contrasts
colnames(glm.tpt.food.missense)[3:12] <- paste(names(glm.tpt.food.missense)[3:12], ".pval", sep="")

glm.tpt.pval.food.missense <- unique(cbind(glm.tpt.food.missense[,c(1:3)],glm.tpt.food.missense$Consequence,select(glm.tpt.food.missense,contains(".pval"),-contains("fdr"),-contains("logp"))))

## Loop through each GLM-results pval column to calculate minimum values for all genes
glm.tpt.pval.food.missense.min <- c() # create an object
#loop through all cols p-values for candidate genes
for(g in 5:ncol(glm.tpt.pval.food.missense)) { #nrow(haf.freq.cand)
  tryCatch({
	#create 2-column table with gene name and one pval column at a time
    temp.min <- as.data.frame(cbind(glm.tpt.pval.food.missense$Gene,glm.tpt.pval.food.missense[,g]))
    #name columns
    colnames(temp.min)[1] <- "Gene" 
    colnames(temp.min)[2] <- "pval"
    #assign minimum FDR values    
    glm.tpt.pval.food.missense.min <- cbind(glm.tpt.pval.food.missense.min,
    	temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(pval),
        	list(min = min)) %>%
        	pull(min))
  }, error=function(e){})
}
## Rename columns of new table
colnames(glm.tpt.pval.food.missense.min) <- names(glm.tpt.pval.food.missense)[c(5:ncol(glm.tpt.pval.food.missense))]
## Add gene names to the table
glm.tpt.pval.food.missense.min <- cbind(Gene=temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(pval),
        	list(min = min)) %>%
        	pull(Gene),glm.tpt.pval.food.missense.min)


## Save minimum pval results from missense SNPs for downstream use
write.table(glm.tpt.pval.food.missense.min, file="rudflies_2025.glm.tpt.minpval.food.missense.txt", quote = FALSE, sep = "\t", row.names = F)


## Pull pval columns only for all genes/features, filtered for missense SNPs only
glm.tpt.annot.pval.missense <- glm.tpt.annot.pval[glm.tpt.annot.pval$Consequence == "missense_variant",]

## Loop through each GLM-results pval column to calculate minimum values for all genes
glm.tpt.annot.pval.missense.min <- c() # create an object
#loop through all cols p-values for candidate genes
for(g in 5:ncol(glm.tpt.annot.pval.missense)) { #nrow(haf.freq.cand)
  tryCatch({
  	#create 2-column table with gene name and one pval column at a time
    temp.min <- as.data.frame(cbind(glm.tpt.annot.pval.missense$Gene,glm.tpt.annot.pval.missense[,g]))
    #name columns
    colnames(temp.min)[1] <- "Gene" 
    colnames(temp.min)[2] <- "pval"   
    #assign minimum p-values 
    glm.tpt.annot.pval.missense.min <- cbind(glm.tpt.annot.pval.missense.min,
    	temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(pval),
        	list(min = min)) %>%
        	pull(min))
  }, error=function(e){})
}
## Rename columns of new table
colnames(glm.tpt.annot.pval.missense.min) <- names(glm.tpt.annot.pval.missense)[c(5:ncol(glm.tpt.annot.pval.missense))]
## Add gene names to the table
glm.tpt.annot.pval.missense.min <- cbind(Gene=temp.min %>%
  			group_by(Gene) %>%
  			summarise_at(vars(pval),
        	list(min = min)) %>%
        	pull(Gene),glm.tpt.annot.pval.missense.min)
        	
        	


## Time to run the Wicoxon Rank Sum analyses. To do so, we will select one matched 
## background gene for every food-choice candidate gene. This matched set selection is 
## based on the following criteria:
## 	 -Gene must be similar length from start to stop (+/-25%)
## 	 -Gene must have similar number of SNPs (+/-25%)
## 	 -Gene must be the same type (protein-coding, ncRNA, etc)
## 	 -Gene must be on the same chromosome

## After all matching genes are discovered, we randomly select one match for each 
## candidate gene, then perform the Wilcoxon rank sum test between the candidate and 
## background genes to see if candidates show significantly higher p-value ranks (lower 
## p-values), or greater parallel allele frequency differences in the respective GLM 
## contrast. This process is repeated for 1K iterations of matched set selection to 
## mitigate any potential random sampling bias. 


##Find gene characteristics of candidates from GFF for matching
head(glm.tpt.pval.food.min)
head(snp.gff.overlap)
glm.tpt.pval.food.min.gff <- merge(snp.gff.overlap, glm.tpt.pval.food.min, by.x="ID", by.y="Gene")

##Find gene characteristics of background genes from GFF for matching
glm.tpt.pval.min.gff <- merge(snp.gff.overlap, glm.tpt.annot.pval.min, by.x="ID", by.y="Gene")

## Pull food candidate gene IDs
food_IDs <- glm.tpt.pval.food.min.gff$ID

## Remove food candidates from potential background gene list
glm.tpt.pval.bg.min.gff <- glm.tpt.pval.min.gff[!(glm.tpt.pval.min.gff$ID %in% food_IDs),]

## Create empty results table with 1K rows (for each iteration) and one column for 
## each GLM contrast.
wilcox.p <- data.frame(matrix(NA, nrow = 1000, ncol = ncol(glm.tpt.pval.food.min.gff)-ncol(snp.gff.overlap)))
## Modify column names to reflect GLM contrasts
names(wilcox.p) <- names(glm.tpt.pval.food.min.gff)[c(9:ncol(glm.tpt.pval.food.min.gff))]

#generate candidate gene filtering tables
for(k in 1:1000) { #set number of iterations
  #Establish background non-candidate set
  bg.samp.list <- c()
  #This loop finds a random set of matched background genes
  for(i in c(1:nrow(glm.tpt.pval.food.min.gff))) {
  	#Select matched lists based on following criteria
	length <- glm.tpt.pval.food.min.gff[i,]$LENGTH
	count <- glm.tpt.pval.food.min.gff[i,]$SNP_COUNT
	type <- glm.tpt.pval.food.min.gff[i,]$TYPE
	chrom <- glm.tpt.pval.food.min.gff[i,]$CHROM
	#Pull one match per candidate gene
	bg.samp.temp <- sample_n(glm.tpt.pval.bg.min.gff %>%
	 filter( 
	    TYPE==type,
		CHROM==chrom,
		LENGTH < length*1.25,
		LENGTH > length*0.75,
		SNP_COUNT < count*1.25,
		SNP_COUNT > count*0.75
	  ),1)
	#append to background list
	bg.samp.list <- rbind(bg.samp.list,bg.samp.temp)
  }
  #This loop will run Wilcoxon Rank Sum tests for each of "j" GLM contrasts
  for(j in 1:(ncol(glm.tpt.pval.food.min.gff)-ncol(snp.gff.overlap))) { #set number of iterations to the number of contrasts
    #Establish Rank Sum test sets for p values
    #candidate set
    candidates <- glm.tpt.pval.food.min.gff[,8+j]  
    #background non-candidate set
    bg.samp <- as.numeric(bg.samp.list[,8+j])
    candidates.samp <- as.numeric(candidates)
    #this will subsample the SNPs contained in one list to the number of SNPs in the shorter list
    if(length(candidates.samp) > length(bg.samp)){
      candidates.samp <- sample(candidates.samp, length(bg.samp))
    } else {
      bg.samp <- sample(bg.samp, length(candidates.samp))
    }
    #finally, run the tests and append results
    wilcox.p[k,j] <- wilcox.test(candidates.samp, bg.samp, alternative = "less")$p.value
}
}

## Save all results
# Wilcoxon rank sum p-values for all iterations
write.table(wilcox.p, file="rudflies_2025.glm.tpt.wilcox.p.txt", quote = FALSE, sep = "\t", row.names = F)

# Median Wilcoxon rank sum p-values across iterations
write.table(colMedians(as.matrix(wilcox.p)), file="rudflies_2025.glm.tpt.wilcox.p.medians.txt", quote = FALSE, sep = "\t", row.names = F)

# Mean Wilcoxon rank sum p-values across iterations
write.table(colMeans(as.matrix(wilcox.p)), file="rudflies_2025.glm.tpt.wilcox.p.means.txt", quote = FALSE, sep = "\t", row.names = F)

## Calculate the percent of p-values significant at a raw p<0.05
colMedians(as.matrix(wilcox.p))
colMeans(as.matrix(wilcox.p))
wilcox.contrast <- c()
wilcox.contrast.p05 <- wilcox.p<0.05
for(l in 1:ncol(wilcox.p)) {
   wilcox.contrast <- append(wilcox.contrast,sum(wilcox.contrast.p05[,l], na.rm=TRUE)*0.1)
}
cbind(contrast=names(wilcox.p),perc_sig=wilcox.contrast)

# Save percent of significant Wilcoxon rank sum p-values across iterations
write.table(cbind(contrast=names(wilcox.p),perc_sig=wilcox.contrast), file="rudflies_2025.glm.tpt.wilcox.p.percsig.txt", quote = FALSE, sep = "\t", row.names = F)


#########
######### END OF CAND GENE ANALYSIS
#########






