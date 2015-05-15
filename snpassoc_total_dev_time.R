
library(SNPassoc)

d = read.table('/home/cfriedline/eckertlab/gypsy_indiv/masked/analysis/samtools1.2/phased_snpassoc.txt', sep="\t", row.names=1, header=T)

#subtract b/c those are the PCA axes
snp_cols = 4:(ncol(d)-10)
snp_data = setupSNP(d, colSNPs=snp_cols, sep="/")
pca_cols = (ncol(d)-9):ncol(d)
pca_data = d[,pca_cols]

wg = WGassociation(Total_Dev_Time~1+pca_data$PC1+pca_data$PC2+pca_data$PC3+pca_data$PC4+pca_data$PC5+pca_data$PC6+pca_data$PC7+pca_data$PC8+pca_data$PC9+pca_data$PC10, data=snp_data, model="co")

saveRDS(wg, "phased_wg_total_dev_time_co.rds")
stats = WGstats(wg)
saveRDS(stats, "phased_wgstats_total_dev_time.rds")
