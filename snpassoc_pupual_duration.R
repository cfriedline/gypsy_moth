
library(SNPassoc)

d = read.table('phased_snpassoc.txt', sep="\t", row.names=1, header=T)

#subtract b/c those are the PCA axes
snp_cols = 4:(ncol(d)-12)
snp_data = setupSNP(d, colSNPs=snp_cols, sep="/")
pca_cols = (ncol(d)-11):ncol(d)
pca_data = d[,pca_cols]

wg = WGassociation(Pupual_Duration~1+pca_data$PC1+pca_data$PC2+pca_data$PC3+pca_data$PC4+pca_data$PC5+pca_data$PC6+pca_data$PC7+pca_data$PC8+pca_data$PC9+pca_data$PC10+pca_data$PC11+pca_data$PC12, data=snp_data, model="co")

saveRDS(wg, "phased_wg_pupual_duration_co.rds")
stats = WGstats(wg)
saveRDS(stats, "phased_wgstats_pupual_duration.rds")
