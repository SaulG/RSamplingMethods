# Load dataset
setwd("<your-path-here>");
getwd();
dataset = read.csv("dataset.csv", header=FALSE);

# Sampling by using Stratified method
sampling = sampling::strata(dataset, stratanames = c("V1"), size=c(10000,10000,10000), method="srswor")
sample = sampling::getdata(dataset, sampling)

# Columns to keep from dataset
keep_columns = c("V1","V2","V3","V4")
sample <- sample[keep_columns]

# All data

# Box plot
pdf("output/boxplot_alldata_strata.pdf", width=6,height=4,paper='special')
boxplot(sample, col = "lightblue", main='Stratified Sample From Dataset', xlab='Variables', ylab='Values')
dev.off()

# Histograms and density
pdf("output/histdens_alldata_strata.pdf", width=12,height=4,paper='special')
par(mfrow=c(1, 4))
colnames <- dimnames(sample)[[2]]
for (i in 1:4) {
  hist(sample[,i], main=colnames[i], probability=TRUE,col="gray",border="white",xlab='Values')
  lines(density(sample[,i]), col="blue", lwd=2)
  lines(density(sample[,i], adjust=2), lty="dotted", col="darkgreen", lwd=2)
}
dev.off()

# Correlation between variables
pdf("output/correlations_alldata_strata.pdf",width=6,height=4,paper='special')
library(corrgram)
corrgram(sample, lower.panel=panel.conf, upper.panel=panel.pts, diag.panel=panel.density, main="Correlations of Stratified Sample From Dataset")
dev.off()

# By groups
for (i in 1:3) {
  pdf(paste("output/histdens_bygroups_strata_", i , ".pdf", sep=""), width=6,height=4,paper='special')
  par(mfrow=c(1,3))
  v_group <- sample[sample$V1==i,]
  for (j in 1:3) {
    hist(v_group[,j], xlab='Values', main=paste(c("V", j), collapse = " "), prob=TRUE, col="grey")
    lines(density(v_group[,j]), col="blue", lwd=2)
    lines(density(v_group[,j], adjust=2), lty="dotted", col="darkgreen", lwd=2)
  }
  dev.off()
}

# By Group box plot
V1_1 = sample[sample$V1==1,]
V1_1$V1 <- NULL
V1_2 = sample[sample$V1==2,]
V1_2$V1 <- NULL
V1_3 = sample[sample$V1==3,]
V1_3$V1 <- NULL

pdf("output/boxplot_bygroup_strata.pdf",width=8,height=2,paper='special')
par(mfrow=c(1, 3))
boxplot(V1_1, xlab = "", ylab = "", main = "Grupo 1", col = "lightgreen")
boxplot(V1_2, xlab = "", ylab = "", main = "Grupo 2", col = "lightgreen")
boxplot(V1_3, xlab = "", ylab = "", main = "Grupo 3", col = "lightgreen")
dev.off()
