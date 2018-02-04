# Load dataset
setwd("<your-path-here>");
getwd();
dataset = read.csv("dataset.csv", header=FALSE);

# Sampling by using Simple random sampling without replacement
sampling_srswor = sampling::srswor(10000, nrow(dataset));
sample = sampling::getdata(dataset, sampling_srswor)

# Columns to keep from dataset
keep_columns = c("V1","V2","V3","V4")
sample <- sample[keep_columns]

# All data

# Box plot
pdf("output/boxplot_alldata_srswor.pdf")
boxplot(sample, col = "lightblue")
dev.off()

# Histograms and density
pdf("output/histdens_alldata_srswor.pdf")
par(mfrow=c(1, 4))
colnames <- dimnames(sample)[[2]]
for (i in 1:4) {
  hist(sample[,i], main=colnames[i], probability=TRUE,col="gray",border="white",xlab='Values')
  lines(density(sample[,i]), col="blue", lwd=2)
  lines(density(sample[,i], adjust=2), lty="dotted", col="darkgreen", lwd=2)
}
dev.off()

# Correlation between variables
pdf("output/correlations_alldata_srswor.pdf")
library(corrgram)
corrgram(sample, lower.panel=panel.conf, upper.panel=panel.pts, diag.panel=panel.density, main="Correlations of Sample data set")
dev.off()

# By groups
pdf("output/histdens_bygroups_srswor.pdf")
par(mfrow=c(3,3))
for (i in 1:3) {
  v_group <- sample[sample$V1==i,]
  for (j in 1:3) {
    hist(v_group[,j], xlab='Values', main=paste(c("V", j), collapse = " "), prob=TRUE, col="grey")
    lines(density(v_group[,j]), col="blue", lwd=2)
    lines(density(v_group[,j], adjust=2), lty="dotted", col="darkgreen", lwd=2)
  }
}
dev.off()

# By Group box plot
V1_1 = sample[sample$V1==1,]
V1_1$V1 <- NULL
V1_2 = sample[sample$V1==2,]
V1_2$V1 <- NULL
V1_3 = sample[sample$V1==3,]
V1_3$V1 <- NULL

pdf("output/boxplot_bygroup_srswor.pdf")
par(mfrow=c(1, 3))
boxplot(V1_1, xlab = "", ylab = "", main = "Grupo 1", col = "lightgreen")
boxplot(V1_2, xlab = "", ylab = "", main = "Grupo 2", col = "lightgreen")
boxplot(V1_3, xlab = "", ylab = "", main = "Grupo 3", col = "lightgreen")
dev.off()

