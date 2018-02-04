# Load dataset
setwd("/Users/saul/Dropbox/Masters_Material/cinvestav/2Q/CienciaDeDatos/ejercicio2/");
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
pdf("boxplot_alldata_srswor.pdf")
boxplot(sample, col = "lightblue")
dev.off()

# Histograms and density
pdf("histdens_alldata_srswor.pdf")
par(mfrow=c(1, 4))
colnames <- dimnames(sample)[[2]]
for (i in 1:4) {
  hist(sample[,i], main=colnames[i], probability=TRUE,col="gray",border="white",xlab='Values')
  lines(density(sample[,i]), col="blue", lwd=2)
  lines(density(sample[,i], adjust=2), lty="dotted", col="darkgreen", lwd=2)
}
dev.off()

# Correlation between variables
pdf("correlations_alldata_srswor.pdf")
library(corrgram)
corrgram(sample, lower.panel=panel.conf, upper.panel=panel.pts, diag.panel=panel.density, main="Correlations of Sample data set")
dev.off()

# By groups
pdf("histdens_bygroups_srswor.pdf")
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

pdf("boxplot_bygroup_srswor.pdf")
par(mfrow=c(1, 3))
boxplot(V1_1, xlab = "", ylab = "", main = "Grupo 1", col = "lightgreen")
boxplot(V1_2, xlab = "", ylab = "", main = "Grupo 2", col = "lightgreen")
boxplot(V1_3, xlab = "", ylab = "", main = "Grupo 3", col = "lightgreen")
dev.off()