mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = T, scale. = T)
summary(mtcars.pca)
str(mtcars.pca)
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
dev.off()
ggbiplot(mtcars.pca)
ggbiplot(mtcars.pca, labels=rownames(mtcars))
mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)
ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),   labels=rownames(mtcars), groups=mtcars.country)
ggbiplot(mtcars.pca,ellipse=TRUE,circle=TRUE, labels=rownames(mtcars), groups=mtcars.country)
ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country)
ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=rownames(mtcars), groups=mtcars.country)
ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country) +
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
  ggtitle("PCA of mtcars dataset")+
  theme_minimal()+
  theme(legend.position = "bottom")
spacecar <- c(1000,60,50,500,0,0.5,2.5,0,1,0,0)

mtcarsplus <- rbind(mtcars, spacecar)
mtcars.countryplus <- c(mtcars.country, "Jupiter")

mtcarsplus.pca <- prcomp(mtcarsplus[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

ggbiplot(mtcarsplus.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = FALSE, var.axes=TRUE, labels=c(rownames(mtcars), "spacecar"), groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "violet", "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample added")+
  theme_minimal()+
  theme(legend.position = "bottom")
