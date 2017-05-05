# Load the package
library(jpeg)
library(imager)

setwd("C:/OneDrive/___dev_R/fator_empilhamento/in")
par <- read.csv("parametros.csv", header = FALSE,  sep = ":")
img <- imager::load.image("imagem.jpg")
imgDm <- dim(img)
img <- imager::resize(img, imgDm[1]/par$V2[1], imgDm[2]/par$V2[1])

# Obtain the dimension
imgDm <- dim(img)

# Assign RGB channels to data frame
imgRGB <- data.frame(
        x = rep(1:imgDm[2], each = imgDm[1]),
        y = rep(imgDm[1]:1, imgDm[2]),
        R = as.vector(img[,,1]),
        G = as.vector(img[,,2]),
        B = as.vector(img[,,3])
)

library(ggplot2)

# ggplot theme to be used
plotTheme <- function() {
        theme(
                panel.background = element_rect(
                        size = 3,
                        colour = "black",
                        fill = "white"),
                axis.ticks = element_line(
                        size = 2),
                panel.grid.major = element_line(
                        colour = "gray80",
                        linetype = "dotted"),
                panel.grid.minor = element_line(
                        colour = "gray90",
                        linetype = "dashed"),
                axis.title.x = element_text(
                        size = rel(1.2),
                        face = "bold"),
                axis.title.y = element_text(
                        size = rel(1.2),
                        face = "bold"),
                plot.title = element_text(
                        size = 20,
                        face = "bold",
                        vjust = 1.5)
        )
}

# Plot the image
#ggplot(data = imgRGB, aes(x = x, y = y)) + 
#        geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
#        labs(title = "Original Image") +
#        xlab("x") +
#        ylab("y") +
#        plotTheme()

##Clustering

#Apply k-Means clustering on the image:
kClusters <- par$V2[2]
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

#fator de empilhamento:
fe <- sum(kMeans$size)/sum(kMeans$size[1:2])

#Plot the clustered colours:
imagem <- ggplot(data = imgRGB, aes(x = x, y = y)) + 
        geom_point(colour = kColours) +
        labs(title = paste("Classificação em", kClusters, "cores")) +
        labs(x = paste("Fator de empilhamento:", fe)) +
        plotTheme()

ggsave('imagem.png', plot = imagem, path = "C:/OneDrive/___dev_R/fator_empilhamento/out")