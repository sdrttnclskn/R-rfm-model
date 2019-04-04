# Data manipulation
install.packages("dplyr")
library("dplyr")
install.packages("knitr")
library("knitr")
install.packages("tidyr")
library("tidyr") 

# Cluster
install.packages("kohonen")
library("kohonen")
install.packages("NbClust")
library("NbClust")
install.packages("factoextra")
library("factoextra")

# Visualization
install.packages("ggplot2")
library("ggplot2")
install.packages("car")
library("car") # associated with the book 'An R Companion to Applied Regression', Third Edition, by John Fox and Sanford Weisberg.
install.packages("rgl")
library("rgl")


##### Number of Clusters
##----1.metod
set.seed(1)
nc <- NbClust(toplamtutar_rfm_table[,5:7], min.nc=2, max.nc=7, method="kmeans")

barplot(table(nc$Best.n[1,]), 
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by Criteria")

##---2.metod

set.seed(123)
fviz_nbclust(toplamtutar_rfm_table[,5:7], kmeans, method = "wss")

##---3.metod
# Alternative to silhouette
fviz_nbclust(toplamtutar_rfm_table[,5:7], kmeans, method = "silhouette")

####--------kmeans hesaplama-----------------------
toplamtutar_k2m <- kmeans(toplamtutar_rfm_table[,5:7], centers = 3, iter.max = 500, nstart = 10) 

cezatutar_k2m <- kmeans(acezatutar_rfm_table[,5:7], centers = 3, iter.max = 500, nstart = 25) 

require("factoextra")
fviz_cluster(toplamtutar_k2m, data = toplamtutar_rfm_table[,5:7])

fviz_cluster(cezatutar_k2m, data = acezatutar_rfm_table[,5:7])

###--------kmeans tablo ekleme----------------------
toplamtutar_rfm_table$clusterk <- as.factor(toplamtutar_k2m$cluster)

toplamtutar_k2m_sonuc <- data.frame(toplamtutar_k2m$centers,toplamtutar_k2m$size)
