

setwd("F:/All Files/Great Lakes/All Courses/3-Data Mining")

custSpendData = read.csv("Cust_Spend_Data.csv",header = TRUE)

print(custSpendData)

distMatrix = dist(custSpendData[,3:7],method = "euclidean")

print(distMatrix, digits = 3)

custSpendData.Scaled = scale(custSpendData[,3:7])    

print(custSpendData.Scaled)   

apply(custSpendData.Scaled,2,mean)

apply(custSpendData.Scaled,2,sd)

apply(custSpendData.Scaled,1,mean)

apply(custSpendData.Scaled,1,sd)

distMatrix.Scaled = dist(custSpendData.Scaled,method = "minkowski", p=2)

print(distMatrix.Scaled)

cluster = hclust(distMatrix.Scaled, method = "average")

plot( cluster, labels = as.character(custSpendData[,2]))

cluster$height

plot( cluster, labels = as.character(custSpendData[,2]))

rect.hclust( cluster, k=2, border = "red" )

rect.hclust( cluster, k=3, border = "red" )

custSpendData$Cluster = cutree(cluster,k=3)

print( custSpendData )

custProfile = aggregate( custSpendData[, -c(1,2,8) ],list(custSpendData$Cluster), FUN = "mean")

print(custProfile)

