# Detection Outliers with K-means method 
In this code after the completion of the missing values, and the normalization process, the K-means method was used to remove the detected outliers. According to Yilmaz & others for removing outliers with the K-means method the following steps should be taken (Yilmaz et al.2014): 
- The first step in k-means is to pick the number of clusters.
- Then, the number of each class (patients with diabetes and patients without diabetes) is calculated in each cluster. 
- In the next step for each cluster, the rows belonging to the class with the lowest number detected as outliers and these rows should be deleted. 

Moreover, Kadhm et al who study on the "PIMA Indian Type-2 diabetes database”, proposed 10 clusters as the best cluster number for removing the outliers (Kadhm et al. 2018). Therefore, in this paper, 10 clusters have been used to remove outliers with the K-means method.
