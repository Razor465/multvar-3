# підключимо бібліотеки
library(dendextend)

# зчитуємо дані
data <- read.table('C:\\Users\\Razor\\Desktop\\дистанційне навчання\\статистичний аналіз багатовимірних даних\\lab3\\mult6.txt')

# застосуємо ієрархічну кластеризацію для методу одного зв'язку і евклідової відстані
h_single_eucl <- hclust(dist(data, method = 'euclidean'), method = 'single')
plot(h_single_eucl, labels = FALSE)

h_single_eucl_up <- cut(as.dendrogram(h_single_eucl), h = 5)$upper
labels(h_single_eucl_up) <- letters[1:14]
plot(h_single_eucl_up, main = 'Upper tree of cut (single/eucl)')
h_single_eucl_low <- cut(as.dendrogram(h_single_eucl), h = 5)$lower[[1]]
labels(h_single_eucl_low) <- NULL
plot(h_single_eucl_low, main = 'Lower tree of cut')

# застосуємо ієрархічну кластеризацію для методу повного зв'язку і евклідової відстані
h_compl_eucl <- hclust(dist(data, method = 'euclidean'), method = 'complete')
plot(h_compl_eucl, labels = FALSE)

h_compl_eucl_up <- cut(as.dendrogram(h_compl_eucl), h = 7)$upper
labels(h_compl_eucl_up) <- letters[1:14]
plot(h_compl_eucl_up, main = 'Upper tree of cut (complete/eucl)')
h_compl_eucl_low <- cut(as.dendrogram(h_compl_eucl), h = 7)$lower[[1]]
labels(h_compl_eucl_low) <- NULL
plot(h_compl_eucl_low, main = 'Lower tree of cut')

# застосуємо ієрархічну кластеризацію для методу середнього зв'язку і евклідової відстані
h_avg_eucl <- hclust(dist(data, method = 'euclidean'), method = 'average')
plot(h_avg_eucl, labels = FALSE)

h_avg_eucl_up <- cut(as.dendrogram(h_avg_eucl), h = 6)$upper
labels(h_avg_eucl_up) <- letters[1:14]
plot(h_avg_eucl_up, main = 'Upper tree of cut (average/eucl)')
h_avg_eucl_low <- cut(as.dendrogram(h_avg_eucl), h = 7)$lower[[1]]
labels(h_avg_eucl_low) <- NULL
plot(h_avg_eucl_low, main = 'Lower tree of cut')

# застосуємо ієрархічну кластеризацію для методу одного зв'язку і манхаттанської відстані
h_single_mannh <- hclust(dist(data, method = 'manhattan'), method = 'single')
plot(h_single_mannh, labels = FALSE)

h_single_mannh_up <- cut(as.dendrogram(h_single_mannh), h = 25)$upper
labels(h_single_mannh_up) <- letters[1:14]
plot(h_single_mannh_up, main = 'Upper tree of cut (single/mannh)')
h_single_mannh_low <- cut(as.dendrogram(h_single_mannh), h = 25)$lower[[1]]
labels(h_single_mannh_low) <- NULL
plot(h_single_mannh_low, main = 'Lower tree of cut')

# застосуємо ієрархічну кластеризацію для методу повного зв'язку і манхаттанської відстані
h_compl_mannh <- hclust(dist(data, method = 'manhattan'), method = 'complete')
plot(h_compl_mannh, labels = FALSE)

h_compl_mannh_up <- cut(as.dendrogram(h_compl_mannh), h = 40)$upper
labels(h_compl_mannh_up) <- letters[1:14]
plot(h_compl_mannh_up, main = 'Upper tree of cut (compl/mannh)')
h_compl_mannh_low <- cut(as.dendrogram(h_compl_mannh), h = 40)$lower[[1]]
labels(h_compl_mannh_low) <- NULL
plot(h_compl_mannh_low, main = 'Lower tree of cut')

# застосуємо ієрархічну кластеризацію для методу середнього зв'язку і манхаттанської відстані
h_avg_mannh <- hclust(dist(data, method = 'manhattan'), method = 'average')
plot(h_avg_mannh, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу одного зв'язку і максимальної відстані
h_single_max <- hclust(dist(data, method = 'maximum'), method = 'single')
plot(h_single_max, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу повного зв'язку і максимальної відстані
h_compl_max <- hclust(dist(data, method = 'maximum'), method = 'complete')
plot(h_compl_max, labels = FALSE)
abline(h = 2.5, col = 'red')
abline(h = 4.25, col = 'green')
abline(h = 3.5, col = 'blue')

# застосуємо ієрархічну кластеризацію для методу середнього зв'язку і максимальної відстані
h_avg_max <- hclust(dist(data, method = 'maximum'), method = 'average')
plot(h_avg_max, labels = FALSE)

#
# 2 частина
#

# зчитуємо дані
library(readxl)
data1 <- read_excel('data.xlsx')
rows <- t(data1[,1])
data1 <- data1[,-1]

# центрування і нормування

data1 <- as.data.frame(scale(data1))
row.names(data1) <- rows

# застосуємо ієрархічну кластеризацію для методу одного зв'язку і евклідової відстані
h_single_eucl1 <- hclust(dist(data1, method = 'euclidean'), method = 'single')
plot(h_single_eucl1, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу повного зв'язку і евклідової відстані
h_compl_eucl1 <- hclust(dist(data1, method = 'euclidean'), method = 'complete')
plot(h_compl_eucl1, labels = FALSE)

h_compl_eucl_up1 <- cut(as.dendrogram(h_compl_eucl1), h = 4.7)$upper
labels(h_compl_eucl_up1) <- letters[1:10]
plot(h_compl_eucl_up1, main = 'Upper tree of cut (complete/eucl)')
h_compl_eucl_low1 <- cut(as.dendrogram(h_compl_eucl1), h = 4.7)$lower[[1]]
labels(h_compl_eucl_low1) <- NULL
plot(h_compl_eucl_low1, main = 'Lower tree of cut')

# застосуємо ієрархічну кластеризацію для методу середнього зв'язку і евклідової відстані
h_avg_eucl1 <- hclust(dist(data1, method = 'euclidean'), method = 'average')
plot(h_avg_eucl1, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу одного зв'язку і манхаттанської відстані
h_single_mannh1 <- hclust(dist(data1, method = 'manhattan'), method = 'single')
plot(h_single_mannh1, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу повного зв'язку і манхаттанської відстані
h_compl_mannh1 <- hclust(dist(data1, method = 'manhattan'), method = 'complete')
plot(h_compl_mannh1, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу середнього зв'язку і манхаттанської відстані
h_avg_mannh1 <- hclust(dist(data1, method = 'manhattan'), method = 'average')
plot(h_avg_mannh1, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу одного зв'язку і максимальної відстані
h_single_max1 <- hclust(dist(data1, method = 'maximum'), method = 'single')
plot(h_single_max1, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу повного зв'язку і максимальної відстані
h_compl_max1 <- hclust(dist(data1, method = 'maximum'), method = 'complete')
plot(h_compl_max1, labels = FALSE)

# застосуємо ієрархічну кластеризацію для методу середнього зв'язку і максимальної відстані
h_avg_max1 <- hclust(dist(data1, method = 'maximum'), method = 'average')
plot(h_avg_max1, labels = FALSE)
