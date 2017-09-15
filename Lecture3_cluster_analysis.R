#Лекция 3
#Кластерный анализ

#Данные по баскетболистам
basket <- read.csv(file = "https://raw.githubusercontent.com/kirillstudy/datasets/master/basketball.csv", sep = ";")
str(basket)
head(basket)
hist(basket$X52, breaks = 20)

#Визуализия
library(ggplot2)
ggplot(basket, aes(SF, X52))+
  geom_boxplot(aes(fill = SF))

ggplot(basket, aes(X52))+
  geom_histogram(data = basket[basket$SF == "C",], fill = "red", alpha = 0.2)+
  geom_histogram(data = basket[basket$SF == "G",], fill = "blue", alpha = 0.2)+
  geom_histogram(data = basket[basket$SF == "PF",], fill = "green", alpha = 0.2)+
  geom_histogram(data = basket[basket$SF == "SF",], fill = "yellow", alpha = 0.2)

#Иерархический кластерный анализ
#Число групп заранее неизвестно
beverage <- read.csv("https://raw.githubusercontent.com/kirillstudy/datasets/master/beverage.csv",
                     header = T, sep = ";")

beverage <- beverage[,-1]
head(beverage)
total_beverage <- sort(apply(beverage, 2, sum), decreasing = T) #Суммарнное количество людей, пивших напиток

#Создаем матрицу расстояний
dist_beverage_eucl <- dist(beverage)
dist_beverage_manh <- dist(beverage, method = "manhattan")

#Кластерный анализ
clust_bev_eucl <- hclust(dist_beverage_eucl, method = "ward.D") #Построение кластеров
clust_bev_manh <- hclust(dist_beverage_manh, method = "ward.D")

plot(clust_bev_eucl)                                            #Визуализация
rect.hclust(clust_bev_eucl, k = 3, border = "red")
groups_eucl <- cutree(clust_bev_eucl, k = 3)

plot(clust_bev_manh)
rect.hclust(clust_bev_manh, k = 3, border = "red")
groups_manh <- cutree(clust_bev_manh, k = 3)

beverage$group_eucl <- groups_eucl                              #Создание столбцов с номерами кластеров
beverage$group_manh <- groups_manh
nrow(beverage[beverage$group_eucl != beverage$group_manh,])     #Аккуратно!

pref_func <- function(df, clusters){
  result <- data.frame()
  for(i in (1:clusters)){
    result <- rbind(result, apply(df[df$group_eucl == i , c(1:8)], 2, mean))
  }
  names(result) <- names(beverage[, c(1:8)])
  return(result)
}

preferences <- pref_func(df = beverage, clusters = 3)           #Средние предпочтения в кластерах

#Каменистая осыпь
library(ggplot2)
length(clust_bev_eucl$height)
ggplot(data = data.frame(), aes(x = c(1:length(clust_bev_eucl$height)),
                                y = clust_bev_eucl$height)) + geom_line()
  qplot(x = c(1:length(clust_bev_eucl$height)), y = clust_bev_eucl$height)

  
#Кластеризация претендентов на работу
assess <- read.csv("https://raw.githubusercontent.com/kirillstudy/datasets/master/applicants_assess.csv",
                   header = T, sep = "\t")

dist_assess_eucl <- dist(assess)
clust_assess_eucl <- hclust(dist_assess_eucl, method = "ward.D")
plot(clust_assess_eucl)
rect.hclust(clust_assess_eucl, k = 4, border = "red")

assess$group <- cutree(clust_assess_eucl, k = 4)

assess_clusters <- data.frame(rbind(apply(assess[assess$group == 1 , c(3:12)], 2, mean),
                         apply(assess[assess$group == 2 , c(3:12)], 2, mean),
                         apply(assess[assess$group == 3 , c(3:12)], 2, mean),
                         apply(assess[assess$group == 4 , c(3:12)], 2, mean)))

install.packages("fmsb")
library(fmsb)
?radarchart
radarchart(assess_clusters, maxmin = F, title = "Skills assessment", plty = 1)

