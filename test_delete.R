avg_sil <- function(k) {
  km.res <- kmeans(df1, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df1))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:10

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")



fviz_nbclust(df1, kmeans, method = "silhouette")


df <- read_csv("data/Mall_Customers.csv")

df1 <- df %>% select(Age,`Annual Income (k$)`,`Spending Score (1-100)`)

df1 <- df1 %>% mutate(across(where(is.numeric), scale))

fviz_nbclust(df1, kmeans, method = "wss")
fviz_nbclust(df1, kmeans, method='silhouette')
fit <- kmeans(df1, 8)
fviz_cluster(fit, data = df1)

wss<- NULL

#Feeding different centroid and record WSS

for (i in 1:10){
  fit = kmeans(df1,centers = i)
  wss = c(wss, fit$tot.withinss)
}

#Visualize the plot
plot(1:10, wss, type = "o", xlab='Number of clusters(k)')


load(CAMIS)


df <- read_csv("data/Mall_Customers.csv")
#Rename the columns
df <- df %>% 
  rename("Annual_Income"= `Annual Income (k$)`, "Spending_score"= `Spending Score (1-100)`)

#remove rows with missing values
df <- na.omit(df)

#scale each variable to have a mean of 0 and sd of 1
df1 <- df %>% 
  mutate(across(where(is.numeric), scale))

cluster_data_mod <-df %>% mutate(Genre=as.factor(Genre)) %>% select(-c(CustomerID, Age))


#gower distance

gower_dist <- daisy(cluster_data_mod,
                    metric = "gower",
                    type = list(logratio = 3))

summary(gower_dist)



# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)



#clustering

pam_fit <- pam(gower_dist, diss = TRUE, k = 4)

pam_results <- cluster_data_mod %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


#visualize

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


fviz_cluster(pam_results, 
             palette =c("#007892","#D9455F"),
             ellipse.type ="euclid",
             repel =TRUE,
             ggtheme =theme_minimal())
