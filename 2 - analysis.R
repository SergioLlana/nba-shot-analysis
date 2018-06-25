library(FactoMineR)
library(factoextra)
library(corrplot)
library(caret)
library(calibrate)

set.seed(514124)

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}
nba <- loadRData("data/final_shot_logs.RData")
summary(nba)



###################################################
## 1 - PRINCIPAL COMPONENTS ANALYSIS
###################################################

# PCA
#   - Active variables:        is_home (3), shot_number (6), period (7), remaining_seconds (8), shot_clock (9), shot_dist (10),
#                              touch_time (12), defender_dist(14), dribbles (15), clutch (19), salary (21), shooter_pct (22),
#                              defender_pct (23), shot_cat_pct (24), shot_difficulty (25), position_pct (26), triple (27)
#   - Supplementary variables: shooter (11), defender (13), shot_difficulty (17), shot_cat (18), position (20)
#   - Response variable:       success (16)
# Although victory and final_margin are continuous variables, they are not taken into account as they are known at the end of the match.
# Team codes are useless.
nba <- nba[,c(16, 3, 6:10, 12, 14:15, 19, 21:27, 11, 13, 17, 18, 20)]

trainIndex <- createDataPartition(nba$success, p=0.7, list=FALSE, times=1)
train <- nba[trainIndex,]
test <- nba[-trainIndex,]
save(train, file = "data/train.RData")
save(test, file = "data/test.RData")

active.vars <- c(2:18)
sup.vars <- c(1, 19:23)

# Apply (standardized) PCA to the active individuals and variables
pca <- PCA(nba, scale.unit = TRUE, 
           ncp = 9,
           ind.sup = as.numeric(rownames(test)),
           quali.sup = sup.vars, graph = FALSE)

# Visualize eigenvalues in a screeplot                      DECIDE NUMBER OF SIGNIFICANT PC!
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 35), ncp = 15)
# Using the Last Elbow rule, it looks as though the number of significant components is 9
sc <- 9

# VARIABLE ANALYSIS
# Quality of representation of each variable for each PC (based on the cos2, squared coordinates)
var <- get_pca_var(pca)
corrplot(var$cos2, is.corr=FALSE)

# Representation of variables in the first factorial plane
fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_var(pca, axes = c(2,3), col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# dimdesc(pca, axes = c(1,2))      'x' must contain finite values only!!! 

# INDIVIDUAL ANALYSIS
fviz_pca_ind(pca, col.ind = "cos2", 
             invisible = "ind.sup", # SHOULD WE SHOW TEST INDIVIDUALS?
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = c("point"))

fviz_pca_ind(pca, axes = c(2,3), col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = c("point"))

fviz_pca_ind(pca, geom = c("point"), col.ind = factor(train$success), invisible = "ind.sup")

# BIPLOT
fviz_pca_biplot(pca, repel = TRUE,
                geom = c("point"),
                invisible = "ind.sup", # SHOULD WE SHOW TEST INDIVIDUALS?
                col.var = "black", # Variables color
                col.ind = "red"  # Individuals color
)


# HIDDEN LATENT FACTORS

# VARIMAX ROTATION

Psi <- pca$ind$coord
Phi <- pca$var$coord
X <- train[,active.vars]
Xs <- scale(X)

pc.rot <- varimax(Phi)

Phi.rot <- pc.rot$loadings
lmb.rot <- diag(t(Phi.rot) %*% Phi.rot)

Psi_stan.rot <- Xs %*% solve(cor(X)) %*% Phi.rot
Psi.rot <- Psi_stan.rot %*% diag(sqrt(lmb.rot))

plot_variables_factor_map <- function(coords, title, xdim=1, ydim=2) {
  plot(0, 0, col = "white", asp=1, xlab=paste("Dim", toString(xdim)), ylab=paste("Dim", toString(ydim)), main=title)
  x.cercle <- seq(-1, 1, by = 0.01)
  y.cercle <- sqrt(1 - x.cercle^2)
  lines(x.cercle, y = y.cercle)
  lines(x.cercle, y = -y.cercle)
  abline(h=0, lty=2)
  abline(v=0, lty=2)
  
  for (var in c(1:nrow(coords))) {
    arrows(0, 0, x1 = coords[var,xdim], y1 = coords[var,ydim], length = 0.1, angle = 30, col="red")
    text(x=coords[,xdim], y=coords[,ydim], labels=row.names(coords), pos=ifelse(coords[,ydim] > 0, 3, 1))
  }
}

par(mfrow=c(1,2))

plot_variables_factor_map(Phi, "Variables factor map (PCA)")
plot_variables_factor_map(Phi.rot, "Rotated variables factor map (PCA)")

plot_variables_factor_map(Phi, "Variables factor map (PCA)", xdim = 2, ydim = 3)
plot_variables_factor_map(Phi.rot, "Rotated variables factor map (PCA)", xdim = 2, ydim = 3)

plot(Psi)
plot(Psi.rot)

plot(Psi[,c(2,3)])
plot(Psi.rot[,c(2,3)])


# A latent factor is easily interpreted when it just correlates with some variables and it is uncorrelated with the remaining. 
# Principal Components are the factors explaining at most the total variance of the original cloud of points.
# But, not necessarily they clearly identify the underling factors existing in your data.

# In our case though, the better explained variables clearly correlate with the principal components, and therefore no rotation of
# the dimensions is needed. Interpret principal components...




# CLUSTERING OF LARGE DATA SETS
# FIRST 2 KMEANS WITH K=14


## Decide whether to use the original cloud of individuals or the rotated one. 

n1 <- 14
k1 <- kmeans(Psi.rot,n1)
k2 <- kmeans(Psi.rot,n1)
table(k2$cluster,k1$cluster)
clas <- (k2$cluster-1)*n1+k1$cluster
freq <- table(clas)   # WHAT DO WE HAVE IN VECTOR freq?
cdclas <- aggregate(as.data.frame(Psi.rot),list(clas),mean)[,2:(sc+1)]

# SECOND HIERARCHICAL CLUSTERING UPON THE CENTROIDS OF CROSSING THE 2 KMEANS PARTITIONS
d2 <- dist(cdclas)
h2 <- hclust(d2,method="ward.D2",members=freq)  # COMPARE THE COST
plot(h2)
abline(h=11.2, col = "red")
barplot(h2$height[(length(h2$height)-15):length(h2$height)],
        ylim = c(0,30), main = "Aggregated distance at the last iterations",
        xlab = "Iterations", ylab = "Height")
abline(v=15.7, col = "red")

nc <- 4   # for instance
c2 <- cutree(h2,nc)
cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:(sc+1)]  # WHY WEIGHT

# CONSOLIDATION
k4 <- kmeans(Psi,centers=cdg)
Bss <- sum(rowSums(k4$centers^2)*k4$size)
Wss <- sum(k4$withinss)
Ib4 <- 100*Bss/(Bss+Wss)
Ib4

colors <- c("black", "red", "green", "blue")
palette(colors)
clusters <- factor(k4$cluster, labels = colors)

par(mfrow=c(1,1))
plot(Psi.rot, col = clusters, main = "Clusters after consolidation", xlab = "Dim 1", ylab = "Dim 2")
abline(h=0,v=0,col="gray")

plot(Psi[,2:3], col = clusters)

catdes.res <- catdes(cbind(clusters, train), 1)
catdes.res

catdes.res$category$black[1:6,]
rbind(head(catdes.res$quanti$black, 4), tail(catdes.res$quanti$black, 3))

catdes.res$category$red[1:5,]
rbind(head(catdes.res$quanti$red, 3), tail(catdes.res$quanti$red, 1))

catdes.res$category$green[1:6,]
rbind(head(catdes.res$quanti$green, 4), tail(catdes.res$quanti$green, 4))

catdes.res$category$blue[1:4,]
rbind(head(catdes.res$quanti$blue, 2), tail(catdes.res$quanti$blue, 2))
