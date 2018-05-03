library(FactoMineR)
library(caret)

nba <- read.table(file = 'final_shot_logs.csv', sep = ',', header = TRUE)
nba$period <- as.ordered(nba$period)
nba$shot_type <- as.ordered(nba$shot_type)
nba$shot_difficulty <- as.ordered(nba$shot_difficulty)
                                  
summary(nba)

trainIndex = createDataPartition(nba$success, p=0.7, list=FALSE, times=1)
testIndex = 
train = nba[trainIndex,]
test = nba[-trainIndex,]


# PRINCIPAL COMPONENTS ANALYSIS
cont_vars <- c(4, 5, 6, 7, 9, 10, 11, 14, 16, 17, 18, 21, 23)
cat_vars <- c(1, 2, 3, 8, 12, 13, 15, 19, 20, 22)

pca <- PCA(nba[, cont_vars], scale.unit = TRUE, 
           ind.sup = as.numeric(rownames(test)),
           quanti.sup = cat_vars, graph = FALSE)

# HIDDEN LATENT FACTORS


# CLUSTERING