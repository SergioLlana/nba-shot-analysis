library(stringdist)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rvest)
library(class)
library(mice)
library(VIM)
library(FactoMineR)


nba <- read.table(file = 'data/shot_logs.csv', sep = ',', header = TRUE)


## DATA SELECTION
# 1- Split "MATCHUP" into date, home_team and away_team
nba <- extract(nba, 'MATCHUP', into=c('date', 'away_team', 'home_team'), 
               '(.*) - ([a-zA-Z]{3}) (?:@|vs\\.) ([a-zA-Z]{3})', remove=TRUE)

# 2- Parse date to YYYY-MM-DD
date <- format(mdy(nba$date),"%Y-%m-%d")

# 3- Turn "GAME_CLOCK" into "remaining_seconds". As it could be better understood by models.
to_seconds <- function(game_clock) {
  minutes_seconds <- unlist(strsplit(as.character(game_clock), ":"))
  minutes <- as.integer(minutes_seconds[1])
  seconds <- as.integer(minutes_seconds[2])
  return(seconds + (minutes*60))
}

remaining_seconds <- sapply(nba$GAME_CLOCK, to_seconds)

# 4- Transform distances from feets to meters.
shot_dist <- nba$SHOT_DIST * 0.3048
defender_dist <- nba$CLOSE_DEF_DIST * 0.3048

# 5- Adjust data types
success <- as.integer(nba$SHOT_RESULT == 'made')
is_home <- as.integer(nba$LOCATION == 'H')
shot_number <- as.ordered(nba$SHOT_NUMBER)
shot_type <- as.ordered(nba$PTS_TYPE)
victory <- as.integer(nba$W == 'W')
period <- as.ordered(nba$PERIOD)

# 6- Rename columns
nba.1 <- data.frame(date, home_team=nba$home_team, away_team=nba$away_team, is_home, victory, final_margin=nba$FINAL_MARGIN, 
                    shot_number, period, remaining_seconds, shot_clock=nba$SHOT_CLOCK,shot_dist, shot_type, shooter=nba$player_name, 
                    touch_time=nba$TOUCH_TIME, defender=nba$CLOSEST_DEFENDER, defender_dist, dribbles=nba$DRIBBLES, success)

summary(nba.1)


## DATA CLEANSING AND EXPLORATION
rm(list=setdiff(ls(), "nba.1"))

# 1- final_margin vs victory.
# There are no wrong values in the interaction of these two variables.
# All games that end up with a positive margin must have been won by shooter
par(mfrow=c(1,2))
hist(nba.1$final_margin[nba.1$victory==TRUE], main="Victories", xlab="Magin of victory")
hist(nba.1$final_margin[nba.1$victory==FALSE], main="Losses", xlab="Magin of defeat")

# 2- touch_time
# There are 312 observations with a negative "touch_time", which does not have any sense.
# For the moment, I have decided to change those values to NA and imputed using 1NN.
# In addition, there are some values with "touch_time" higher than 24, which cannot be possible.
# As its values are pretty close to 24, I have rounded them to 24.
par(mfrow=c(1,1))
boxplot(nba.1$touch_time, main="Boxplot of touch_time", xlab="Touch time")

length(which(nba.1$touch_time > 24))
nba.1$touch_time[nba.1$touch_time > 24] <- 24

length(which(nba.1$touch_time < 0))
nba.1$touch_time[nba.1$touch_time < 0] <- NA

aggr_plot <- aggr(nba.1, col=c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

par(mfrow=c(1,2))
boxplot(nba.1$touch_time, main="Boxplot of touch_time", xlab="Touch time")
hist(nba.1$touch_time, main="Histogram of touch_time", xlab="Touch time")

# 3- defender_dist
# There are no wrong values.
boxplot(nba.1$defender_dist, main="Boxplot of defender_dist", xlab="Defender distance (m.)")
hist(nba.1$defender_dist, main="Histogram of defender_dist", xlab="Defender distance (m.)")

# 4- shot_dist vs shot_type
par(mfrow=c(1,1))
ggplot(nba.1, aes(x=shot_dist)) + ggtitle("Shot distance vs Shot type") + geom_density(aes(group=shot_type, colour=shot_type, fill=shot_type), alpha=0.3) + geom_vline(xintercept=7.24)

par(mfrow=c(1,2))
hist(nba.1$shot_dist[nba.1$shot_type==2], main="2-pointers histogram", xlab="Meters")
hist(nba.1$shot_dist[nba.1$shot_type==3], main="3-pointers histogram", xlab="Meters")

length(which(nba.1$shot_dist[nba.1$shot_type==2] >= 23.9))
length(which(nba.1$shot_dist[nba.1$shot_type==3] > 23.9))

# 5- shot_clock vs remaining_seconds
# Weird spike when shot_clock=24, which can be explained easily if we take a look at which players did those kind of shots (https://rpubs.com/BruinRisk/NBA_Shot_Log)
# It is caused by shots done after an offensive rebound (when the clock isn't reset), as they are done by pivots.
# After looking at a few of these, a fan of the game would realize these are all big men playing close to the hoop. Domain knowledge can lead to the conclusion that these are tip-ins/ put-backs where a player near the hoop collects an offensive rebound after a teammate missed a shot. In this situation, the shot clock would reset on hitting the rim!
# In addition, there are 5567 NAs. Most of them occur when game_clock is lower than shot_clock and therefore the shot_clock is turned off, because it has no importance anymore.
# As they are only a 4.34% of the observations, we could delete them from the dataset, but as we don't know which is the goal of this data pre-processing yet, we will handle them later.
# Removing them will make us loose data on significant shots for future anylisis that are not affected by this feature.
par(mfrow=c(1,1))
hist(nba.1$shot_clock, main="Histogram of shot_clock", xlab="Shot clock (s.)") 
summary(nba.1$shot_clock)

nba.1 %>%
  filter(shot_clock > 23) %>%
  group_by(shooter) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

shot_clock_na <- nba.1 %>%
  filter(is.na(shot_clock) == T) %>%
  group_by(remaining_seconds) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(shot_clock_na, aes( x = remaining_seconds, y = count )) + 
  geom_area(stat = 'identity') + 
  coord_cartesian(xlim = c(0,100)) +
  labs(title = 'Distribution of NAs in shot_clock vs remaining_seconds')

## FEATURE CREATION
# 1- Shot difficulty (https://stats.nba.com/players/catch-shoot/)
nba.1$shot_difficulty <- 'Open'
nba.1$shot_difficulty[nba.1$defender_dist <= 3.5*0.3048] <- 'Contested'
nba.1$shot_difficulty[nba.1$defender_dist <= 2*0.3048] <- 'Tightly Contested'
nba.1$shot_difficulty[nba.1$defender_dist > 6*0.3048] <- 'Wide Open'
nba.1$shot_difficulty <- as.factor(nba.1$shot_difficulty)

# 2- Shot type
nba.1$shot_cat <- 'Other'
nba.1$shot_cat[nba.1$dribbles <= 1 & nba.1$shot_dist  > 4*0.3048] = 'Catch&Shoot'
nba.1$shot_cat[nba.1$dribbles <= 1 & nba.1$shot_dist <= 4*0.3048] = 'Cut'
nba.1$shot_cat[nba.1$dribbles > 1 & nba.1$shot_dist  <= 4*0.3048] = 'Drive'
nba.1$shot_cat[nba.1$dribbles > 4] = 'ISO/Post up'
nba.1$shot_cat[nba.1$dribbles > 20] = 'Long ISO'
nba.1$shot_cat[nba.1$dribbles <=1 & nba.1$shot_type == 3] = 'Spot Up Three'
nba.1$shot_cat <- as.factor(nba.1$shot_cat)

# 3- Clutch situations
nba.1$clutch <- 0
nba.1$clutch[nba.1$remaining_seconds < 60 & nba.1$period >= 4 & abs(nba.1$final_margin) <= 5] <- 1


# EXPORTING
write.csv(nba.1, file = 'data/preprocessd_shot_logs.csv', row.names=FALSE)
summary(nba.1)
rm(list=setdiff(ls(), "nba.1"))


# MERGE WITH SALARIES DATASET
salaries <- data.frame(name=character(), 
                       position=character(), 
                       salary=numeric()) 

url <- "http://www.espn.com/nba/salaries/_/year/2015/page/%d"

for(page in 1:11) {
  webpage <- read_html(sprintf(url, page))
  tmp_salaries <- webpage %>%
    html_nodes(xpath="//table[@class='tablehead']") %>%
    html_table()
  
  tmp_salaries <- tmp_salaries[[1]]
  
  # Remove rows that contain RK in the first column (headers)
  tmp_salaries <- tmp_salaries[tmp_salaries$X1 != 'RK', ]
  
  # Split second column by comma
  tmp_salaries <- extract(tmp_salaries, 'X2', into=c('name', 'position'), '(.*), ([a-zA-Z]*)')
  tmp_salaries$name <- tolower(tmp_salaries$name)
  tmp_salaries$position <- as.factor(tmp_salaries$position)
  
  # Clean last column (salary)
  tmp_salaries$salary <- as.numeric(gsub('[$,]', '', tmp_salaries$X4))
  
  # Add to output DF
  salaries <- rbind(salaries, tmp_salaries[, c(2, 3, 6)])
}

nba.2 <- full_join(nba.1, salaries, by = c("shooter" = "name"))

names1 <- unique(nba.2[is.na(nba.2$salary), 13])
names2 <- nba.2[is.na(nba.2$date), 13]
matches <- amatch(names1, names2, maxDist = 2, nomatch = 0)

for (idx_names1 in 1:length(matches)) {
  idx_names2 <- matches[idx_names1]
  if (idx_names2 > 0) {
    nba.2[nba.2$shooter == names1[idx_names1], c("position", "salary")] <- salaries[salaries$name == names2[idx_names2], c("position", "salary")]
  }
}

nba.2[nba.2$shooter == "otto porter", c("position", "salary")] <- salaries[salaries$name == "otto porter jr.", c("position", "salary")]
nba.2[nba.2$shooter == "jose juan barea", c("position", "salary")] <- salaries[salaries$name == "j.j. barea", c("position", "salary")]
nba.2 <- nba.2[!is.na(nba.2$date) & !is.na(nba.2$salary), ]

## MISSING VALUE HANDLING
## NAs in shot_clock
# There are 5567 NAs. Most of them occur when remaining_seconds < shot_clock and therefore the shot_clock is turned off, 
# because it has no importance anymore. These ones will be imputed by a random number from a uniform distribution in the
# range of [0, remaining_seconds]. However, the missing values with remaining_seconds >= shot_clock will be removed.
nba.3 <- filter(nba.2, !is.na(shot_clock) | remaining_seconds < 24)
nba.3[is.na(nba.3$shot_clock), "shot_clock"] <- sapply(nba.3[is.na(nba.3$shot_clock), "remaining_seconds"],
                                                       function(x) round(runif(1, 0, x), digits=1))

## NAs in touch_time
# Create variable whether it is negative or not and apply catdes to see if it is related with certain 
# values in some other feature. Otherwise apply 1 NN.
not_needed_columns <- c("touch_time", "shooter")
result <- catdes(cbind(touch_time_missing=as.factor(is.na(nba.3$touch_time)),
                       nba.3 %>% select(-one_of(not_needed_columns))), 1)
(result$quanti)
# We see in the results that the shots with missing touch_time have a significantly lower and strangely
# exact (0.3) ratio of success compared to the overall mean (0.45), but most importantly, the mean of 
# the number of dribbles before the shot is astonishingly lower than the overall mean (0.007 vs 2.05).
(result$category)
# In the categorical variables we see that the amount of shots of the categories "Cut" and "Catch&Shoot"
# is much higher than the average, as well as the positions "PF" and "C" and the difficulty "Tightly Contested".
# On the other hand, the shot categories "Drive", "ISO/Post up" and "Other" appear fewer times than average
# as well as the position "PG" and the difficulty "Open"

# Seeing this result one interpretation could be that the shots with missing values in touch_time 
# correspond mostly to quick plays where probably this time wasn't measured correctly due to the 
# celerity (swiftness) of the game.

# Options:
#   - Impute random low values following the conclusions extracted after the catdes (maybe too naive)
#   - Impute values using K-NN (with K = 1) and supervise that the values imputed are generally lower 
#     than average, so as to satisfy our conclusions.

# Should we just not use the categorical variables, or should we try to convert them into integers?
# Maybe there are some variables that could be "continuousified" (like shot_difficulty, whose values can
# be ordered), but not all of them (player's names...)
nba.train.touch_time <- nba.3[!is.na(nba.3$touch_time),] %>% 
  select(-one_of("touch_time")) %>%
  select_if(is.numeric)

nba.test.touch_time <- nba.3[is.na(nba.3$touch_time),] %>% 
  select(-one_of("touch_time")) %>%
  select_if(is.numeric)

nba.true_classifications.touch_time <- nba.3$touch_time[!is.na(nba.3$touch_time)]
nba.knn <- knn(nba.train.touch_time, nba.test.touch_time, nba.true_classifications.touch_time)

# The mean is lower in the imputed values, so our hypothesis holds
paste("Mean of the imputed values:", mean(as.numeric(as.character(nba.knn))))
paste("Mean of the original values:", mean(nba.3$touch_time[!is.na(nba.3$touch_time)]))

nba.3$touch_time[is.na(nba.3$touch_time)] <- as.numeric(as.character(nba.knn))


# EXPORTING AGAIN
write.csv(nba.3, file = 'data/final_shot_logs.csv', row.names=FALSE)
summary(nba.3)
rm(list=setdiff(ls(), "nba.3"))

