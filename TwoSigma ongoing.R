rm(list = ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(xlsx)
require("tidyr")
library(data.table)
library(jsonlite)
require(devtools)
require(Rstem)
library(randomForest)

setwd("C:/Users/mpocchiari/Downloads/R tutorials csv files/Kaggle-master/Kaggle-master")

# Load and visualize 

d <- jsonlite::fromJSON("train.json", flatten = TRUE)
d <- as.data.table(d) # necessary to make it faster
d2 <- as.data.frame(d)

# what is the max number of features in the list?
max <- -1

for (i in 1:nrow(d2)) {
  if(max(length(d2[i, 7][[1]])) > max) {
    max = max(length(d2[i, 7][[1]]))
  }
}

max #39

# Unlist nested list of features

# Create empty matrix

d3 <- matrix(0, nrow = nrow(d2), ncol = max)

# Assign feature to a cell in the matrix

for (i in 1:nrow(d2)) {
  if(max(length(d2[i, 7][[1]] != 0))) { # if n of features != 0, 
    for (j in 1:max(length(d2[i, 7][[1]]))) { # go to column j looping on the max n of features
      d3[i, j] <- d2[i, 7][[1]][j]
    }
  }
}

features <- as.data.frame(d3)

for (i in 1:ncol(features)) {
  names(features)[i] <-  paste0("feature_", i)
}

features[features == 0] <- NA

final <- cbind(d2, features)

final <- final[, -c(7,12)]

# Cleaning data measurements levels

final$bathrooms <- as.numeric(final$bathrooms)
final$bedrooms <- as.numeric(final$bedrooms)
final$price <- as.numeric(final$price)
final$latitude <- as.numeric(final$latitude)
final$longitude <- as.numeric(final$longitude)
final$description <- as.character(final$description)
final$created <- as.Date.character(final$created)
df <- ldply(final$interest_level, data.frame)
final$interest_level <- as.factor(df[, 1])
final$interest_level <- as.numeric(final$interest_level)

final$respid <- 1000+seq.int(1:nrow(final))

idvars <- c(paste(names(final[, c(1:13, 53)]), sep = ","))

fin <- melt(final, id.vars = idvars)

fin$value <- as.character(fin$value)
fin$value <- tolower(fin$value)
fin$value <- gsub( "[^[:alnum:],]", "", fin$value)
fin$value <- as.factor(fin$value)

# select only features with most frequency

my.summary <- summary(fin$value)
freqfeatures<-data.frame(ids=names(my.summary), nums=my.summary)
ids=names(my.summary)
ids <- c(ids, "respid")

# dcast features on respid and select only columns of the most frequent features

al <- dcast(fin, respid ~ value, value.var="value", fun.aggregate = length)
al <- al[names(al) %in% ids]

final <- final[,-c(14:53)]

###################################################################################
################################                ################################### 
################################  FINAL DATASET ###################################
################################                ###################################
###################################################################################

dfs <- cbind(final, al)

rm(list = c("al", "d", "d2", "df", "features", "fin", "final", "freqfeatures"))


###################################################################################
################################                ################################### 
################################  VISUALIZATION ###################################
################################                ###################################
###################################################################################

#### Target variable

ggplot(dfs, aes(x = interest_level)) + 
  geom_bar()

# It has a lot of data in the middle

dfs$interest_level.s <- scale(dfs$interest_level, scale = T, center = T)

#### Price ~ Interest

ggplot(dfs, aes(x = price)) +
  geom_density()
# price is very skewed, needs recode into log

dfs$price.log <- log(dfs$price)

ggplot(dfs, aes(x = price.log, y = interest_level.s)) +
  geom_jitter()+
  geom_smooth(method = "lm", se = T)

ggplot(dfs, aes(x = as.factor(interest_level.s), y = price.log)) +
  geom_boxplot()

# quality check, price vs n bedrooms and bathrooms

dfs$bb <- dfs$bedrooms + dfs$bathrooms

ggplot(dfs, aes(x = bb, y = price.log)) +
  geom_point()+
  geom_smooth(method = "lm", se = T)
# ok, makes quite sense

# other check, location

require(rworldmap)
require(mapproj)
require("ggmap")

mymap <- ggmap::get_map(location = "New York", maptype = "roadmap", zoom = 12)
ggmap(mymap)

dfs$latitude <- as.numeric(dfs$latitude)
dfs$longitude <- as.numeric(dfs$longitude)

ggmap(mymap) +
  geom_point(aes(x = longitude, y = latitude, color = price.log, group = price.log), #alpha = 0.5),
             data = dfs)

# There doesn't seem to be a clear relationship 

#### Date ~ Interest (seasonality)

ggplot(dfs, aes(x = created, y = interest_level.s)) +
  geom_jitter()
# nope

#### Features ~ Interest

# bathrooms
ggplot(dfs, aes(x = bathrooms, y = interest_level.s)) +
  geom_jitter()+
  geom_smooth(method = "lm")

# bedrooms

ggplot(dfs, aes(x = bedrooms, y = interest_level.s)) +
  geom_jitter()+
  geom_smooth(method = "lm")

# not great patterns

# total n features

for (i in 15:ncol(dfs)) {
  dfs[, i] <- as.numeric(dfs[, i])
}
dfs$total.features <- rowSums(dfs[, 15:ncol(dfs)])

ggplot(dfs, aes(x = total.features, y = interest_level.s)) +
  geom_jitter() + 
  geom_smooth(method = "lm")

# clear positive pattern, more features -> more interest


#### Correlation across features

cov.matrix <- as.data.frame(cor(dfs[, 15:112]))
lower <- as.data.frame(lower.tri(cov.matrix))

for (i in 1:nrow(cov.matrix)) {
  for (j in 1:ncol(cov.matrix)) {
    if (lower[i,j] == FALSE) {
      cov.matrix[i,j] <- 0
    } 
  }
}

write.xlsx(cov.matrix, "covariance features lower triangle.xlsx", sheetName = "cov")

vec <- rep(0, 102)

covar <- as.data.frame(cov.matrix)
covar$colnames <- rownames(covar)

covar <- melt(covar, id.vars = "colnames")
#covar <- covar[covar$value > 0,]
covar <- covar[covar$value > 0.20 | covar$value < -0.20, ]
covar <- covar[covar$value > 0.50 | covar$value < -0.50, ]

library(dplyr)

covar <- covar %>% arrange(desc(value))



clean$manager_id <- as.factor(unlist(clean$manager_id))
clean$building_id <- as.factor(unlist(clean$building_id))


###################################################################################
################################                ################################### 
################################    CLEANING    ###################################
################################                ###################################
###################################################################################


# exclude columns that do not contribute to prediction

exclude_cols <- c("building_id","display_address","listing_id")
subs <- dfs[!names(dfs) %in% exclude_cols ]

dfs$manager_id <- as.numeric(unlist(dfs$manager_id))
dfs$street_address <- as.numeric(dfs$street_address)
dfs$display_address <- as.numeric(dfs$display_address)
dfs$building_id <- NULL
dfs$coordinates <- paste(as.numeric(dfs[, "latitude"]), as.numeric(dfs[, "longitude"]), collapse = ",")

vec <- unique(covar$colnames)

# remove the features that are highly correlated with others (stored in vec)

clean <- dfs[, -which(names(dfs) %in% c("washerinunit","dogsallowed","simplex","marblebath","lowrise","light","subway",
                                        "virtualdoorman","gymfitness","luxurybuilding","residentslounge","renovated","hardwoodfloors",
                                        "elevator","fitnesscenter","wifiaccess","loungeroom","fulltimedoorman","walkinclosets",
                                        "laundryinbuilding","valet","onsitegarage","stainlesssteelappliances"))]
clean <- clean[, c(13, 9,14:ncol(clean))]

########################################################################################
################################                     ################################### 
################################  SAVED FOR LATER    ###################################
################################                     ###################################
########################################################################################


kiva_rf_9 <- randomForest(interest_level.s ~ . -respid -manager_id, 
                          data = clean, ntree = 1000,
                          importance = TRUE, mtry = 9,na.rm = TRUE)

ddf <- data.frame(rf_9 = kiva_rf_9$err.rate[, "OOB"],
                  trees = 1:1000)

min(kiva_rf_9$err.rate[, "OOB"])

df_melt <- melt(ddf, id.vars = "trees")
ggplot(df_melt, aes(x = trees, y = value, group = variable, colour = variable)) +
  geom_line() +
  labs(title = "OOB Error Estimate", x = "Trees", y = "Error")+
  ylim(0.008, 0.009)+
  theme(axis.text.x = element_text(size = 12))

## Plot permutation variable importance using ggplot
imp <- data.frame(Importance = kiva_rf_9$importance[, "MeanDecreaseAccuracy"],
                  Variable = rownames(kiva_rf_9$importance))