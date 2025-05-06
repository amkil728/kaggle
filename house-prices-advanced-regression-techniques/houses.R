library(tidyverse)

# set directory and read data

getwd()
setwd("~/Documents/house-prices-advanced-regression-techniques/")

house.data <- read.csv("train.csv", stringsAsFactors = TRUE)
# data <- data %>% mutate(across(where(is.factor), addNA))


# EDA


head(house.data)
colnames(house.data)
summary(house.data)
# 
# house.data |> select(where(is.integer))
# df <- house.data |> select(!where(is.factor)) |> na.omit()
# df[] <- scale(df)
# 
# ggplot(
#   data,
#   aes(x = HasPool, y = SalePrice)
# ) +
#   geom_jitter(width=0.01)
# 
# 
# nrow(data[is.na(data$Fence),])
# data$HasFence <- ifelse(is.na(data$Fence), FALSE, TRUE)
# 
# ggplot(
#   data,
#   aes(x = HasFence, y = SalePrice)
# ) +
#   geom_jitter(width=0.01)
# 
# data |> filter(HasFence) |> 
#   summarise(min(SalePrice))
# data |> filter(HasFence) |> 
#   summarise(max(SalePrice))
# data |> filter(!HasFence) |> 
#   summarise(min(SalePrice))
# data |> filter(!HasFence) |> 
#   summarise(max(SalePrice))
# 
# data[data$HasFence,]
# 
# ggplot(
#   data,
#   aes(x = log(LotArea), y = SalePrice)
# ) +
#   geom_point()
# 
# ggplot(
#   data,
#   aes(x = log(LotArea), y = SalePrice)
# ) +
#   geom_point() +
#   geom_abline(slope = coef(model)[2], intercept = coef(model)[1])
# 
# # model <- lm(SalePrice ~ log(LotArea), data = data)
# # coef(model)
# # 
# # plot(model)
# # 
# # data_filtered <- data |> filter(SalePrice <= 5e5, log(LotArea) < 11)
# # 
# # ggplot(
# #   data_filtered,
# #   aes(x = log(LotArea), y = SalePrice)
# # ) +
# #   geom_point()
# # 
# # model <- lm(SalePrice ~ log(LotArea), data = data_filtered)
# # coef(model)
# # 
# # ggplot(
# #   data_filtered,
# #   aes(x = log(LotArea), y = SalePrice)
# # ) +
# #   geom_point() +
# #   geom_abline(slope = coef(model)[2], intercept = coef(model)[1])
# # 
# # plot(model)
# # 
# # 
# # # Pool
# # 
# # ggplot(
# #   data = data,
# #   aes(x = PoolArea, y = SalePrice, colour = PoolQC)
# # ) +
# #   geom_point()
# # 
# # 
# # model <- lm()
# 
# # Test-train split
# 
# set.seed(1024)
# 
# train_idx <- sample(nrow(df), ceiling(nrow(df) * 0.8))
# train_data <- df[train_idx,]
# test_data <- df[-train_idx,]
# 
# # Linear Regression
# x.train <- train_data |> select(!SalePrice) |> as.matrix() |> unname()
# solve(t(x.train) %*% x.train)
# 
# fit <- lm(SalePrice ~ . - GrLivArea - TotalBsmtSF,
#           train_data)
# fit$coefficients
# 
# mean((predict(fit, newdata=test_data) - test_data$SalePrice) ^ 2)
# 
# plot(fit)


# house.data$LotFrNA <- is.na(house.data$LotFrontage)
# house.data <- house.data |> select(!LotFrNA)
# house.data$LotFrontage[is.na(house.data$LotFrontage)] <-
#   mean(na.omit(house.data$LotFrontage))
# 
# ggplot(data = house.data, aes(x = LotFrNA, y = LotArea)) +
#   geom_boxplot()


# prepare data

work_data <- house.data |> select(!Id)
work_data |> summarise(across(everything(), ~ sum(is.na(.))))

work_data <- work_data |> 
  mutate(across(
    where(~ is.factor(.) & sum(is.na(.) > 0)), addNA))
work_data <- work_data |> mutate(across(is.numeric, ~ replace_na(., 0)))

# work_data <- drop_na(work_data)
# work_data <- work_data |> mutate(y = log(SalePrice)) |> select(!SalePrice)
work_data <- work_data |> mutate(y = SalePrice) |> select(!SalePrice)

# work_data <- house.data |> select(where(is.numeric)) |> select(!LotFrontage & !Id)
# work_data |> mutate(log(LotArea)) |> select(!LotArea)

y <- work_data$y
X <- work_data |> select(!y)

replaceLowLevels <- function(x, new, threshold) {
  levels(x) <- c(levels(x), new)
  
  for (lv in levels(x)) {
    # if (!is.na(lv)) { 
      if (sum(x == lv) < threshold * length(x)) {
        x[x == lv] <- new
      }
    }
  }
}

# normalise numeric columns
X_num <- work_data |> select(where(is.numeric))
X_num <- data.frame(scale(X_num))
X_fac <- work_data |> select(where(is.factor))

for (c in colnames(X_fac)) {
  if (sum(is.na(X_fac[c])) > 0) {
    levels(X_fac[c]) <- c(levels(X_fac[c]), "NONE")
    X_fac[is.na(X_fac$c), c] <- "NONE"
  }
}

for (c in colnames(X_fac)) {
  if (sum(X_fac[c] == "Other") < 40) {
    X_fac <- X_fac |> select(!all_of(c))
  }
}

for (c in colnames(X_fac)) {
  print(sum(X_fac[c] == "Other"))
}


work_data <- cbind(X_num, X_fac)

ITERS <- 40

mses <- rep(0, ITERS)

for (i in 1:ITERS) {
  train <- sample(nrow(work_data), nrow(work_data) * 0.9)
  train_data <- work_data[train,]
  val_data <- work_data[-train,]
  y_train <- y[train]
  y_test <- y[-train]
  
  fit1 <- lm(y ~ polym(., degree=3), data = train_data)
  summary(fit1)
  mses[i] <- mean((predict(fit1, newdata=val_data) - val_data$y) ^ 2)
}

mean(mses)

# for (i in 1:ITERS) {
#   train <- sample(nrow(work_data), nrow(work_data) * 0.8)
#   train_data <- work_data[train,]
#   test_data <- work_data[-train,]
#   y_train <- y[train]
#   y_test <- y[-train]
#   
#   fit1 <- lm(SalePrice ~ ., data = train_data)
#   summary(fit1)
#   mses[i] <- mean((predict(fit1, newdata=test_data) - test_data$SalePrice) ^ 2)
# }
# 



m <- mean(y[train])
s <- mean((y[train] - m)^2)
hist(y, prob=TRUE)
xs <- seq(m-2, m+2, length.out=50)
lines(xs, dnorm(xs, mean = m, sd = s), type='l', ylim = c(0, 3))


for (i in 1:ITERS) {
  train <- sample(nrow(work_data), nrow(work_data) * 0.8)
  train_data <- work_data[train,]
  test_data <- work_data[-train,]
  y_train <- y[train]
  y_test <- y[-train]
  
  normal.pred <- rnorm(n = nrow(test_data), mean = m, sd = s)
  mses[i] <- mean((test_data$y - normal.pred)^2)
}

# prediction

fit1 <- lm(y ~ ., data = work_data)


test_data <- read.csv('test.csv')
test_ids <- test_data$Id
test_data <- test_data |> select(where(is.numeric)) |>
  select(!LotFrontage & !Id)
test_data |> colnames()

test_data |> summarise(across(everything(), ~ sum(is.na(.))))
test_data$BsmtFinSF1[is.na(test_data$BsmtFinSF1)] <- 0
test_data$BsmtFinSF2[is.na(test_data$BsmtFinSF2)] <- 0
test_data$MasVnrArea[is.na(test_data$MasVnrArea)] <- 0
test_data$BsmtUnfSF[is.na(test_data$BsmtUnfSF)] <- 0
test_data$TotalBsmtSF[is.na(test_data$TotalBsmtSF)] <- 0
test_data$BsmtFullBath[is.na(test_data$BsmtFullBath)] <- 0
test_data$BsmtHalfBath[is.na(test_data$BsmtHalfBath)] <- 0
test_data$GarageCars[is.na(test_data$GarageCars)] <- 0
test_data$GarageYrBlt[is.na(test_data$GarageYrBlt)] <- 
  mean(na.omit(house.data$GarageYrBlt))
# test_data$LotFrontage[is.na(test_data$LotFrontage)] <- 0
test_data$GarageArea[is.na(test_data$GarageArea)] <- 0
test_data$GarageCars[is.na(test_data$GarageCars)] <- 0


test_data <- data.frame(scale(test_data))

colnames(test_data)
colnames(work_data)
nrow(test_data)

results <- predict(fit1, test_data)
exp(results)

result_df <- data.frame(
  Id=test_ids,
  SalePrice=exp(results)
  )

write_csv(result_df, "linear_submission.csv")

###########################################################################

work_data <- house.data |> mutate(y = SalePrice)
work_data <- work_data |> select(!SalePrice & !Id)

y <- work_data$y
X <- work_data |> select(!y)

X |> summarise(across(where(is.factor), ~ sum(is.na(.))))
sum(X$Condition2 == "Norm")
X <- X |> select(!PoolQC & !MiscFeature & !Condition1 & !Condition2 & !RoofMatl)

X_fac <- X |> select(where(is.factor))
X_num <- X |> select(where(is.numeric))
X_num <- scale(X_num)
y <- log(y)
svd.fit <- softImpute(X_num)
X_num <- complete(X_num, svd.fit)
X_fac <- X_fac |> mutate(across(everything(), addNA))
# pc <- prcomp(X)
# X <- data.frame(pc$x[,1:3])

work_data <- cbind(data.frame(X_num), X_fac, y)

ITERS <- 20

mses <- rep(0, ITERS)

for (i in 1:ITERS) {
  train <- sample(nrow(work_data), nrow(work_data) * 0.8)
  train_data <- work_data[train,]
  val_data <- work_data[-train,]
  y_train <- y[train]
  y_test <- y[-train]
  
  fit1 <- lm(y ~ ., data = train_data)
  summary(fit1)
  mses[i] <- mean((predict(fit1, newdata=val_data) - val_data$y) ^ 2)
}

mean(mses)

fit <- lm(y ~ ., data = work_data)
summary(fit)

mean((predict(fit, work_data) - y) ^ 2)

test_data <- read.csv('test.csv')
test_ids <- test_data$Id
test_data <- test_data |> select(where(is.numeric)) |>
  select(!Id)
test_data <- scale(test_data)
test.svd <- softImpute(test_data)
test_data <- data.frame(complete(test_data, test.svd))
# test_data <- data.frame(prcomp(test_data)$x[,1:3])

results <- exp(predict(fit, test_data))
head(results)

result_df <- data.frame(
  Id=test_ids,
  SalePrice=results
)

write_csv(result_df, "pca_submission.csv")



