lk <- read.csv("/Users/michaelweiss/Desktop/lkdata.csv")

lk.sot <- lk[lk$heading == "N" | lk$heading == "S", ]

dat <- na.omit(lk.sot)

pca <- prcomp(~ pwm + bwm + swm + sot.n, data = dat, scale. = T)

dat$pc1 <- pca$x[,1]

correlation.ag <- function(data, method){
  years <- levels(as.factor(data$year))
  pc1.yearly <- rep(NA, length(years))
  chinook.yearly <- rep(NA, length(years))
  for(i in 1:length(years)){
    chinook.yearly[i] <- mean(data$chinook[as.factor(data$year) == years[i]])
    pc1.yearly[i] <- mean(data$pc1[as.factor(data$year) == years[i]])
  }
  yearly <- data.frame(pc1.yearly, chinook.yearly)
  correlation <- cor(yearly, method = method)[1,2]
  results <- list(data = yearly, correlation = correlation)
  results
}

permute.cor.ag <- function(data, reps, method){#permute correlation coef. while preserving # of pods present for each observation
  obs <- correlation(data, method)$correlation
  null.dist <- rep(NA, reps)
  for(i in 1:reps){
    p1 <- subset(data, npods == 1)
    p2 <- subset(data, npods == 2)
    p3 <- subset(data, npods == 3)
    p1$pc1 <- sample(p1$pc1, size = length(p1$pc1), replace = F)
    p2$pc1 <- sample(p2$pc1, size = length(p2$pc1), replace = F)
    p3$pc1 <- sample(p3$pc1, size = length(p3$pc1), replace = F)
    d <- rbind(p1, p2, p3)
    null.dist[i] <- correlation(d, method)$correlation
  }
  p.value <- min(mean(null.dist < obs), mean(null.dist > obs))*2
  plot(density(null.dist))
  abline(v = obs)
  p.value
}

permute.cor <- function(data, reps, method){#permute correlation coef. while preserving # of pods present for each observation
  obs <- cor(model.frame(pc1 ~ chinook, data = data), method = method)[1,2]
  null.dist <- rep(NA, reps)
  for(i in 1:reps){
    p1 <- subset(data, npods == 1)
    p2 <- subset(data, npods == 2)
    p3 <- subset(data, npods == 3)
    p1$pc1 <- sample(p1$pc1, size = length(p1$pc1), replace = F)
    p2$pc1 <- sample(p2$pc1, size = length(p2$pc1), replace = F)
    p3$pc1 <- sample(p3$pc1, size = length(p3$pc1), replace = F)
    d <- rbind(p1, p2, p3)
    null.dist[i] <- cor(model.frame(pc1 ~ chinook, data = d), method = method)[1,2]
  }
  p.value <- min(mean(null.dist < obs), mean(null.dist > obs))*2
  plot(density(null.dist))
  abline(v = obs)
  p.value
}


