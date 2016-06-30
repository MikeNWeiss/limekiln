lk <- read.csv("~/limekiln/lkdata copy.csv", row.names=1)

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
  obs <- correlation.ag(data, method)$correlation
  null.dist <- rep(NA, reps)
  for(i in 1:reps){
    p1n <- subset(data, npods == 1 & heading == "N")
    p1s <- subset(data, npods == 1 & heading == "S")
    p2n <- subset(data, npods == 2 & heading == "N")
    p2s <- subset(data, npods == 2 & heading == "S")
    p3n <- subset(data, npods == 3 & heading == "N")
    p3s <- subset(data, npods == 3 & heading == "S")
    p1n$pc1 <- sample(p1n$pc1, size = length(p1n$pc1), replace = F)
    p1s$pc1 <- sample(p1s$pc1, size = length(p1s$pc1), replace = F)
    p2n$pc1 <- sample(p2n$pc1, size = length(p2n$pc1), replace = F)
    p2s$pc1 <- sample(p2s$pc1, size = length(p2s$pc1), replace = F)
    p3n$pc1 <- sample(p3n$pc1, size = length(p3n$pc1), replace = F)
    p3s$pc1 <- sample(p3s$pc1, size = length(p3s$pc1), replace = F)
    d <- rbind(p1n, p1s, p2n, p2s, p3n, p3s)
    null.dist[i] <- correlation.ag(d, method)$correlation
  }
  p.value <- min(mean(null.dist < obs), mean(null.dist > obs))*2
  plot(density(null.dist))
  abline(v = obs)
  results <- list(p.value = p.value, correlation = obs)
  results
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


