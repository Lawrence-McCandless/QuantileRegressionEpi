################################################################################
# R Codes for: # "Quantile regression of gestational age and preterm birth for epidemiology research"
################################################################################

pms <- read.csv("pms.synthetic.csv")

#Table 1 code: gestational age by albuminuria"
tp <- pms$ga[pms$album == 1]
tab <- c(length(tp), round(mean(tp), 1), round(sd(tp), 1),
         quantile(tp, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 1),
         sum(tp < 259), round(100 * mean(tp < 259), 1))

tp <- pms$ga[pms$album == 0]
tab <- rbind(tab,
             c(length(tp), round(mean(tp), 1), round(sd(tp), 1),
               quantile(tp, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 1),
               sum(tp < 259), round(100 * mean(tp < 259), 1)))

tp <- pms$ga
tab <- rbind(tab,
             c(length(tp), round(mean(tp), 1), round(sd(tp), 1),
               quantile(tp, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), type = 1),
               sum(tp < 259), round(100 * mean(tp < 259), 1)))

rownames(tab) <- c("Albuminuria", "No albuminuria", "All")
colnames(tab) <- c("n", "mean", "sd", "q10", "q25", "q50", "q75", "q90",
                   "preterm_count", "preterm_percent")

tab

print(tab)

# ==============================================================================
# Figure 1 code: f(x), F(x), and inverse CDF for gestational age
# ==============================================================================

pms <- read.csv("pms.synthetic.csv")

n <- dim(pms)[1]
f.x <- rep(0, max(pms$ga) + 1)
f.x[as.numeric(names(table(pms$ga)))] <- table(pms$ga) / n

F.x <- cumsum(f.x)

par(mfrow = c(1, 3))

plot(c(220, 321), c(0, max(f.x)), type = "n",
     ylab = "Probability", xlab = "Gestational Age", main = "f(x)")
for (i in 220:321) {
  lines(c(i, i), c(0, f.x[i]), lwd = 1.5)
}
abline(v = 260, lty = 2, lwd = 2)

plot(c(220, 321), c(0, 1), type = "n",
     ylab = "Probability", xlab = "Gestational Age", main = "F(x)")
for (i in 220:321) {
  lines(c(i, i + 1), c(F.x[i + 1], F.x[i + 1]), lwd = 1.5)
}
for (i in 220:321) {
  lines(c(i, i), c(F.x[i], F.x[i + 1]), lwd = 1.5)
}
abline(v = 260, lty = 2, lwd = 2)

plot(c(0, 1), c(220, 321), type = "n",
     ylab = "Gestational Age",
     xlab = expression(paste(tau, " - quantile")),
     main = expression(F^{-1}(x)))
for (i in 220:321) {
  lines(c(F.x[i + 1], F.x[i + 1]), c(i, i + 1), lwd = 1.5)
}
for (i in 220:321) {
  lines(c(F.x[i], F.x[i + 1]), c(i, i), lwd = 1.5)
}
abline(h = 260, lty = 2, lwd = 2)

# ==============================================================================
# Figure 2 code: distribution of ga by albuminuria 
# ==============================================================================

pms <- read.csv("pms.synthetic.csv")

n <- dim(pms)[1]

par(mfrow = c(1, 3))

plot(c(220, 321), c(0, 0.08), type = "n",
     ylab = "Probability", xlab = "Gestational Age", main = "f(x)")

tmp <- density(pms$ga[pms$album == 0], bw = 0.5)
lines(tmp$x, tmp$y)

tmp <- density(pms$ga[pms$album == 1], bw = 0.5)
lines(tmp$x, tmp$y, col = "red")

abline(v = 260, lty = 2, lwd = 2)

plot(c(220, 321), c(0, 1), type = "n",
     ylab = "Probability", xlab = "Gestational Age", main = "F(x)")

tabs.labels <- as.numeric(names(table(pms$ga[pms$album == 0])))
f.x <- table(pms$ga[pms$album == 0]) / sum(table(pms$ga[pms$album == 0]))
F.x <- cumsum(f.x)

for (i in 1:(length(tabs.labels) - 1)) {
  lines(c(tabs.labels[i], tabs.labels[i + 1]), c(F.x[i], F.x[i]))
}
for (i in 1:length(tabs.labels)) {
  lines(c(tabs.labels[i], tabs.labels[i]), c(ifelse(i == 1, 0, F.x[i - 1]), F.x[i]))
}

tabs.labels <- as.numeric(names(table(pms$ga[pms$album == 1])))
f.x <- table(pms$ga[pms$album == 1]) / sum(table(pms$ga[pms$album == 1]))
F.x <- cumsum(f.x)

for (i in 1:(length(tabs.labels) - 1)) {
  lines(c(tabs.labels[i], tabs.labels[i + 1]), c(F.x[i], F.x[i]), col = "red")
}
for (i in 1:length(tabs.labels)) {
  lines(c(tabs.labels[i], tabs.labels[i]), c(ifelse(i == 1, 0, F.x[i - 1]), F.x[i]), col = "red")
}

abline(v = 260, lty = 2, lwd = 2)

plot(c(0, 1), c(220, 321), type = "n",
     ylab = "Gestational Age",
     xlab = expression(paste(tau, " - quantile")),
     main = expression(F^{-1}(x)))

tabs.labels <- as.numeric(names(table(pms$ga[pms$album == 0])))
f.x <- table(pms$ga[pms$album == 0]) / sum(table(pms$ga[pms$album == 0]))
F.x <- cumsum(f.x)

for (i in 1:(length(tabs.labels) - 1)) {
  lines(c(F.x[i], F.x[i]), c(tabs.labels[i], tabs.labels[i + 1]))
}
for (i in 1:length(tabs.labels)) {
  lines(c(ifelse(i == 1, 0, F.x[i - 1]), F.x[i]), c(tabs.labels[i], tabs.labels[i]))
}

tabs.labels <- as.numeric(names(table(pms$ga[pms$album == 1])))
f.x <- table(pms$ga[pms$album == 1]) / sum(table(pms$ga[pms$album == 1]))
F.x <- cumsum(f.x)

for (i in 1:(length(tabs.labels) - 1)) {
  lines(c(F.x[i], F.x[i]), c(tabs.labels[i], tabs.labels[i + 1]), col = "red")
}
for (i in 1:length(tabs.labels)) {
  lines(c(ifelse(i == 1, 0, F.x[i - 1]), F.x[i]), c(tabs.labels[i], tabs.labels[i]), col = "red")
}

abline(h = 260, lty = 2, lwd = 2)

# ==============================================================================
# Table 2 code: quantile regression, OLS, and logistic regression
# ==============================================================================

pms <- read.csv("pms.synthetic.csv")

rq(ga ~ album, tau=c(0.1, 0.25, 0.5, 0.75, 0.9), data=pms)

rq(ga ~ album + female + as.factor(smk), tau=c(0.1, 0.25, 0.5, 0.75, 0.9), data=pms);

lm(ga ~ album, data=pms);

pms$ptb <- ifelse(pms$ga < 259, 1, 0)
glm(ptb ~ album, family='binomial', data=pms)

# ==============================================================================
# Figure 3 code: integer-valued coefficients in multivariable QR
# ==============================================================================

pms <- read.csv("pms.synthetic.csv")

taus <- 0.1 * (1:9)

fit <- rq(ga ~ album + female + single + as.factor(smk),
          data = pms, tau = taus)

b <- coef(fit)

preds <- c("album", "female", "single",
           "as.factor(smk)1", "as.factor(smk)2")

main_titles <- c("Albuminuria",
                  "Female Sex",
                  "Single",
                  "Former smoker\nversus Non-smoker",
                  "Smoker\nversus Non-smoker")

par(mfrow = c(2, 3))

for (j in 1:length(preds)) {
  y <- b[preds[j], ]
  
  plot(taus, y,
       type = "n",
       ylim = c(-6, 1),
       xlab = expression(tau),
       ylab = "Coefficient estimate",
       main = main_titles[j])
  
  abline(h = seq(-8, 1, by = 1), col = "grey85", lty = 1)
  abline(h = 0, col = "grey40", lty = 2)
  
  lines(taus, y, type = "b", pch = 19)
}

# ==============================================================================
# Figure 4 code: dithering gestational age for coefficients in multivariable QR
# ==============================================================================

pms <- read.csv("pms.synthetic.csv")

taus <- 0.1 * (1:9)

preds <- c("album", "female", "single",
           "as.factor(smk)1", "as.factor(smk)2")

main_titles <- c("Albuminuria",
                 "Female Sex",
                 "Single",
                 "Former smoker\nversus Non-smoker",
                 "Smoker\nversus Non-smoker")

coef_sum <- matrix(0, nrow = length(preds), ncol = length(taus))

rownames(coef_sum) <- preds
colnames(coef_sum) <- taus

for (i in 1:100) {
  pms_d <- pms
  pms_d$ga <- dither(pms$ga)
    fit_d <- rq(ga ~ album + female + single + as.factor(smk), data = pms_d, tau = taus)
  
  bmat <- coef(fit_d)
  
  coef_sum <- coef_sum + bmat[preds, ]
}

coef_mean <- coef_sum / 100

par(mfrow = c(2, 3))

for (j in 1:length(preds)) {
  
  y <- coef_mean[j, ]
  
  plot(taus, y,
       type = "n",
       ylim = c(-6, 1),
       xlab = expression(tau),
       ylab = "Coefficient estimate",
       main = main_titles[j])
  
  abline(h = seq(-8, 1, by = 1), col = "grey85", lty = 1)
  abline(h = 0, col = "grey40", lty = 2)
  
  lines(taus, y, type = "b", pch = 19)
}
