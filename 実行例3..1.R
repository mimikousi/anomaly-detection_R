#実行例3.1
library(car) #carパッケージの読込み
library(MASS)
data(Davis)

N <- length(Davis$weight)#標本数
mu <- mean(Davis$weight)#標本平均
si <- sd(Davis$weight)* sqrt((N - 1)/ N)#標準偏差
kmo <- (mu/ si)^2#モーメント法によるｋの推定値
smo <- si^2/mu #モーメント法によるsの推定値
ml <- fitdistr(Davis$weight, "gamma")
kml <- ml$estimate["shape"]#最尤法によるkの推定値
sml <- 1/ ml$estimate["rate"]#最尤法によるsの推定値
c(kmo, smo)
c(kml, sml)

#実行例3.2
a <- Davis$weight/smo - (kmo -1)* log(Davis$weight/ smo)
th <- order(a, decreasing = T)[0.01*N]
plot(a, ylab = "anomaly score")
lines(0:200, rep(a[th], length(0:200)), col="red", lty=2)

# ヒストグラムの表示（オリジナル）
h <- hist(Davis$weight, xlim = c(35, 105), breaks = 24)

# 平均・分散などの算出
mu <- mean(Davis$weight)
s2 <- var(Davis$weight)
sigma <- sqrt(s2)
n <- length(Davis$weight)
bin_width <- diff(h$breaks)[1]  # 階級幅（等間隔前提）

# x軸の細かい点を生成
x <- seq(35, 105, length = 300)

# ==== 正規分布（破線） ====
y_norm <- dnorm(x, mean = mu, sd = sigma) * n *bin_width
lines(x, y_norm, col = "red", lty = 2, lwd = 2)

# ==== ガンマ分布（実線） ====
shape <- mu^2 / s2
scale <- s2 / mu
y_gamma <- dgamma(x, shape = shape, scale = scale) * n * bin_width
lines(x, y_gamma, col = "darkgreen", lty = 1, lwd = 2)
