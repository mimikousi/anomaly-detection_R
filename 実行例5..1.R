#実行例5.1
library(MASS)
cc <- c("Min.Price", "Price", "Max.Price", "MPG.city", "MPG.highway",
        "EngineSize", "Horsepower", "RPM", "Rev.per.mile", "Fuel.tank.capacity",
        "Length", "Wheelbase", "Width", "Turn.circle", "Weight")
mask <- is.element(colnames(Cars93), cc)   # 上記15変数のみを選ぶマスク
Xc <- t(scale(Cars93[, mask]))             # 中心化したデータ行列の作成
colnames(Xc) <- t(Cars93[, "Make"])         # 車種（make）を変数名にする
S　<- Xc %*% t(Xc) ;evd <- eigen(S)         # 散布行列の作成と固有値分解
plot(evd$values, type = "b", xlab = "固有値番号", ylab = "固有値") # 図示

#実行例5.2
m <- 2
x2 <- t(evd$vectors[,1:m] ) %*% Xc     # 正常部分空間内の成分を計算
a1 <- colSums(Xc*Xc) - colSums(x2*x2)  # 異常度を全訓練標本に対し計算
idx <- order(a1, decreasing = TRUE)[1:6]
print(a1[idx])                         # 異常度上位六つを出力

#実行例5.3
G <- t(Xc) %*% Xc
evd <- eigen(G)                       # グラム行列 G の固有値分解
Lam_12 <- diag(evd$values[1:m]^(-1/2)) # 固有値の対角行列の -1/2 乗
xx2 <- Lam_12 %*% t(evd$vectors[,1:m]) %*% t(Xc) %*% Xc  # 正常成分
aa1 <- colSums(Xc*Xc) - colSums(xx2*xx2)  # 異常度
idx <- order(aa1, decreasing = TRUE)[1:3]
print(aa1[idx])                         # 上位三つの出力

#実行例5.4
library(kernlab)
m <- 2 ; sig <- 0.1 ; li <- c(-6, 7) # 主成分数、カーネルパラメータ、図示範囲
kpc <- kpca(t(Xc), kernel="rbfdot",
            kpar=list(sigma=sig), features=m)
Zt <- rotated(kpc)                   # 主部分空間における座標

plot(Zt[,1], Zt[,2], xlab="第１主成分", ylab="第２主成分",
     cex=3, col=3,
     xlim=li, ylim=li, main = paste("σ =", sig, "の場合"))     # 座標の図示
text(Zt[,1], Zt[,2], c(1:93), cex=0.8, xlim=li, ylim=li)#車種番号を書き込む


