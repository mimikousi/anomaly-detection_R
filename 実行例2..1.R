#実行例2.1
#install.packages("car")#パッケージのインストール（まだの場合）
library(car) #carパッケージの読込み
data(Davis)
Davis
#ヒストグラムの表示
hist(Davis$weight, xlim=c(35, 105), breaks=14)

#実行例2.2
mu <- mean(Davis$weight) #標本平均
s2 <- mean((Davis$weight - mu)^2) #標本分散
c(mu, s2)

#実行例2.3
a <- (Davis$weight - mu)^2/s2 #異常度
th <- qchisq(0.99, 1) #カイ二乗分布による１％水準の閾値
#異常度のプロット
plot(a, xlab="index", ylab="anomaly score")
lines(0:200, rep(th, length(0:200)), col="red", lty=2) #閾値の線

#write.csv(Davis, "Davis.csv") #Davisデータをcsvに保存
#getwd()#保存先フォルダを確認
c(th)

#実行例2.4
X <- cbind(Davis$weight, Davis$height)#データ行列
#散布図
plot(X[,1], X[,2], pch=16, xlab="weight", ylab="height")

#実行例2.5
th <- qchisq(0.99, 2)#カイ二乗分布による１％水準の閾値（２変数）
mx <- colMeans(X) #標本平均
Xc <- X - matrix(1, nrow(X), 1) %*% mx#中心化したデータ行列
Sx <- t(Xc) %*% Xc / nrow(X)#標本共分散行列
a <- colSums(t(Xc) * solve(Sx, t(Xc)))#異常度
plot(a,xlab="index", ylab="anomaly score")#異常度のプロット
lines(0:200, rep(th, length(0:200)), col="red", lty=2)#閾値

#実行例2.6
#install.packages("MASS")#パッケージのインストール（まだの場合）
library(MASS)
#write.csv(road, "road.csv")# roadデータをCSVに書き出し
#getwd()#保存先フォルダを確認
X <- road / road$drivers #driverにより各列を割る
X <- as.matrix(log(X[,-2] + 1))#対数変換
mx <- colMeans(X)#平均値の計算
Xc <- X - matrix(1, nrow(X),, 1) %*% mx #中心化データ行列
Sx <- t(Xc) %*% Xc / nrow(X)#共分散行列の計算
a <- rowSums((Xc %*% solve(Sx)) * Xc)/ ncol(X)#１変数あたりの異常度
plot(a, xlab = "index", ylab = "anomaly score", ylim = c(-1, 30) / ncol(X))
lines(0:30, rep(1, length(0:30)), col = "red", lty = 2)#「１」の高さに線を引く

#実行例2.7
xc_prime <- Xc["Calif",]#中心化行列からCalifのデータを取り出す
SN1 <- 10*log10(xc_prime^2/diag(Sx))#全変数一気にSN比を計算
barplot(SN1)#棒グラフの出力