#実行例6.1
library(MASS)
X <- UScrime[,-c(2,16)]; M <- ncol(X)  # 第 2, 16 変数を除く
y <- UScrime[,16]; N <- length(y)      # 第 16 番目が y に対応
lambdas <- seq(0,5,length=50)         # ラムダの候補
model <- lm.ridge(y ~ ., cbind(X,y), lambda=lambdas)
bestIdx <- which.min(model$GCV)       # 一般化交差確認法の評価値が最小のもの
coefs <- coef(model)[bestIdx, ]       # 回帰係数
lam <- model$lambda[bestIdx]          # 選択されたラムダの値
ypred <- as.matrix(X)%*%as.matrix(coefs[2:15])+coefs[1]  # 予測値

#実行例6.2
sig2 <- ( lam*sum(coefs[2:15]^2) + sum( as.numeric(ypred) - y)^2 )/N
X_ <- t(scale(X,scale=F))                           # 中心化したデータ行列
H  <- t(X_) %*% solve( X_%*%t(X_) + lam*diag(M), X_) # H 行列
TrHN <- sum(diag(H))/N                              # H のトレースを N で割ったもの
a <- (as.numeric(ypred) - y)^2/((1 - TrHN)^2*sig2)  # 異常度
plot(a,xlab="標本番号",ylab="異常度")           # 異常度のプロット
th <- sort(a)[N*(1-0.05)]                           # 閾値
lines(0:50,rep(th,length(0:50)),col="red",lty=2)    # 閾値の線を描く

#実際値 vs 予測値（リッジ回帰）プロット
# 色ベクトルの作成（TRUE=赤, FALSE=黒）
col_vec <- ifelse(a > th, "red", "black")
plot(y, ypred,
     xlab = "実際の値 (y)",
     ylab = "予測値 (ypred)",
     main = "実際値 vs 予測値（リッジ回帰）",
     pch = 19,
     col=col_vec)
# 45度線（完全一致ライン）
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

#PLSによる異常検知
#install.packages("pls")#パッケージのインストール（まだの場合）
library(pls)   # PLS 回帰

## 1. 訓練データの準備
dat <- data.frame(y = y, X)

## 2. PLS 回帰を学習（CVで成分数を決定）
set.seed(123)
pls_mod <- plsr(y ~ ., data = dat,
                scale = TRUE,          # 必要に応じて FALSE でもよい
                validation = "CV")

# CV RMSEP が最小になる成分数を選ぶ
rm <- RMSEP(pls_mod)
best_ncomp <- which.min(rm$val[1,1,-1])  # ncomp = 1,2,... の中から最小
cat("選ばれた PLS 成分数 ncomp =", best_ncomp, "\n")

## 3. 式 (6.34): σ̂_PLS^2 を求める
y_hat_train <- as.numeric(predict(pls_mod, ncomp = best_ncomp)[,,1])

resid <- y - y_hat_train
sigma2_pls <- mean(resid^2)    # 1/N * Σ (y - ŷ)^2
cat("sigma^2_PLS =", sigma2_pls, "\n")

## 4. 式 (6.35): 訓練データの異常度 a_i を計算
##     a_i = (y_i - ŷ_i)^2 / σ̂_PLS^2
a_train <- (y - y_hat_train)^2 / sigma2_pls

## 5. モーメント法で χ^2 分布に当てはめて閾値を決める
##    （手順 6.3 (c)〜「運用時」相当）
m1 <- mean(a_train)      # E[a]
m2 <- var(a_train)       # Var[a]

# χ^2_k の平均 = k, 分散 = 2k なので、
#   m1 ≈ k, m2 ≈ 2k から k を推定
df_hat <- 2 * m1^2 / m2  # 簡単なモーメント推定

alpha <- 0.05            # 上側 5% を閾値とする例
a_th <- qchisq(1 - alpha, df = df_hat)

cat("推定された自由度 df_hat =", df_hat, "\n")
cat("閾値 a_th (上側 5%) =", a_th, "\n")

## 6. 訓練サンプルの異常度プロット
plot(a_train, xlab = "標本番号", ylab = "異常度")
abline(h = a_th, col = "red", lty = 2)

## 7. 実際値 vs 予測値（PLS）プロット
# 色ベクトルの作成（TRUE=赤, FALSE=黒）
col_vec <- ifelse(a_train > a_th, "red", "black")
plot(y, y_hat_train,
     xlab = "実際の値 (y)",
     ylab = "予測値 (ypred)",
     main = "実際値 vs 予測値（PLS）",
     pch = 19,
     col=col_vec)
# 45度線（完全一致ライン）
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

c(coefs)
