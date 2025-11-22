#実行例4.1
score   <- c(0.19,0.86,0.17,0.12,0.04,0.78,0.16,0.51,0.57,0.27)  # 異常度
anomaly <- c(F,T,F,F,F,T,F,T,F,F)                                # 異常かどうかのフラグ
data0   <- cbind(score, anomaly)                                 # 異常度とフラグをまとめて一つのデータに
rownames(data0) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")
barplot(data0[, "score"], ylim = c(0,1), col = data0[, "anomaly"], ylab = "score (異常度)")  # 図示

#実行例4.2
data1          <- data0[order(score, decreasing = TRUE), ]
score_sorted   <- data1[, "score"]
anomaly_sorted <- data1[, "anomaly"]

## ==== 実行例4.2のグラフ（左：元の順、右：並べ替え後）====
op <- par(mfrow = c(1,2), mar = c(4,3.5,2,1)) #１行２列でグラフ表示、余白の設定

# (a) 元の順のバー
barplot(height = data0[, "score"],
        ylim = c(0,1),
        names.arg = rownames(data0),
        col = c("white","black")[as.logical(data0[, "anomaly"]) + 1],
        ylab = "score (異常度)",
        main = "(a) 異常度スコアの例")

# (b) 並べ替え後のバー
barplot(height = score_sorted,
        ylim = c(0,1),
        names.arg = rownames(data1),
        col = c("white","black")[as.logical(anomaly_sorted) + 1],
        ylab = "score (異常度)",
        main = "(b) 並べ替えたもの")

par(op) #設定を元に戻す


#実行例4.3
n_total <- length(anomaly)                   # 全標本数
n_anom  <- sum(anomaly)
n_norm  <- n_total - n_anom  # 正常および異常標本数
coverage  <- rep(0, n_total)                 # 異常網羅率の入れ物
detection <- rep(1, n_total)                 # 正答率の入れ物

for(i in c(1:n_total)){
  n_detectedAnom <- sum(anomaly_sorted[1:i])
  n_detectedNorm <- (n_total - i) - sum(anomaly_sorted[-(1:i)])
  coverage[i]  <- n_detectedAnom / n_anom
  detection[i] <- n_detectedNorm / n_norm
}

## ==== 実行例4.3のグラフ====
x <- score_sorted  
plot(x, coverage, type = "b", pch = 16, ylim = c(0,1),
     xlab = "異常判定の閾値",
     ylab = "異常標本精度／正常標本精度（正答率）")
lines(x, detection, type = "b", pch = 1, lty = 2)
legend("bottomleft",
       legend = c("異常標本精度 (coverage)", "正常標本精度 (detection)"),
       pch = c(16,1), lty = c(1,2), bty = "n")
