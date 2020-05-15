library(AdaptGauss)
data<- read.csv('./prepared_resids.csv', header = T)
data <- data[,c("TSP_resid", "SO2_resid", "SO4_resid", "NO2_resid")]

for(column in c("TSP_resid", "SO2_resid", "SO4_resid", "NO2_resid")) {
  ts = data[, column][data[,column] != 0]
  sampled_ts = sample(ts)
  len_half = length(ts) %/% 2
  train = sampled_ts[1:len_half]
  test = sampled_ts[len_half:length(ts)]
  result = EMGauss(train, K=2, SDs=c(1, 0.2))
  print(result)
  print(Chi2testMixtures(test, result$Means, result$SDs, result$Weights))
  kstest=KStestMixtures(test, result$Means, result$SDs, result$Weights)
  print(kstest$PvalueKS)
}
