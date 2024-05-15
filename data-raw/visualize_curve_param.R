# Import longitudinal antibody parameters from OSF
curve_param <- getAdditionalData(fileURL = "https://osf.io/download/rtw5k/")

plot1 <- graph.curve.params(curve_param)
print(plot1)
