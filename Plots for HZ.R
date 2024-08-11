# save Cumulative Hazard Functions for Australia residents

savepdf("Fig_6a", width = 12, height = 10, toplines = 0.8)
plot(fts(age, H_female), xlab = "Age", ylab = "Cumulative hazard function", main = "Australian female (1921-2020)")
dev.off()

savepdf("Fig_6b", width = 12, height = 10, toplines = 0.8)
plot(fts(age, H_male),   xlab = "Age", ylab = "", ylim = c(0,12), main = "Australian male (1921-2020)")
dev.off()

# save log transformed Cumulative Hazard function

savepdf("Fig_7a", width = 12, height = 10, toplines = 0.8)
plot(fts(1:(n_age-1), H_log_female[1:110,]), 
     xlab = "Age", ylab = "log Cumulative hazard function",
     colorchoice = "rainbow", main = "Australian female (1921-2020)")
dev.off()

savepdf("Fig_7b", width = 12, height = 10, toplines = 0.8)
plot(fts(1:(n_age-1), H_log_male[1:110,]), 
     xlab = "Age", ylab = " ",
     colorchoice = "rainbow", main = "Australian male (1921-2020)")
dev.off()

# Plot the forecast of Cumulative Hazard Function
savepdf("Fig_8a", width = 12, height = 10, toplines = 0.8)
plot(fts(65:110, H_female_forecast), 
     xlab = "Age", ylab = "Cumulative Hazard function", ylim = c(0,8),
     main = "Australian female LC method (2021-2065)")
dev.off()

savepdf("Fig_8b", width = 12, height = 10, toplines = 0.8)
plot(fts(65:110, H_male_forecast), 
     xlab = "Age", ylab = "",
     main = "Australian male FPCR method (2021-2065)")
dev.off()
# plot comparison result for Year 2001
savepdf("Fig_H2001a", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:110, testing_H_female[,1],type = 'l',xlab = "Age", ylab = "Cumulative Hazard Function",ylim = c(0,10))
lines(66:110, training_H_female_LC_back_transform[,1], col = "blue")
lines(66:110, training_H_female_FPCR_back_transform[,1], col = "green")
title("Female Cumulative Hazard function in 2001", col.main = "black")
legend(x = "topleft", 4, legend = c("Actual", "LC", "FPCR"), fill = c("black", "blue", "green"))
dev.off()

savepdf("Fig_H2001b", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:110,testing_H_male[,1],type = 'l',xlab = "Age", ylab = "",ylim = c(0,10))
lines (66:110,training_H_male_LC_back_transform[,1], col = "blue")
lines (66:110,training_H_male_FPCR_back_transform[,1], col = "green")
title("Male Cumulative Hazard function in 2001", col.main = "black")
dev.off()

# plot comparison result for Year 2010
savepdf("Fig_H2010a", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:110, testing_H_female[,10],type = 'l',xlab = "Age", ylab = "Cumulative Hazard Function",ylim = c(0,10))
lines(66:110, training_H_female_LC_back_transform[,10], col = "blue")
lines(66:110, training_H_female_FPCR_back_transform[,10], col = "green")
title("Female Cumulative Hazard function in 2010", col.main = "black")
#legend(x = "topleft", 4, legend = c("Actual", "LC", "FPCR"), fill = c("black", "blue", "green"))
dev.off()

savepdf("Fig_H2010b", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:110,testing_H_male[,10],type = 'l',xlab = "Age", ylab = "",ylim = c(0,10))
lines (66:110,training_H_male_LC_back_transform[,10], col = "blue")
lines (66:110,training_H_male_FPCR_back_transform[,10], col = "green")
title("Male Cumulative Hazard function in 2010", col.main = "black")
dev.off()

# plot comparison result for Year 2020
savepdf("Fig_9a", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:110, testing_H_female[,20],type = 'l',xlab = "Age", ylab = "Cumulative Hazard Function",ylim = c(0,10))
lines(66:110, training_H_female_LC_back_transform[,20], col = "blue")
lines(66:110, training_H_female_FPCR_back_transform[,20], col = "green")
title("Female Cumulative Hazard function in 2020", col.main = "black")
#legend(x = "topleft", 4, legend = c("Actual", "LC", "FPCR"), fill = c("black", "blue", "green"))
dev.off()

savepdf("Fig_9b", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:110,testing_H_male[,20],type = 'l',xlab = "Age", ylab = "",ylim = c(0,10))
lines (66:110,training_H_male_LC_back_transform[,20], col = "blue")
lines (66:110,training_H_male_FPCR_back_transform[,20], col = "green")
title("Male Cumulative Hazard function in 2020", col.main = "black")
dev.off()

#Plot figures for MAE and MAPE

savepdf("Fig_10a", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(MAE_FPCR_female,type = 'l',xlab = "forecast horizon", ylab = "",col = "green")
lines (MAE_LC_female, col = "blue", lty = 2)
lines (MAE_FPCR_male, col = "black", lty = 3)
lines (MAE_LC_male, col = "red", lty = 4)
title("Mean Absolute Forecast Error (MAFE)", col.main = "black")
legend(x="topleft", 4, legend=c("Female FPCR", "Male FPCR","Female LC", "Male LC"),lty = c(1, 3, 2, 4),
       col = c("green","black","blue","red") )
dev.off()

savepdf("Fig_10b", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(MAPE_FPCR_female,type = 'l',xlab = "forecast horizon", ylab = "",col = "green")
lines (MAPE_LC_female, col = "blue", lty = 2)
lines (MAPE_FPCR_male, col = "black", lty = 3)
lines (MAPE_LC_male, col = "red", lty = 4)
title("Mean Absolute Percent Error (MAPE)", col.main = "black")
dev.off()