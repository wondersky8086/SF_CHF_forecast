# save Survival Functions for Australia residents

savepdf("Fig_1a", width = 12, height = 10, toplines = 0.8)
plot(fts(age, SF_female), xlab = "Age", ylab = "Survival function", main = "Australian female (1921-2020)")
dev.off()

savepdf("Fig_1b", width = 12, height = 10, toplines = 0.8)
plot(fts(age, SF_male),   xlab = "Age", ylab = "", main = "Australian male (1921-2020)")
dev.off()

# save logit transformed Survival function

savepdf("Fig_2a", width = 12, height = 10, toplines = 0.8)
plot(fts(1:(n_age-1), SF_female_logit[1:110,]), 
     xlab = "Age", ylab = "logit transformed Survival function",
     colorchoice = "rainbow", main = "Australian female (1921-2020)")
dev.off()

savepdf("Fig_2b", width = 12, height = 10, toplines = 0.8)
plot(fts(1:(n_age-1), SF_male_logit[1:110,]), 
     xlab = "Age", ylab = " ",
     colorchoice = "rainbow", main = "Australian male (1921-2020)")
dev.off()

# Plot the inverse logit transformation
savepdf("Fig_3a", width = 12, height = 10, toplines = 0.8)
SF_female_back_transform_FPCR = cbind(inv.logit(t(fore_SF_female_FPCR$mean$y)), rep(0, fhap))
plot(fts(65:111, t(SF_female_back_transform_FPCR)), 
     xlab = "Age", ylab = "Survival function", 
     main = "Australian female FPCR method (2021-2065)")
dev.off()

savepdf("Fig_3b", width = 12, height = 10, toplines = 0.8)
SF_male_back_transform_FPCR = cbind(inv.logit(t(fore_SF_male_FPCR$mean$y)), rep(0, fhap))
plot(fts(65:111, t(SF_male_back_transform_FPCR)), 
     xlab = "Age", ylab = "",
     main = "Australian male FPCR method (2021-2065)")
dev.off()

# plot comparison result for Year 2001
savepdf("Fig_2001a", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:111, testing_SF_female[,1],type = 'l',xlab = "Age", ylab = "Survival Function",ylim = c(0,1))
lines(66:111, training_SF_female_LC_back_transform[,1], col = "blue")
lines(66:111, training_SF_female_FPCR_back_transform[,1], col = "green")
title("Female survival function in 2001", col.main = "black")
legend(x = "bottomleft", 4, legend = c("Actual", "LC", "FPCR"), fill = c("black", "blue", "green"))
dev.off()

savepdf("Fig_2001b", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:111,testing_SF_male[,1],type = 'l',xlab = "Age", ylab = "",ylim = c(0,1))
lines (66:111,training_SF_male_LC_back_transform[,1], col = "blue")
lines (66:111,training_SF_male_FPCR_back_transform[,1], col = "green")
title("Male survival function in 2001", col.main = "black")
dev.off()

# plot comparison result for Year 2010
savepdf("Fig_2010a", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:111, testing_SF_female[,10],type = 'l',xlab = "Age", ylab = "Survival Function",ylim = c(0,1))
lines(66:111, training_SF_female_LC_back_transform[,10], col = "blue")
lines(66:111, training_SF_female_FPCR_back_transform[,10], col = "green")
title("Female survival function in 2010", col.main = "black")
#legend(x = "bottomleft", 4, legend = c("Actual", "LC", "FPCR"), fill = c("black", "blue", "green"))
dev.off()

savepdf("Fig_2010b", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:111,testing_SF_male[,10],type = 'l',xlab = "Age", ylab = "",ylim = c(0,1))
lines (66:111,training_SF_male_LC_back_transform[,10], col = "blue")
lines (66:111,training_SF_male_FPCR_back_transform[,10], col = "green")
title("Male survival function in 2010", col.main = "black")
dev.off()

# plot comparison result for Year 2020
savepdf("Fig_4a", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:111, testing_SF_female[,20],type = 'l',xlab = "Age", ylab = "Survival Function",ylim = c(0,1))
lines(66:111, training_SF_female_LC_back_transform[,20], col = "blue")
lines(66:111, training_SF_female_FPCR_back_transform[,20], col = "green")
title("Female survival function in 2020", col.main = "black")
#legend(x = "bottomleft", 4, legend = c("Actual", "LC", "FPCR"), fill = c("black", "blue", "green"))
dev.off()

savepdf("Fig_4b", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(66:111,testing_SF_male[,20],type = 'l',xlab = "Age", ylab = "",ylim = c(0,1))
lines (66:111,training_SF_male_LC_back_transform[,20], col = "blue")
lines (66:111,training_SF_male_FPCR_back_transform[,20], col = "green")
title("Male survival function in 2020", col.main = "black")
dev.off()

#Plot figures for MAE and MAPE

savepdf("Fig_5a", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(MAE_FPCR_female,type = 'l',xlab = "forecast horizon", ylab = "",col = "green")
lines (MAE_LC_female, col = "blue", lty = 2)
lines (MAE_FPCR_male, col = "black", lty = 3)
lines (MAE_LC_male, col = "red", lty = 4)
title("Mean Absolute Forecast Error (MAFE)", col.main = "black")
legend(x="topleft", 4, legend=c("Female FPCR", "Male FPCR","Female LC", "Male LC"),lty = c(1, 3, 2, 4),
       col = c("green","black","blue","red") )
dev.off()

savepdf("Fig_5b", width = 12, height = 10, toplines = 0.8, pointsize = 12)
plot(MAPE_FPCR_female,type = 'l',xlab = "forecast horizon", ylab = "",col = "green")
lines (MAPE_LC_female, col = "blue", lty = 2)
lines (MAPE_FPCR_male, col = "black", lty = 3)
lines (MAPE_LC_male, col = "red", lty = 4)
title("Mean Absolute Percent Error (MAPE)", col.main = "black")
dev.off()