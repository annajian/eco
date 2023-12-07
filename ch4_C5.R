library(datasets)
data_ch4_c5<-mlb1
data_ch4_c5$log_salary <- log(data_ch4_c5$salary)
# 假設 lm_full 是完整模型
lm_full <- lm(log_salary~ years + gamesyr + bavg + hrunsyr + rbisyr, data = data_ch4_c5)
# 假設 lm_simplified 是簡化模型，去除了 rbisyr
lm_simplified <- lm(log_salary ~ years + gamesyr + bavg + hrunsyr , data = data_ch4_c5)
summary(lm_full)
summary(lm_simplified)
#(i)根據summary的結果可以看出在lm_full中的hrunsyr的P值明顯大很多表示在lm_full中hrunsyr並不顯著，相較之下在lm_simplfied中，拿掉rbisyr之後P值下降很多，變得顯著了，這表示 lm_simplified 模型對於 hrunsyr 的統計顯著性產生了明顯的影響，使得 hrunsyr 的係數在模型中變得顯著，可能說明 hrunsyr 在解釋log(salary)有更顯著的效果
lm_simplified_new <- lm(log_salary ~ years + gamesyr + bavg + hrunsyr + runsyr+fldperc+sbasesyr,data = data_ch4_c5)
#(ii)
summary_result <- summary(lm_simplified_new)
p_values <- summary_result$coefficients[, "Pr(>|t|)"]
significant_variables <- names(p_values[p_values < 0.05])
print(significant_variables)
#(iii)
lm_full2 <- lm(log_salary ~ years + gamesyr + bavg + hrunsyr + runsyr + fldperc + sbasesyr, data = data_ch4_c5)
lm_simplified2 <- lm(log_salary ~ bavg + fldperc + sbasesyr, data = data_ch4_c5)
anova_result <- anova(lm_simplified2, lm_full2)
print(anova_result)
#根據anova結果顯示Ｐ值小於0.05 拒絕H0假設，表示這些字變數具有聯合顯著性

