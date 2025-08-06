data<-read.csv("data.csv") 
summary(data)
library(survival)
library(survminer)
library(readr)

#各变量的单因素举例
fit.A<-coxph(Surv(Time,Status==1)~A,data = data)
summary(fit.A)
fit.B<-coxph(Surv(Time,Status==1)~B,data = data)
summary(fit.B)
fit.C<-coxph(Surv(Time,Status==1)~C,data = data24)
summary(fit.C)

#多变量cox回归
fit<coxph(Surv(Time,Status)~B+C+E+F+G,data=data)
summary(fit)

# 生存分析代码-例子
# 读取数据
setwd("data")
data <- read.csv("data")

# 转换OS时间为数值型
data$OS.time <- as.numeric(data$OS.time)

# 设定Adjuvant Therapy为分类变量
no.yes <- factor(data$AdjuvanTherapy, levels = c(0, 1), labels = c("no", "yes"))
time <- data$OS.time
status <- data$OS

# 拟合生存模型
fit <- survfit(Surv(time, status) ~ no.yes, data = data)

# 设置每对组之间的P值
pairwise_pvals <- c("Overall P" = "*")  # 设定实际的P值

# 绘制生存曲线图
plot <- ggsurvplot(fit, data = data,
                   title = "",
                   ylab = "Overall survival (%)",
                   xlab = "Survival time (m)",
                   pval = NULL,  # 不显示总体P值
                   pval.size = 7,
                   surv.median.line = "none",
                   ncensor.plot = FALSE,
                   ncensor.plot.height = 0.25,
                   risk.table = TRUE,
                   risk.table.ylab = "none",
                   risk.table.height = 0.25,
                   legend = c(0.8, 0.9),
                   legend.title = "Adjuvant therapy",
                   legend.labs = c("no", "yes"),
                   conf.int = FALSE,
                   palette = "npg")

# 在图上显示每对组之间的P值
plot$plot <- plot$plot +
  annotate("text", x = 13, y = 0.29, label = "Log-rank", size = 5) +   # 显示Log-rank
  annotate("text", x = 13, y = 0.22, label = paste("P =", pairwise_pvals["Overall P"]), size = 4)

# 显示图形
print(plot)
