data<-read.csv("data.csv") 
summary(data)
library(survival)
library(survminer)
library(readr)

# Univariate Analysis of Variables
fit.A<-coxph(Surv(Time,Status==1)~A,data = data)
summary(fit.A)
fit.B<-coxph(Surv(Time,Status==1)~B,data = data)
summary(fit.B)
fit.C<-coxph(Surv(Time,Status==1)~C,data = data24)
summary(fit.C)

# Multivariate Cox Regression
fit<coxph(Surv(Time,Status)~B+C+E+F+G,data=data)
summary(fit)

# Example Code for Survival Analysis
# Load data
setwd("data")
data <- read.csv("data")

# Convert overall survival (OS) time to numeric format
data$OS.time <- as.numeric(data$OS.time)

# Set adjuvant therapy as a categorical variable
no.yes <- factor(data$AdjuvanTherapy, levels = c(0, 1), labels = c("no", "yes"))
time <- data$OS.time
status <- data$OS

# Fit survival model
fit <- survfit(Surv(time, status) ~ no.yes, data = data)

# Set actual P-values (calculated by SPSS)
pairwise_pvals <- c("Overall P" = "*")

# Plot survival curves
plot <- ggsurvplot(fit, data = data,
                   title = "",
                   ylab = "Overall survival (%)",
                   xlab = "Survival time (m)",
                   pval = NULL,  # Suppress overall P-value
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

# Display P-values on the graph
plot$plot <- plot$plot +
  annotate("text", x = 13, y = 0.29, label = "Log-rank", size = 5) +
  annotate("text", x = 13, y = 0.22, label = paste("P =", pairwise_pvals["Overall P"]), size = 4)

# Render the plot
print(plot)
