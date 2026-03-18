#### BREAST CANCER RELAPSE TIME – STATISTICAL ANALYSIS ################################################################
# Statistical study on 180 breast cancer patients.
# Objective: identify clinical variables associated with time to disease relapse.
# Methods: descriptive statistics, Spearman correlation, T-test, ANOVA,
#          multiple linear regression.
# Author: Javier Pascual Collado
# Dataset: 180 breast cancer patients (datasets.xlsx)


#### LOAD DATA #########################################################################################################
library(readxl)
breastCa <- read_excel("datasets.xlsx")


#### DESCRIPTIVE STATISTICS – STANDARD DEVIATION ######################################################################
sd_age     <- sd(breastCa$age);     sd_age
sd_size    <- sd(breastCa$size);    sd_size
sd_grade   <- sd(breastCa$grade);   sd_grade
sd_nodes   <- sd(breastCa$nodes);   sd_nodes
sd_enodes  <- sd(breastCa$enodes);  sd_enodes
sd_rectime <- sd(breastCa$rectime); sd_rectime
sd_pgr     <- sd(breastCa$pgr);     sd_pgr
sd_er      <- sd(breastCa$er);      sd_er


#### DESCRIPTIVE STATISTICS – VARIANCE ################################################################################
var_age     <- var(breastCa$age);     var_age
var_size    <- var(breastCa$size);    var_size
var_grade   <- var(breastCa$grade);   var_grade
var_nodes   <- var(breastCa$nodes);   var_nodes
var_enodes  <- var(breastCa$enodes);  var_enodes
var_rectime <- var(breastCa$rectime); var_rectime
var_pgr     <- var(breastCa$pgr);     var_pgr
var_er      <- var(breastCa$er);      var_er


#### DESCRIPTIVE STATISTICS – SUMMARY #################################################################################
summary(breastCa$age)
summary(breastCa$size)
summary(breastCa$nodes)
summary(breastCa$enodes)
summary(breastCa$pgr)
summary(breastCa$er)
summary(breastCa$rectime)


#### MENOPAUSAL STATUS – DESCRIPTIVE ANALYSIS #########################################################################
premenopausal  <- (breastCa$meno == "premenopausal")
postmenopausal <- (breastCa$meno == "Postmenopausal")

num_premenopausal  <- sum(premenopausal)
num_postmenopausal <- sum(postmenopausal)

rectime_pre  <- vector("numeric")
rectime_post <- vector("numeric")
meno_binary  <- vector("numeric")  # 0 = premenopausal, 1 = postmenopausal

for (k in 1:180) {
  if (breastCa$meno[k] == "premenopausal") {
    rectime_pre <- append(rectime_pre, breastCa$rectime[k])
    meno_binary <- append(meno_binary, 0)
  } else {
    rectime_post <- append(rectime_post, breastCa$rectime[k])
    meno_binary  <- append(meno_binary, 1)
  }
}

n_total <- length(rectime_pre) + length(rectime_post)

pct_postmenopausal <- num_postmenopausal / n_total * 100; pct_postmenopausal
pct_premenopausal  <- num_premenopausal  / n_total * 100; pct_premenopausal
mean(rectime_pre);  sd(rectime_pre);  var(rectime_pre)
mean(rectime_post); sd(rectime_post); var(rectime_post)


#### T-TEST – MENOPAUSAL STATUS VS ALL QUANTITATIVE VARIABLES #########################################################
t.test(breastCa$age[breastCa$meno     == "Postmenopausal"],
       breastCa$age[breastCa$meno     == "premenopausal"],
       alternative = "two.sided", mu = 0, conf.level = 0.95)

t.test(breastCa$size[breastCa$meno    == "Postmenopausal"],
       breastCa$size[breastCa$meno    == "premenopausal"],
       alternative = "two.sided", mu = 0, conf.level = 0.95)

t.test(breastCa$nodes[breastCa$meno   == "Postmenopausal"],
       breastCa$nodes[breastCa$meno   == "premenopausal"],
       alternative = "two.sided", mu = 0, conf.level = 0.95)

t.test(breastCa$enodes[breastCa$meno  == "Postmenopausal"],
       breastCa$enodes[breastCa$meno  == "premenopausal"],
       alternative = "two.sided", mu = 0, conf.level = 0.95)

t.test(breastCa$pgr[breastCa$meno     == "Postmenopausal"],
       breastCa$pgr[breastCa$meno     == "premenopausal"],
       alternative = "two.sided", mu = 0, conf.level = 0.95)

t.test(breastCa$er[breastCa$meno      == "Postmenopausal"],
       breastCa$er[breastCa$meno      == "premenopausal"],
       alternative = "two.sided", mu = 0, conf.level = 0.95)

t.test(breastCa$rectime[breastCa$meno == "Postmenopausal"],
       breastCa$rectime[breastCa$meno == "premenopausal"],
       alternative = "two.sided", mu = 0, conf.level = 0.95)


#### HISTOLOGICAL GRADE – DESCRIPTIVE ANALYSIS ########################################################################
rectime_grade1 <- vector("numeric")
rectime_grade2 <- vector("numeric")
rectime_grade3 <- vector("numeric")
grade2_vs_rest <- vector("numeric")  # 1 if grade 2, 0 otherwise
grade3_vs_rest <- vector("numeric")  # 1 if grade 3, 0 otherwise
num_grade1 <- 0; num_grade2 <- 0; num_grade3 <- 0

for (k in 1:180) {
  if (breastCa$grade[k] == 3) {
    num_grade3      <- num_grade3 + 1
    rectime_grade3  <- append(rectime_grade3, breastCa$rectime[k])
    grade2_vs_rest  <- append(grade2_vs_rest, 0)
    grade3_vs_rest  <- append(grade3_vs_rest, 1)
  } else if (breastCa$grade[k] == 2) {
    num_grade2      <- num_grade2 + 1
    rectime_grade2  <- append(rectime_grade2, breastCa$rectime[k])
    grade2_vs_rest  <- append(grade2_vs_rest, 1)
    grade3_vs_rest  <- append(grade3_vs_rest, 0)
  } else {
    num_grade1      <- num_grade1 + 1
    rectime_grade1  <- append(rectime_grade1, breastCa$rectime[k])
    grade2_vs_rest  <- append(grade2_vs_rest, 0)
    grade3_vs_rest  <- append(grade3_vs_rest, 0)
  }
}

mean(rectime_grade1); sd(rectime_grade1); var(rectime_grade1)
mean(rectime_grade2); sd(rectime_grade2); var(rectime_grade2)
mean(rectime_grade3); sd(rectime_grade3); var(rectime_grade3)

pct_grade1 <- num_grade1 / n_total * 100; pct_grade1
pct_grade2 <- num_grade2 / n_total * 100; pct_grade2
pct_grade3 <- num_grade3 / n_total * 100; pct_grade3


#### ANOVA – HISTOLOGICAL GRADE VS QUANTITATIVE VARIABLES #############################################################
summary(aov(breastCa$rectime ~ breastCa$grade))
summary(aov(breastCa$age     ~ breastCa$grade))
summary(aov(breastCa$size    ~ breastCa$grade))
summary(aov(breastCa$nodes   ~ breastCa$grade))
summary(aov(breastCa$enodes  ~ breastCa$grade))
summary(aov(breastCa$pgr     ~ breastCa$grade))
summary(aov(breastCa$er      ~ breastCa$grade))


#### VISUALIZATIONS ####################################################################################################
# Boxplot: relapse time by histological grade
par(mfrow = c(1,1))
boxplot(breastCa$rectime ~ breastCa$grade,
        col  = "#87CEFA",
        xlab = "Histological grade",
        ylab = "Relapse time (days)",
        main = "Relapse time by tumour grade")

# Histograms: distribution of quantitative variables
par(mfrow = c(2,3))
hist(breastCa$age,    xlab = "Age (years)",          main = "Age",                    col = "#9AC0CD")
hist(breastCa$size,   xlab = "Tumour size (mm)",      main = "Tumour size",            col = "#9AC0CD")
hist(breastCa$nodes,  xlab = "Positive lymph nodes",  main = "Positive lymph nodes",   col = "#9AC0CD")
hist(breastCa$enodes, xlab = "% lymph nodes affected",main = "% Lymph nodes affected", col = "#9AC0CD")
hist(breastCa$pgr,    xlab = "Progesterone receptors",main = "Progesterone receptors", col = "#9AC0CD")
hist(breastCa$er,     xlab = "Oestrogen receptors",   main = "Oestrogen receptors",    col = "#9AC0CD")

# Scatter plot: relapse time vs positive lymph nodes
par(mfrow = c(1,1))
plot(breastCa$nodes, breastCa$rectime,
     xlab = "Number of positive lymph nodes",
     ylab = "Relapse time (days)",
     col  = "#87CEFA",
     main = "Relapse time vs lymph node involvement")
abline(lm(rectime ~ nodes, data = breastCa), lty = 3, lwd = 3)


#### SPEARMAN CORRELATIONS – ALL VARIABLE PAIRS ########################################################################
# With rectime
cor.test(breastCa$age,    breastCa$rectime, method = "spearman")
cor.test(breastCa$size,   breastCa$rectime, method = "spearman")
cor.test(breastCa$nodes,  breastCa$rectime, method = "spearman")
cor.test(breastCa$enodes, breastCa$rectime, method = "spearman")
cor.test(breastCa$pgr,    breastCa$rectime, method = "spearman")
cor.test(breastCa$er,     breastCa$rectime, method = "spearman")

# Between predictors (confounding variable detection)
cor.test(breastCa$size,   breastCa$age,    method = "spearman")
cor.test(breastCa$nodes,  breastCa$age,    method = "spearman")
cor.test(breastCa$enodes, breastCa$age,    method = "spearman")
cor.test(breastCa$pgr,    breastCa$age,    method = "spearman")
cor.test(breastCa$er,     breastCa$age,    method = "spearman")
cor.test(breastCa$nodes,  breastCa$size,   method = "spearman")
cor.test(breastCa$enodes, breastCa$size,   method = "spearman")
cor.test(breastCa$pgr,    breastCa$size,   method = "spearman")
cor.test(breastCa$er,     breastCa$size,   method = "spearman")
cor.test(breastCa$enodes, breastCa$nodes,  method = "spearman")
cor.test(breastCa$pgr,    breastCa$nodes,  method = "spearman")
cor.test(breastCa$er,     breastCa$nodes,  method = "spearman")
cor.test(breastCa$pgr,    breastCa$enodes, method = "spearman")
cor.test(breastCa$er,     breastCa$enodes, method = "spearman")
cor.test(breastCa$er,     breastCa$pgr,    method = "spearman")


#### MULTIPLE LINEAR REGRESSION MODELS ################################################################################
# Full model: all significant variables
summary(lm(rectime ~ age + nodes + enodes + pgr + er + grade2_vs_rest + grade3_vs_rest,
           data = breastCa))

# Intermediate models (variable selection)
summary(lm(rectime ~ age + nodes + er,          data = breastCa))
summary(lm(rectime ~ age + enodes + pgr,        data = breastCa))
summary(lm(rectime ~ meno + nodes + pgr,        data = breastCa))
summary(lm(rectime ~ grade2_vs_rest + grade3_vs_rest + pgr + meno, data = breastCa))

# Final model: histological grade (3 vs 1) + lymph node count
# Explains 32.12% of rectime variability
# y = 1141.84 - 504.41 * grade3_vs_rest - 39.20 * nodes
summary(lm(rectime ~ grade3_vs_rest + nodes, data = breastCa))


#### 95% CONFIDENCE INTERVALS – FINAL MODEL ###########################################################################
confint(lm(rectime ~ grade3_vs_rest + nodes, data = breastCa), level = 0.95)
