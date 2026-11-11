# Εκτέλεση από τον φάκελο lecture-05: Rscript scripts/analysis.R
# Μόνο base R / stats, χωρίς εξωτερικά πακέτα.
d <- read.csv("anova_study.csv", fileEncoding = "UTF-8-BOM")
stopifnot(identical(names(d), c("child_id", "story_method", "comprehension_score")),
          nrow(d) == 90, !anyNA(d), !anyDuplicated(d$child_id),
          all(d$comprehension_score >= 0 & d$comprehension_score <= 100))
d$story_method <- factor(d$story_method, levels = c("Α", "Β", "Γ"))
print(table(d$story_method))
print(aggregate(comprehension_score ~ story_method, d,
                function(x) c(n=length(x), mean=mean(x), sd=sd(x), min=min(x), max=max(x))))
fit <- aov(comprehension_score ~ story_method, data = d)
print(summary(fit))
a <- summary(fit)[[1]]
SS_B <- a[1, "Sum Sq"]; SS_W <- a[2, "Sum Sq"]
MS_W <- a[2, "Mean Sq"]; df_B <- a[1, "Df"]; SS_T <- SS_B + SS_W
eta2 <- SS_B / SS_T
omega2 <- (SS_B - df_B * MS_W) / (SS_T + MS_W)
print(c(eta2 = eta2, omega2 = omega2))
print(TukeyHSD(fit, conf.level = 0.95))
print(shapiro.test(residuals(fit)))
# Levene με κέντρο τον μέσο: ANOVA των απόλυτων αποκλίσεων.
d$deviation <- abs(d$comprehension_score -
                    ave(d$comprehension_score, d$story_method, FUN = mean))
print(summary(aov(deviation ~ story_method, d)))
# Η deviation είναι προσωρινό διαγνωστικό αντικείμενο· το CSV μένει με 3 στήλες.
# Σύγκριση διορθώσεων (όλοι οι t έλεγχοι χρησιμοποιούν κοινό MS_W).
print(pairwise.t.test(d$comprehension_score, d$story_method,
                      p.adjust.method = "holm", pool.sd = TRUE))
print(pairwise.t.test(d$comprehension_score, d$story_method,
                      p.adjust.method = "bonferroni", pool.sd = TRUE))
stopifnot(abs(unname(a[1, "F value"]) - 16.721250389965) < 1e-8,
          abs(eta2 - .277663620096) < 1e-9,
          abs(omega2 - .258908541721) < 1e-9)
pdf("diagnostics_R.pdf", width=10, height=5)
par(mfrow=c(1,2)); plot(fit, which=1); plot(fit, which=2)
dev.off()
