# Independent reference analysis; run from the lecture-06 folder.
# Packages: car, emmeans (install separately if absent).
d <- read.csv('two_way_anova.csv', fileEncoding = 'UTF-8-BOM')
d$story_method <- factor(d$story_method, levels=c('Αφήγηση','Εικόνες','Κούκλες'))
d$age_group <- factor(d$age_group, levels=c('4 ετών','5 ετών'))
contrasts(d$story_method) <- contr.sum(3)
contrasts(d$age_group) <- contr.sum(2)
fit <- lm(comprehension_score ~ story_method * age_group, data=d)
print(car::Anova(fit, type=3))
print(anova(update(fit, . ~ 1), fit))
print(car::leveneTest(comprehension_score ~ story_method * age_group, data=d, center=mean))
print(shapiro.test(residuals(fit)))
# One family of all 15 comparisons, not separate Tukey families by age.
means <- emmeans::emmeans(fit, ~ story_method * age_group)
print(means)
print(summary(pairs(means, adjust='tukey'), infer=c(TRUE,TRUE)))
