library(multcomp)

library(tidyverse)

library(cowplot)
library(VIM)

df<-read.csv("Desktop/DataMining-master/WA_Fn-UseC_-Marketing-Campaign-Eff-UseC_-FastF.csv")

#check results
head(df)

#check for missing data using VIM package
aggr(df, prop = F, numbers = T) # no red - no missing values

#summary sales statistics
(grouped.df <- df %>% 
  group_by(Promotion) %>% 
  summarize(
    count = n(),
    totalSales = sum(SalesInThousands),
    meanSales = mean(SalesInThousands),
    sd = sd(SalesInThousands)))

library("ggpubr")

ggboxplot(df, x = "Promotion", y = "SalesInThousands", 
          color = "Promotion", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Sales", xlab = "Promotion")

(viz_1 <- ggplot(df, aes(x=MarketSize))+
  geom_bar(stat="count", width=0.7)+
  theme_minimal())

#check the promotion variable
str(df$Promotion) #an integer object, we need to change this

#factor the promotion variable before we model it
df$Promotion <- as.factor(df$Promotion)

#check results
str(df$Promotion)

aggregate(SalesInThousands ~ Promotion, df, mean)

#promotion 1 has the highest level of sales, but
# is it statistically significant?

#We plot the ANOVA model to visualize confidence
#intervals for mean sales by promotion
df.anova <- aov(SalesInThousands ~  Promotion, data = df)
summary(df.anova)

#Use glht() to perform multiple pairwise-comparisons for
# a one-way ANOVA: (with confidence interval and p-values)
summary(glht(df.anova, linfct = mcp(Promotion = "Tukey")))

#group 2 is significant against group 1
#group 3 is significant against group 2

TukeyHSD(aov(df.anova), "Promotion") #does same as glht function but includes the confidence intervals

# plot difference in mean levels of promotion
plot(TukeyHSD(df.anova))

#Tukey comparison of means - much better and has confidence intervals
a1 <- aov(formula = df$SalesInThousands ~ df$Promotion)
plot(a1) # plots to check normality

#Post hoc testing
posthoc <- TukeyHSD(x=a1, conf.level = 0.95)
posthoc

library(gplots)

plotmeans(SalesInThousands ~ Promotion, data = df,
  frame = FALSE, connect = TRUE, mean.labels = TRUE,
  digits = 2, col=" dark red",
  barwidth=2, pch = " ",
          main = "Groups 1 & 2 and 3 & 2 are significant")

#With intercept removed, glht gives us the mean value for each segment
df.anova2 <- aov(SalesInThousands ~  -1 + Promotion, data = df)
glht(df.anova2)

# Now we plot difference in mean levels of promotion
plot(glht(df.anova2), xlab="Average sales by promotion (95% CI)")

#The dot shows the mean for each segment and bars reflect the confidence intervals.

#1. Kruskal-Wallis
#Non-parametric alternative to ANOVA
# It’s recommended when the assumptions of one-way ANOVA test are not
# met. One of those assumptions are that the residuals are normally
# distributed

kruskal.test(SalesInThousands ~ Promotion, data = df)

#The p-value is tiny; therefore, we can reject null hypothesis that there
# are no differences in group means, but we don't know which groups.

#2. We do pairwise comparisons and adjust for multiple groups
pairwise.wilcox.test(df$SalesInThousands, df$Promotion,
                 p.adjust.method = "bonferroni", paired = FALSE)
