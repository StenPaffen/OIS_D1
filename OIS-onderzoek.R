#Change format of UEQ-s from 1-7 to -3-3: ONLY USE ONCE
for (x in c(10:17, 25:32)){
  Clean_v2_QES_answers[x] <- Clean_v2_QES_answers[x] - 4
}

#pwr test
library(pwr)
pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.8, type = "paired")


Means_table_UEQ <- data.frame(GPT_mean = c(mean(as.numeric(as.character(Clean_v2_QES_answers$`obs-supp-GPT`))),
                                           mean(as.numeric(as.character(Clean_v2_QES_answers$`comp-easy-GPT`))),
                                           mean(as.numeric(as.character(Clean_v2_QES_answers$`ineff-eff-GPT`))),
                                           mean(as.numeric(as.character(Clean_v2_QES_answers$`conf-clear-GPT`))),
                                           mean(as.numeric(as.character(Clean_v2_QES_answers$`bori-exci-GPT`))),
                                           mean(as.numeric(as.character(Clean_v2_QES_answers$`not-intere-GPT`))),
                                           mean(as.numeric(as.character(Clean_v2_QES_answers$`conven-inventi-GPT`))),
                                           mean(as.numeric(as.character(Clean_v2_QES_answers$`usual-lead-GPT`)))),
                              GPT_sd = c(sd(as.numeric(as.character(Clean_v2_QES_answers$`obs-supp-GPT`))),
                                         sd(as.numeric(as.character(Clean_v2_QES_answers$`comp-easy-GPT`))),
                                         sd(as.numeric(as.character(Clean_v2_QES_answers$`ineff-eff-GPT`))),
                                         sd(as.numeric(as.character(Clean_v2_QES_answers$`conf-clear-GPT`))),
                                         sd(as.numeric(as.character(Clean_v2_QES_answers$`bori-exci-GPT`))),
                                         sd(as.numeric(as.character(Clean_v2_QES_answers$`not-intere-GPT`))),
                                         sd(as.numeric(as.character(Clean_v2_QES_answers$`conven-inventi-GPT`))),
                                         sd(as.numeric(as.character(Clean_v2_QES_answers$`usual-lead-GPT`)))),
                              Bard_mean = c(mean(as.numeric(as.character(Clean_v2_QES_answers$`obs-supp-bard`))),
                                            mean(as.numeric(as.character(Clean_v2_QES_answers$`comp-easy-bard`))),
                                            mean(as.numeric(as.character(Clean_v2_QES_answers$`ineff-eff-bard`))),
                                            mean(as.numeric(as.character(Clean_v2_QES_answers$`conf-clear-bard`))),
                                            mean(as.numeric(as.character(Clean_v2_QES_answers$`bori-exci-bard`))),
                                            mean(as.numeric(as.character(Clean_v2_QES_answers$`not-intere-bard`))),
                                            mean(as.numeric(as.character(Clean_v2_QES_answers$`conven-inventi-bard`))),
                                            mean(as.numeric(as.character(Clean_v2_QES_answers$`usual-lead-bard`)))),
                              Bard_sd = c(sd(as.numeric(as.character(Clean_v2_QES_answers$`obs-supp-bard`))),
                                          sd(as.numeric(as.character(Clean_v2_QES_answers$`comp-easy-bard`))),
                                          sd(as.numeric(as.character(Clean_v2_QES_answers$`ineff-eff-bard`))),
                                          sd(as.numeric(as.character(Clean_v2_QES_answers$`conf-clear-bard`))),
                                          sd(as.numeric(as.character(Clean_v2_QES_answers$`bori-exci-bard`))),
                                          sd(as.numeric(as.character(Clean_v2_QES_answers$`not-intere-bard`))),
                                          sd(as.numeric(as.character(Clean_v2_QES_answers$`conven-inventi-bard`))),
                                          sd(as.numeric(as.character(Clean_v2_QES_answers$`usual-lead-bard`)))),
                              Description = c('Obstructive-Supportive', 
                                              'Complicated-Easy',
                                              'Inefficiënt-Efficiënt',
                                              'Confusing-Clear',
                                              'Boring-Exciting',
                                              'Not_interesting-interesting',
                                              'Conventional-Inventive',
                                              'Usual-Leading_edge'))


Means_table_ITS <- data.frame(GPT_mean =  c(mean(as.numeric(as.factor(Clean_v2_QES_answers$`I have mastered the corresponding subject knowledge (GPT)`))),
                                            mean(as.numeric(as.factor(Clean_v2_QES_answers$`The system enhances my ability to analyze and solve problems (GPT)`))),
                                            mean(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my independent learning ability (GPT)`))),
                                            mean(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my interest in learning (GPT)`))),
                                            mean(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my self-confidence in learning (GPT)`)))),
                              GPT_sd = c(sd(as.numeric(as.factor(Clean_v2_QES_answers$`I have mastered the corresponding subject knowledge (GPT)`))),
                                         sd(as.numeric(as.factor(Clean_v2_QES_answers$`The system enhances my ability to analyze and solve problems (GPT)`))),
                                         sd(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my independent learning ability (GPT)`))),
                                         sd(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my interest in learning (GPT)`))),
                                         sd(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my self-confidence in learning (GPT)`)))),
                              Bard_mean = c(mean(as.numeric(as.factor(Clean_v2_QES_answers$`I have mastered the corresponding subject knowledge (Bard)`))),
                                            mean(as.numeric(as.factor(Clean_v2_QES_answers$`The system enhances my ability to analyze and solve problems (Bard)`))),
                                            mean(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my independent learning ability (Bard)`))),
                                            mean(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my interest in learning (Bard)`))),
                                            mean(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my self-confidence in learning (Bard)`)))),
                              Bard_sd = c(sd(as.numeric(as.factor(Clean_v2_QES_answers$`I have mastered the corresponding subject knowledge (Bard)`))),
                                          sd(as.numeric(as.factor(Clean_v2_QES_answers$`The system enhances my ability to analyze and solve problems (Bard)`))),
                                          sd(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my independent learning ability (Bard)`))),
                                          sd(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my interest in learning (Bard)`))),
                                          sd(as.numeric(as.factor(Clean_v2_QES_answers$`The system improves my self-confidence in learning (Bard)`)))),
                              Description = c('I have mastered the corresponding subject knowledge',
                                              'The system enhances my ability to analyze and solve problems',
                                              'The system improves my independent learning ability',
                                              'The system improves my interest in learning',
                                              'The system improves my self-confidence in learning'))


#------------------------------Cronbachs alpha---------
library(ltm)

cronbach.alpha(Clean_v2_QES_answers[c(10:13, 25:28)])
cronbach.alpha(Clean_v2_QES_answers[c(14:17, 29:32)])
cronbach.alpha(Clean_v2_QES_answers[c(18:22, 33:37)])

#------------------------------------------------------------------------

#creëren van hedonic means gpt
hedonic_gpt_means <- Clean_v2_QES_answers[c(14:17)]
hedonic_gpt_means$mean <- apply(hedonic_gpt_means,1,mean)

#creëren van hedonic means bard
hedonic_bard_means <- Clean_v2_QES_answers[c(29:32)]
hedonic_bard_means$mean <- apply(hedonic_bard_means,1,mean)

#Gemiddelden voor hedonic gpt en bard
mean(hedonic_gpt_means$mean)
mean(hedonic_bard_means$mean)

#Sd voor hedonic gpt en bard
sd(hedonic_gpt_means$mean)
sd(hedonic_bard_means$mean)
#------------------------------------------------------------------------
#creëren van pragmatic means gpt
pragmatic_gpt_means <- Clean_v2_QES_answers[c(10:13)]
pragmatic_gpt_means$mean <- apply(pragmatic_gpt_means,1,mean)

#creëren van pragmatic means bard
pragmatic_bard_means <- Clean_v2_QES_answers[c(25:28)]
pragmatic_bard_means$mean <- apply(pragmatic_bard_means,1,mean)

#Gemiddelden voor hedonic gpt en bard
mean(pragmatic_gpt_means$mean)
mean(pragmatic_bard_means$mean)

#Sd voor hedonic gpt en bard
sd(pragmatic_gpt_means$mean)
sd(pragmatic_bard_means$mean)
#---------------------------------------------------------------------
#store the differences in means for pragmatic in a variable
pragmatic_differences <- pragmatic_gpt_means$mean - pragmatic_bard_means$mean

#make qqplot for checking normal distribituion differences in means

# Set up a graphical device with a larger size
options(repr.plot.width=8, repr.plot.height=4)

# Create a QQ plot with improved aesthetics
qqnorm(pragmatic_differences, pch = 20, col = "blue", main = "QQ Plot for Pragmatic Differences", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(pragmatic_differences, col = "red", lwd = 2)

# Add a grid for better readability
grid()

# Add a legend
legend("topleft", legend = "QQ Line", col = "red", lwd = 2, bty = "n", cex = 0.8)

#do shapiro-wilk test to check for normal distribution
shapiro.test(pragmatic_differences)
#W = 0.95572, p-value = 0.1819

#store the differences in means for hedonic in a variable
hedonic_differences <- hedonic_gpt_means$mean - hedonic_bard_means$mean

#make qqplot for checking normal distribituion differences in means
# Set up a graphical device with a larger size
options(repr.plot.width=8, repr.plot.height=4)

# Create a QQ plot with improved aesthetics
qqnorm(hedonic_differences, pch = 20, col = "blue", main = "QQ Plot for Hedonic Differences", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(hedonic_differences, col = "red", lwd = 2)

# Add a grid for better readability
grid()

# Add a legend
legend("topleft", legend = "QQ Line", col = "red", lwd = 2, bty = "n", cex = 0.8)

#do shapiro-wilk test to check for normal distribution
shapiro.test(hedonic_differences)
#W = 0.9391, p-value = 0.05796

#----------------------------------------------------------------------
#t.test voor hedonic eigenschappen
t.test(hedonic_gpt_means$mean, hedonic_bard_means$mean, paired = TRUE)

#t.test voor pragmatic eigenschappen
t.test(pragmatic_gpt_means$mean, pragmatic_bard_means$mean, paired = TRUE)

#----------------------------------------------------------------------
#creëren van ITS-GPT means
ITS_gpt_means <- Clean_v2_QES_answers[c(18:22)]
ITS_gpt_means$mean <- apply(ITS_gpt_means,1,mean)

#creëren van ITS-Bard means
ITS_bard_means <- Clean_v2_QES_answers[c(33:37)]
ITS_bard_means$mean <- apply(ITS_bard_means,1,mean)

#Gemiddelden voor hedonic gpt en bard
mean(ITS_gpt_means$mean)
mean(ITS_bard_means$mean)
median(ITS_gpt_means$mean)
median(ITS_bard_means$mean)

#Sd voor hedonic gpt en bard
sd(ITS_gpt_means$mean)
sd(ITS_bard_means$mean)

######################################################3

#store the differences in means for ITS in a variable
ITS_differences <- ITS_gpt_means$mean - ITS_bard_means$mean

#make qqplot for checking normal distribituion differences in means

# Set up a graphical device with a larger size
options(repr.plot.width=8, repr.plot.height=4)

# Create a QQ plot with improved aesthetics
qqnorm(ITS_differences, pch = 20, col = "blue", main = "QQ Plot for ITS Differences", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(ITS_differences, col = "red", lwd = 2)

# Add a grid for better readability
grid()

# Add a legend
legend("topleft", legend = "QQ Line", col = "red", lwd = 2, bty = "n", cex = 0.8)

#do shapiro-wilk test to check for normal distribution
shapiro.test(ITS_differences)
#W = 0.92842, p-value = 0.02814

wilcox.test(ITS_gpt_means$mean, ITS_bard_means$mean, paired = TRUE)
#---------------------------------Effect size
library(MBESS)

#Calculate effect size pragmatic
hedonic_mean_difference <- t.test(hedonic_gpt_means$mean, hedonic_bard_means$mean, paired = TRUE)$estimate
hedonic_effect_size <- (mean(hedonic_gpt_means$mean) - mean(hedonic_bard_means$mean))/sd(hedonic_differences)
ci.sm(Mean=hedonic_mean_difference, SD=hedonic_effect_size, N=34, conf.level=0.95)

#Calculate effect size pragmatic
pragmatic_mean_difference <- t.test(pragmatic_gpt_means$mean, pragmatic_bard_means$mean, paired = TRUE)$estimate
pragmatic_effect_size <- (mean(pragmatic_gpt_means$mean) - mean(pragmatic_bard_means$mean))/sd(pragmatic_differences)
ci.sm(Mean=pragmatic_mean_difference, SD=pragmatic_effect_size, N=34, conf.level=0.95)

#Calculate effect size ITS
library(coin)

z_value <- wilcoxsign_test(ITS_gpt_means$mean ~ ITS_bard_means$mean, distribution="exact")@statistic@teststatistic
z_value/sqrt(34)

wilcoxsign_test(ITS_gpt_means$mean ~ ITS_bard_means$mean, distribution="exact")

#---------------------------------skewness
library(e1071)

#Skewness berekenen
skewness(ITS_differences)
skewness(pragmatic_differences)
skewness(hedonic_differences)

#----------------------Boxplots voor tijd en prompts------------------------------------------
# Combine the data into a matrix for side-by-side boxplots
times_data <- cbind(Clean_v2_QES_answers$`How much time did it take to answer all the questions? (GPT)`,
                    Clean_v2_QES_answers$`How much time did it take to answer all the questions?  (Bard)`)

# Create side-by-side boxplots
boxplot(times_data, col = c("blue", "green"), names = c("GPT", "Bard"), 
        main = "Vergelijking van Tijd om te Antwoorden", ylab = "Time (seconds)")

prompts_data <- cbind(Clean_v2_QES_answers$`How many prompts where used in total? (GPT)`,
                      Clean_v2_QES_answers$`How many prompts where used in total? (Bard)`)

boxplot(prompts_data, col = c("red", "purple"), names = c("GPT", "Bard"), 
        main = "Vergelijking van Hoeveelheid Gebruikte Prompts", ylab = "Aantal Prompts")


barplot(table(Clean_v2_QES_answers$`How many hours per week on average do you use Chat-GPT?`))
barplot(table(Clean_v2_QES_answers$`How many hours per week on average do you use Bard?`))

#-----------------------------Waffle chart--------------
library(waffle)

proptable <- prop.table(table(Clean_v2_QES_answers$`How many hours per week on average do you use Chat-GPT?`))
row.names(proptable) = c("0 - 1 Uur", "1 - 2 Uur", "2 - 3 Uur", "3 - 4 Uur", "4 - 5 Uur")

# Convert the proportions to the number of tiles
n_tiles <- round(proptable * 100)

proptable2 <- prop.table(table(Clean_v2_QES_answers$`How many hours per week on average do you use Bard?`))
row.names(proptable2) = c("0 - 1 Uur", "1 - 2 Uur")

# Convert the proportions to the number of tiles
n_tiles2 <- round(proptable2 * 100)


require(gridExtra)
plot1 <- waffle(n_tiles, rows = 4, size = 0.5, colors = c("#69b3a2", "#404040", "#a6a6a6", "#d9d9d9", "#252525"),
                title = "Wafeldiagram: Uren per week gebruik van Chat-GPT",
                legend_pos = "bottom")
plot2 <- waffle(n_tiles2, rows = 4, size = 0.5, colors = c("#69b3a2", "#404040", "#a6a6a6", "#d9d9d9", "#252525"),
                title = "Wafeldiagram: Uren per week gebruik van Bard",
                legend_pos = "bottom")
grid.arrange(plot1, plot2, ncol=1)
#-----------------------------------------------------------Plots voor Poster------
par(bg = '#dbdfe6')

# Set the layout to 2 rows and 1 column
par(mfrow = c(3, 1))

# Code for boxplots of pragmatic qualities
boxplot(pragmatic_gpt_means$mean, pragmatic_bard_means$mean, col = c("#4d5f81", "#ee897e"),
        names = c("GPT", "Bard"), 
        main = "Comparison of Pragmatic Qualities", xlab = expression(paste("Rating on a scale from ", -3, " to 3")),
        ylim = c(-3, 3),
        horizontal = TRUE)

# Code for boxplots of hedonic qualities
boxplot(hedonic_gpt_means$mean, hedonic_bard_means$mean, col = c("#4d5f81", "#ee897e"),
        names = c("GPT", "Bard"), 
        main = "Comparison of Hedonic Qualities", xlab = expression(paste("Rating on a scale from ", -3, " to 3")),
        ylim = c(-3, 3),
        horizontal = TRUE)

# Code for boxplots of ITS comparison
boxplot(ITS_gpt_means$mean, ITS_bard_means$mean, col = c("#4d5f81", "#ee897e"),
        names = c("GPT", "Bard"), 
        main = "Comparison of Learning Effect", xlab = expression(paste("Rating on a scale from ", 1, " to 5")),
        ylim = c(1, 5),
        horizontal = TRUE)

