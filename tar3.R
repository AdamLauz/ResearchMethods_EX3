require(xlsx)
data <- read.xlsx("data_and_headers_processed.xlsx", 1, stringsAsFactors=T)
data$Age <- as.numeric(as.character(data$Age))

data.for.clarity <- cbind(data[,c("C1", "C2", "C3", "C5")], 8-data$C4, 8-data$C6)
clarity <- apply(data.for.clarity, MARGIN = 1, FUN = mean)

data.for.politeness <- cbind(data[,c("P1", "P2", "P4", "P5", "P6")], 8-data$P3)
politeness <- apply(data.for.politeness, MARGIN = 1, FUN = mean)

data.for.satisfaction <- cbind(data[,c("S1", "S2", "S3", "S5", "S6")], 8-data$S4)
satisfaction <- apply(data.for.satisfaction, MARGIN = 1, FUN = mean)

#now adding them to the data frame
data <- cbind(data, clarity = clarity, politeness = politeness, satisfaction = satisfaction)



data_filtered <- data[data$Age  >= 18 & data$Age<=49,]

#Remove NA rows from the data
data_filtered <- na.exclude(data_filtered)


#-----------------------------Task 1------------------------------------
politeness_s_group <- data_filtered[data_filtered$System == 'S', names(data_filtered) %in% 'politeness']
politeness_c_group <- data_filtered[data_filtered$System == 'C', names(data_filtered) %in% 'politeness']
politeness_males_group <- data_filtered[data_filtered$Sex == 'C1', names(data_filtered) %in% 'politeness']
politeness_females_group <- data_filtered[data_filtered$Sex == 'C2', names(data_filtered) %in% 'politeness']

#Normality test: H0: the population is normally distibuted
shapiro.test(politeness_s_group) #p-value < 0.05 therefore the population is not normally distirbuted!
shapiro.test(politeness_c_group) #p-value > 0.05 therefore the population is normally distirbuted
shapiro.test(politeness_males_group) #p-value < 0.05 therefore the population is not normally distirbuted
shapiro.test(politeness_females_group) #p-value > 0.05 therefore the population is normally distirbuted!
qqnorm(politeness_s_group);qqline(politeness_s_group, col = 8)
qqnorm(politeness_c_group);qqline(politeness_c_group, col = 8)
hist(politeness_s_group, col="blue", breaks = 20, main = "System = S")
hist(politeness_c_group, col="red", breaks = 20, main = "System = C")
hist(politeness_males_group, col="green", breaks = 20, main = "Sex = Male")
hist(politeness_females_group, col="yellow", breaks = 20, main = "Sex = Female")
#Perform variance test 
var.test(politeness_s_group,politeness_c_group)
var.test(politeness_males_group, politeness_females_group)

#t-test 1: H0: Politeness(S) = Politeness(C). H1: Politeness(S) != Politeness(C)
t.test(politeness_s_group, politeness_c_group, var.equal = TRUE)

#t-test 2: H0: Politeness(males) = Politeness(females). H1: Politeness(males) != Politeness(females)
t.test(politeness_males_group, politeness_females_group, var.equal = TRUE)

#-----------------------------Task 2------------------------------------
#Descriptive statistics
library(doBy)
descrp_stats <- summaryBy(clarity ~ System + Comp_Use_Know + System*Comp_Use_Know , data = data_filtered, 
          FUN = function(x) { c(Mean = mean(x), Std_D = sd(x), N = length(x) ) } )

#Two-way anova model
anovaModel <- aov(clarity ~ System + Comp_Use_Know + System*Comp_Use_Know, data = data_filtered)

#Perform Levene's test where H0: we have a constant variance
anovaModel.levene.data <- abs(anovaModel$residuals)
anovaModel.levene <- aov(anovaModel.levene.data ~ data_filtered$clarity)
summary(anovaModel.levene) # p-value is < 0.05 therefore we reject H0 -> need to lower the p-value in the anova-two-test to 0.01!

#Two-way anova summary
summary(anovaModel) #System is strongly significent ***. the interaction System*Comp_use_know is also significent * -> THERE IS AN INTERACTION!!!


#limit margins
par(mar = rep(2, 4))
interaction.plot(data_filtered$System, data_filtered$Comp_Use_Know, data_filtered$clarity)
interaction.plot(data_filtered$Comp_Use_Know, data_filtered$System, data_filtered$clarity)

#Tukey test - not needed*************************
TukeyHSD(anovaModel, "Comp_Use_Know")

#One-way anovas
anovaModel_S <- aov(clarity ~ Comp_Use_Know, data = data_filtered[data_filtered$System == "S",])
anovaModel_C <- aov(clarity ~ Comp_Use_Know, data = data_filtered[data_filtered$System == "C",])
summary(anovaModel_S)
summary(anovaModel_C)
#t-tests
Comp_Use_Know_F1 <- data_filtered[data_filtered$Comp_Use_Know == "F1",]
var.test(Comp_Use_Know_F1$clarity, Comp_Use_Know_F1$System)
t.test(clarity ~ System, data=Comp_Use_Know_F1, var.equal = FALSE)

Comp_Use_Know_F2 <- data_filtered[data_filtered$Comp_Use_Know == "F2",]
var.test(Comp_Use_Know_F2$clarity, Comp_Use_Know_F2$System)
t.test(clarity ~ System, data=Comp_Use_Know_F2, var.equal = FALSE)

Comp_Use_Know_F3 <- data_filtered[data_filtered$Comp_Use_Know == "F3",]
var.test(Comp_Use_Know_F3$clarity, Comp_Use_Know_F3$System)
t.test(clarity ~ System, data=Comp_Use_Know_F3, var.equal = FALSE)

Comp_Use_Know_F4 <- data_filtered[data_filtered$Comp_Use_Know == "F4",]
var.test(Comp_Use_Know_F4$clarity, Comp_Use_Know_F4$System)
t.test(clarity ~ System, data=Comp_Use_Know_F4, var.equal = FALSE)


#Scheffe post-hoc test on Comp_Use_Know for System in {C,S}
library(agricolae)
comperison_S_C <- scheffe.test(anovaModel, "Comp_Use_Know", group = TRUE, console = TRUE, main = "Comp Use Know\n System in {C,S}")

#Scheffe post-hoc test on Comp_Use_Know for System = S
comperison_S <- scheffe.test(anovaModel_S, "Comp_Use_Know", group = TRUE, console = TRUE, main = "Comp Use Know\n System = S")

#Scheffe post-hoc test on Comp_Use_Know for System = C
comperison_C <- scheffe.test(anovaModel_C, "Comp_Use_Know", group = TRUE, console = TRUE, main = "Comp Use Know\n System= C")








