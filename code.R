library(lme4)
library(caret)
library(pROC)
library(ggplot2)
library(multcomp)
library(party)
library(readr)

source("functions.R")
source("plot-funcs.R")

data <- read_csv("data_final.csv")

### Make the plots

type.agreement.plot = ggplot(data, aes(Agreement2))+
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    ylab("percent")+xlab("Agreement") +
    scale_y_continuous(labels = scales::percent)
type.agreement.plot

## ggsave(file="type-agreement.eps", type.agreement.plot, height=6, width=4 )

agreement.combinations.plot = ggplot(data, aes(AgreementComb, fill=Agreement2))+
    geom_bar(aes(y = ((..count..)/sum(..count..))*6), position="dodge")+
    ylab("percent")+xlab("Agreement Combination")+
    scale_fill_grey(start = 0.1, end = .65)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent)
agreement.combinations.plot

## ggsave(file= "agreement-combination.eps", agreement.combinations.plot, height=6, width=6 )

## models
## there is more information in the models below than we were able to include
## in the final version of the paper

## load data

data1.cv <- read_csv("data-1.csv")
data2.cv <- read_csv("data-2.csv")
data3.cv <- read_csv("data-3.csv")

## make factors

data1.cv$Agreement <- as.factor(data1.cv$Agreement)
data2.cv$Agreement <- as.factor(data2.cv$Agreement)
data3.cv$Agreement <- as.factor(data3.cv$Agreement)

data1.cv$Gender.N1 <- as.factor(data1.cv$Gender.N1)
data2.cv$Gender.N1 <- as.factor(data2.cv$Gender.N1)
data3.cv$Gender.N1 <- as.factor(data3.cv$Gender.N1)

data1.cv$Gender.N2 <- as.factor(data1.cv$Gender.N2)
data2.cv$Gender.N2 <- as.factor(data2.cv$Gender.N2)
data3.cv$Gender.N2 <- as.factor(data3.cv$Gender.N2)

data1.cv$Adjective <- as.factor(data1.cv$Adjective)
data2.cv$Adjective <- as.factor(data2.cv$Adjective)
data3.cv$Adjective <- as.factor(data3.cv$Adjective)

data1.cv$Speaker <- as.factor(data1.cv$Speaker)
data2.cv$Speaker <- as.factor(data2.cv$Speaker)
data3.cv$Speaker <- as.factor(data3.cv$Speaker)

## Formula for the models

form <- formula("Agreement ~ Gender.N1 + Gender.N2 + Sex + (1|Adjective) + (1|Speaker)")
formN2 <- formula("Agreement ~ Gender.N1 + Sex + (1|Adjective) + (1|Speaker)")
formN1 <- formula("Agreement ~ Gender.N2 + Sex + (1|Adjective) + (1|Speaker)")
formSex <- formula("Agreement ~ Gender.N1 + Gender.N2 + (1|Adjective) + (1|Speaker)")

## N1 vs N2

data1.glmer <- glmer(form,
    data = data1.cv, family="binomial",
    control=glmerControl(optimizer="bobyqa"))
summary(data1.glmer)

summary(glht(data1.glmer, mcp(Gender.N1="Tukey")))
summary(glht(data1.glmer, mcp(Gender.N2="Tukey")))

data1.glmer.aic <- summary(data1.glmer)$AIC[1]
data1.N1 <- glmer(formN1,
    data = data1.cv, family="binomial",
    control=glmerControl(optimizer="bobyqa"))
data1.N1.aic <- summary(data1.N1)$AIC[1]
(data1.N1.aic-data1.glmer.aic)*log2(exp(1))

data1.N2 <- glmer(formN2,
    data = data1.cv, family="binomial",
    control=glmerControl(optimizer="bobyqa"))
data1.N2.aic <- summary(data1.N2)$AIC[1]
(data1.N2.aic-data1.glmer.aic)*log2(exp(1))

data1.Sex <- glmer(formSex,
    data = data1.cv, family="binomial",
    control = glmerControl(optimizer="bobyqa"))
data1.Sex.aic = summary(data1.Sex)$AIC[1]
(data1.Sex.aic-data1.glmer.aic)*log2(exp(1))

# plots for random effects:

n1n2.plot <- plot.raneff(data1.glmer,"Speaker")
n1n2.plot
## ggsave(file="n1n2.pdf", n1n2.plot, height=6, width=4)
n1n2.adj.plot <- plot.raneff(data1.glmer, "Adjective")
n1n2.adj.plot
## ggsave(file="n1n2-adj.pdf", n1n2.adj.plot,height=6, width=4)

## Prediction

data1.glmer.pred <- predict(data1.glmer, type = "response")
data1.glmer.predicted <- ifelse(data1.glmer.pred > 0.5, 1, 0)
data1.cv$Agreement1 <- ifelse(data1.cv$Agreement == "N1", 0,1)
confusionMatrix(data1.glmer.predicted, data1.cv$Agreement1)

## Bootstrap

set.seed(1234)
results.N1N2 <- bootstrap(form, data1.cv)
results.N1N2

## N2 vs O

data2.glmer <- glmer(form,
    data = data2.cv,
    control = glmerControl(optimizer = "bobyqa"),
    family = "binomial")
summary(data2.glmer)

summary(glht(data2.glmer, mcp(Gender.N1 = "Tukey")))
summary(glht(data2.glmer, mcp(Gender.N2 = "Tukey")))

## AIC

data2.glmer.aic <- summary(data2.glmer)$AIC[1]
data2.N1 <- glmer(formN1,
    data = data2.cv, family="binomial",
    control=glmerControl(optimizer="bobyqa"))
data2.N1.aic <- summary(data2.N1)$AIC[1]
(data2.N1.aic-data2.glmer.aic)*log2(exp(1))

data2.N2 <- glmer(formN2,
    data = data2.cv, family="binomial",
    control=glmerControl(optimizer="bobyqa"))
data2.N2.aic <- summary(data2.N2)$AIC[1]
(data2.N2.aic-data2.glmer.aic)*log2(exp(1))

data2.Sex <- glmer(formSex,
    data = data2.cv, family="binomial",
    control=glmerControl(optimizer="bobyqa"))
data2.Sex.aic <- summary(data2.Sex)$AIC[1]
(data2.Sex.aic-data2.glmer.aic)*log2(exp(1))

## Plot random effects

n2n0.plot <- plot.raneff(data2.glmer)
n2n0.plot
## ggsave(file="n2n0.pdf", n2n0.plot, height=6, width=4)

n2n0.adj.plot <- plot.raneff(data2.glmer, "Adjective")
n2n0.adj.plot
## ggsave(file="n2n0-adj.pdf", n2n0.adj.plot,height=6, width=4)


## Prediction

data2.glmer.pred <- predict(data2.glmer, type="response")
data2.glmer.predicted <- ifelse(data2.glmer.pred>0.5,1,0)
data2.cv$Agreement1 <- ifelse(data2.cv$Agreement=="N2", 0,1)
confusionMatrix(data2.glmer.predicted, data2.cv$Agreement1)

## Bootstrap

set.seed(1234)
results.N2N0 <- bootstrap(form, data2.cv)
results.N2N0

## N1 vs O

data3.glmer <- glmer(form,
    data = data3.cv,
    control=glmerControl(optimizer="bobyqa"),
    family="binomial")
summary(data3.glmer)

summary(glht(data3.glmer, mcp(Gender.N1="Tukey")))
summary(glht(data3.glmer, mcp(Gender.N2="Tukey")))

## AIC

data3.glmer.aic <- summary(data3.glmer)$AIC[1]
data3.N1 <- glmer(formN1,
    data = data3.cv, family = "binomial",
    control=glmerControl(optimizer = "bobyqa"))
data3.N1.aic <- summary(data3.N1)$AIC[1]
(data3.N1.aic-data3.glmer.aic)*log2(exp(1))

data3.N2 <- glmer(formN2,
    data = data3.cv, family = "binomial",
    control=glmerControl(optimizer = "bobyqa"))
data3.N2.aic <- summary(data3.N2)$AIC[1]
(data3.N2.aic-data3.glmer.aic)*log2(exp(1))

data3.Sex <- glmer(formSex,
    data = data3.cv, family = "binomial",
    control=glmerControl(optimizer = "bobyqa"))
data3.Sex.aic =summary(data3.Sex)$AIC[1]
(data3.Sex.aic-data3.glmer.aic)*log2(exp(1))

## Plot random effects

n1n0.plot <- plot.raneff(data3.glmer)
n1n0.plot
## ggsave(file="n1n0.pdf", n1n0.plot, height=6, width=4)

n1n0.adj.plot <- plot.raneff(data3.glmer, "Adjective")
n1n0.adj.plot
## ggsave(file="n1n0-adj.pdf", n1n0.adj.plot,height=6, width=4)

#Prediction
data3.glmer.pred <- predict(data3.glmer, type = "response")
data3.glmer.predicted <- ifelse(data3.glmer.pred > 0.5, 1, 0)
data3.cv$Agreement1 <- ifelse(data3.cv$Agreement == "N1", 0, 1)
confusionMatrix(data3.glmer.predicted, data3.cv$Agreement1)

#Bootstrap

results.N1N0 <- bootstrap(form, data3.cv)
results.N1N0

## tree

data.tree <- read_csv("data-final-small.csv")

## for plotting we fix some labels first

data.tree$Adjective <- as.character(data.tree$Adjective)
data.tree$Adjective[data.tree$Adjective=="straff"] <- "straff\n"
data.tree$Adjective[data.tree$Adjective=="krank"] <- "\nkrank"
## data.tree$Adjective[data.tree$Adjective=="koestlic"] <- "koestlich"
data.tree$Adjective[data.tree$Adjective=="traurig"] <- "\ntraurig"
data.tree$Adjective <- as.factor(data.tree$Adjective)

data.tree$Speaker <- as.character(data.tree$Speaker)
data.tree$Speaker[data.tree$Speaker=="21"] <- "21\n"
data.tree$Speaker <- as.factor(data.tree$Speaker)
data.tree$Gender.N1 <- as.factor(data.tree$Gender.N1)
data.tree$Gender.N2 <- as.factor(data.tree$Gender.N2)
data.tree$Sex <- as.factor(data.tree$Sex)
data.tree$Agreement <- as.factor(data.tree$Agreement)

s.data.tree1 = ctree(Agreement ~ Gender.N1 + Gender.N2 + Sex + Speaker + Adjective,
    data = data.tree)
plot(s.data.tree1)

## pdf("tree-1.pdf", height=15, width=20)
##    plot(s.data.tree1)
## dev.off()
