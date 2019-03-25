library("Hmisc")
library("ggpubr")
library("corrplot")
library("car")


#read our data- q50.csv#
my_data <- read.csv(file.choose())

#This function find the correlation  between variables as a correlation matrix using spearman function and print it to console##
cor(my_data[sapply(my_data, function(x) !is.factor(x))], method ="spearman")
correlation <- cor(my_data[sapply(my_data, function(x) !is.factor(x))], method ="spearman")

#plot the result of the correlation matrix#
corrplot(correlation, type = "full", order = "hclust", method = "number", tl.col = "black", tl.srt = 45)

#This function find thep_valuebetween variables using spearman function and print it to console##
rcorr(as.matrix(my_data[sapply(my_data, function(x) !is.factor(x))]), type = "spearman")
p_value <- rcorr(as.matrix(my_data[sapply(my_data, function(x) !is.factor(x))]), type = "spearman")

#plot the result of the p_value matrix#
corrplot(p_value$P, type = "full", order = "hclust", method = "number", tl.col = "black", tl.srt = 45)

#this is just one sample to show the scatterplot of two variables#
ggscatter(my_data, x = "Number.of.Bugs", y = "CountLineComment", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "CountLineComment")

ggscatter(my_data, x = "Number.of.Bugs", y = "CountLineCode", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "CountLineCode")

ggscatter(my_data, x = "Number.of.Bugs", y = "RatioCommentToCode", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "RatioCommentToCode")

ggscatter(my_data, x = "Number.of.Bugs", y = "PercentLackOfCohesion", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "PercentLackOfCohesion")

ggscatter(my_data, x = "Number.of.Bugs", y = "CountDeclMethodAll", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "CountDeclMethodAll")

ggscatter(my_data, x = "Number.of.Bugs", y = "CountDeclMethod", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "CountDeclMethod")

ggscatter(my_data, x = "Number.of.Bugs", y = "MaxInheritanceTree", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "MaxInheritanceTree")

ggscatter(my_data, x = "Number.of.Bugs", y = "CountLine", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "CountLine")

ggscatter(my_data, x = "Number.of.Bugs", y = "CountClassDerived", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Number.of.Bugs", ylab = "CountClassDerived")

train_set <- my_data[c(0:4),]
train_set
test_set <- my_data[c(5),]
test_set


model_bugs <- glm(formula = Number.of.Bugs ~ CountClassDerived
                    , data = train_set , family = 'poisson' )
summary(model_bugs)
anova(model_bugs,test="Chisq")

predict(model_bugs, test_set, type="response")


