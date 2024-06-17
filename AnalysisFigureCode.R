#Data Analysis of Survey and OSCE data
#install and load appropriate packages:
library(readxl)
library(dplyr)
library(ggstatsplot)
library(psych)
library(Hmisc)
library(jmv)
library(rstatix)
library(lme4)
library(AICcmodavg)
library(car)
library(lmtest)
library(ltm)

#Load data:
stats <- read_excel("replace_with_file_name_location.xslx", 
                      sheet = "stats", col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", #ensure the correct sheet name
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric"))
View(stats)

likertstats <- read_excel("replace_with_file_name_location.xlsx", 
                              sheet = "Likert_chart", col_types = c("text", #ensure sheet name is correct
                                                                      +         "numeric", "numeric", "numeric", 
                                                                      +         "numeric", "numeric", "numeric", 
                                                                      +         "numeric", "numeric", "numeric", 
                                                                      +         "numeric", "numeric", "numeric", 
                                                                      +         "numeric", "numeric", "numeric", 
                                                                      +         "numeric", "numeric", "numeric", 
                                                                      +         "numeric", "text"))
View(likertstats)

likert_chart_hh <- read_excel("replace_with_file_name_location.xlsx", sheet = "Likert_chart_hh") #Ensure appropraite sheet


View(likert_chart_hh)

#Analyze Likert data using non-parametric wilcoxon paired signed rank tests.

#Describe data using Hmisc package
desribe(stats)   #gives frequency, values, proportions, etc.  Need for making Likert Chart plots
hist.data.frame(stats)  #make histograms of all columns

#Function will eliminate those with NAs, packaged used is rstatix
wilcox_test(data=likertstats, q1 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q2 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q3 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q4 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q5 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q6 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q7 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q8 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q9 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q10 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q11 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q12 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q13 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q14 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q15 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q16 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q17 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)
wilcox_test(data=likertstats, q18 ~ prepost, paired = TRUE, ref.group = 'pre', detailed=TRUE)

#Likert plots
#install.packages("HH")
library(ggplot2)
library(HH)

#Sort data by pre and post lab
#Select pre-lab survey data
pre_direct <- likert_chart_hh %>%
  filter(pre_post == "pre" & direct_indirect == "direct")
pre_direct <- as.data.frame(pre_direct)

pre_indirect <- likert_chart_hh %>%
  filter(pre_post == "pre" & direct_indirect == "indirect")
pre_indirect <- as.data.frame(pre_indirect)

#Select post-lab survey data
post_direct <- likert_chart_hh %>%
  filter(pre_post == "post" & direct_indirect == "direct")
post_direct <- as.data.frame(post_direct)

post_indirect <- likert_chart_hh %>%
  filter(pre_post == "post" & direct_indirect == "indirect")
post_indirect <- as.data.frame(post_indirect)

#Plot data
HH::likert(Statement~ ., pre_direct, main="Student's Perceptions of Their Competency in Direct Fundoscopy Before Deliberate Practice",
           col = c('#440154FF', '#414487FF','#ececec', '#b8e186','#4dac26'), auto.key = list(columns=3))

HH::likert(Statement~ ., post_direct, main="Student's Perceptions of Their Competency in Direct Fundoscopy After Deliberate Practice",
           col = c('#440154FF', '#414487FF','#ececec', '#b8e186','#4dac26'), auto.key = list(columns=3))

HH::likert(Statement~ ., pre_indirect, main="Student's Perceptions of Their Competency in Indirect Fundoscopy Before Deliberate Practice",
           col = c('#440154FF', '#414487FF','#ececec', '#b8e186','#4dac26'), auto.key = list(columns=3))

HH::likert(Statement~ ., post_indirect, main="Student's Perceptions of Their Competency in Indirect Fundoscopy After Deliberate Practice",
           col = c('#440154FF', '#414487FF','#ececec', '#b8e186','#4dac26'), auto.key = list(columns=3))


#Generalized linear models
#First select data that is needed to check for correlations
glm_data  <- stats[,c("StudentIdentifier", "Time_cohort", "vet_tech_yrs", "vet_board_yrs", "Ave_overall",
                      "direct_video",	"indirect_video", "Ave_direct", "Ave_indirect")]
head(glm_data)
glm_data <- as.data.frame(glm_data)

#Check for correlations
cor(glm_data)
Corr <- cor(sapply(glm_data, as.numeric),
            use = "pairwise.complete.obs", method = "pearson")
Corr
corrplot::corrplot(Corr, method = "square", type = "upper",
                   tl.col = "black")

#Check for normality to better understand data, however note it is the residuals that need to be normal, not input variables
shapiro.test(glm_data$Ave_overall)  #normal
shapiro.test(glm_data$Ave_direct)  #failed, negative skewed
shapiro.test(glm_data$Ave_indirect)  #Normal
shapiro.test(glm_data$vet_tech_yrs) #failed
shapiro.test(glm_data$vet_board_yrs) #failed

shapiro.test(glm_data$indirect_video) #failed, positive skewed
shapiro.test(glm_data$direct_video)  #failed

#Histographs
hist.data.frame(glm_data)

#Make Time_cohort a factor.
glm_data$StudentIdentifier <- as.factor(glm_data$StudentIdentifier)
glm_data$Time_cohort <- as.factor(glm_data$Time_cohort)


#glm using stats package as some data is not normally distributed.  Use studentIdentifier as random var if more than 1 observation per individual
#Use linear models and assess residuals and other assumptions.  Do not include studentID as random but fixed if 1 observation only.
nullmodel <- glm(Ave_overall ~ 1, data=glm_data) #glm from stats R package
all_vars <- glm(Ave_overall ~ vet_tech_yrs + vet_board_yrs + indirect_video + direct_video + Time_cohort, data=glm_data)
all_vars_int <- glm(Ave_overall ~ vet_tech_yrs + vet_board_yrs + indirect_video + direct_video + Time_cohort
                    + vet_tech_yrs*Time_cohort + indirect_video*Time_cohort + direct_video*Time_cohort, data=glm_data)
summary(nullmodel)
summary(all_vars)
summary(all_vars_int)
#Build models then eliminate any interactions that are not sigificant
model1 <- glm(Ave_overall ~ Time_cohort, data=glm_data)
summary(model1)
model2 <- glm(Ave_overall ~ Time_cohort + vet_board_yrs, data=glm_data) #Drop interaction as not significant
summary(model2)
model3 <- glm(Ave_overall ~ Time_cohort + vet_board_yrs + vet_tech_yrs + vet_tech_yrs*vet_board_yrs, data=glm_data)
summary(model3)



#Use AICcmodavg to compare AIC values
candidates = list(nullmodel, all_vars, all_vars_int, model1, model2, model3)
model.names = c("null", "all_vars", "all_vars_int", "m1", "m2", "m3")
# Be sure that the order of model names matches
# the order used for the candidate models.
aictab(cand.set = candidates, modnames = model.names, sort = TRUE)

#All models better than the null model except the one with interactions.
#Best fitting models are m3 and m2.
summary(model2)  #Shows time cohort is significant.
summary(model3)
#Linearity assumption
avPlots(all_vars)
avPlots(model1)
avPlots(model3)

#Independence assumption:
durbinWatsonTest(all_vars)
durbinWatsonTest(model1)
durbinWatsonTest(model3)

#Homoscedasticity assumption
bptest(all_vars)
bptest(model1)
bptest(model3)

#Plot residuals in Q-Q plot
res <- resid(all_vars)
qqnorm(res)
qqline(res)

res <- resid(model2)
qqnorm(res)
qqline(res)

res <- resid(model3)
qqnorm(res)
qqline(res)
#Q-Q plot seems to show   very good distribution
#Post hoc tests for significant models
#Overall OSCE
all_vars_post <- emmeans(all_vars, ~Time_cohort)
contrast(all_vars_post, "pairwise", adjust = "Tukey")

summary(model2)
model2_post <- emmeans(model2, ~Time_cohort)
contrast(model1_post, "pairwise", adjust = "Tukey")

summary(model3)
model3_post <- emmeans(model3, ~Time_cohort)
contrast(model3_post, "pairwise", adjust = "Tukey")

# 12 min is significantly different than 24 min and 36 min, but 24 min and 36 min are not different between each other
#No issues with model assumptions


#Cronbachs alpha
ca <- stats[,c("Ave_overall", "Ave_direct", "Ave_indirect")]
ca_data <- as.data.frame(ca)
cronbach.alpha(data=ca_data)

ca <- stats[,c("S1finalscore", "S2finalscore", "S3finalscore", "S4finalscore")]
ca_data <- as.data.frame(ca)
cronbach.alpha(data=ca_data)


#glm using stats package as some data is not normally distributed.
nullmodelD <- glm(Ave_direct ~ 1, data=glm_data) #null model
all_varsD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort, data=glm_data)
all_vars_intD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort
                    + vet_tech_yrs*Time_cohort + direct_video*Time_cohort, data=glm_data)  #Drop interactions as not significant
model1D <- glm(Ave_direct ~ vet_tech_yrs + direct_video + Time_cohort, data=glm_data)
model2D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort, data=glm_data)
model3D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort + vet_tech_yrs*Time_cohort, data = glm_data)

summary(all_varsD)
summary(all_vars_intD)
summary(model1D)
summary(model2D)
summary(model3D)
#Use AICcmodavg to compare AIC values
candidates = list(nullmodelD, all_varsD, all_vars_intD, model1D, model2D, model3D)
model.names = c("null", "all_vars", "all_vars_int", "m1", "m2", "m3")
# Be sure that the order of model names matches
# the order used for the candidate models.
aictab(cand.set = candidates, modnames = model.names, sort = TRUE)

#Assess top two models
summary(model2D)
summary(model3D) 
#No significance.

#Plot Q-Q plot
resD <- resid(model2D)
qqnorm(resD)
qqline(resD)
resD <- resid(model3D)
qqnorm(resD)
qqline(resD)
#Data seems a bit skewed, assess normality for residuals
# Extract standardized residuals from the QQ plot
residuals <- residuals(model2D)
standardized_residuals <- residuals / sd(residuals)
shapiro.test(standardized_residuals)


residuals <- residuals(model3D)
standardized_residuals <- residuals / sd(residuals)
shapiro.test(standardized_residuals)

#Both of the models, the residuals fail normal distribution therefore the model is violated.
#Do weighted regressions
#glm_data1 <- glm_data[complete.cases(glm_data), ] #remove rows with NAs as otherwise weighted models failed
#Run models without Nas first before calculating weights
nullmodelD <- glm(Ave_direct ~ 1, data=glm_data) #null model
all_varsD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort, data=glm_data)
all_vars_intD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort
                     + vet_tech_yrs*Time_cohort + direct_video*Time_cohort, data=glm_data)  #Drop interactions as not significant
model1D <- glm(Ave_direct ~ vet_tech_yrs + direct_video + Time_cohort, data=glm_data)
model2D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort, data=glm_data)
model3D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort + vet_tech_yrs*Time_cohort, data = glm_data)


wtNull <- 1 / lm(abs(nullmodelD$residuals) ~ nullmodelD$fitted.values)$fitted.values^2  #observations with lower variance are given more weight
wtAll <- 1 / lm(abs(all_varsD$residuals) ~ all_varsD$fitted.values)$fitted.values^2 
wt1 <- 1 / lm(abs(model1D$residuals) ~ model1D$fitted.values)$fitted.values^2 
wt2 <- 1 / lm(abs(model2D$residuals) ~ model2D$fitted.values)$fitted.values^2 
wt3 <- 1 / lm(abs(model3D$residuals) ~ model3D$fitted.values)$fitted.values^2 

nullmodelD <- glm(Ave_direct ~ 1, data=glm_data, weights=wtNull) #null model
all_varsD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort, data=glm_data, weights = wtAll)
all_vars_intD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort
                     + vet_tech_yrs*Time_cohort + direct_video*Time_cohort, data=glm_data, weights=wtAll)  #Drop interactions as not significant
model1D <- glm(Ave_direct ~ vet_tech_yrs + direct_video + Time_cohort, data=glm_data, weights = wt1)
model2D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort, data=glm_data, weights=wt2)
model3D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort + vet_tech_yrs*Time_cohort, data = glm_data, weights = wt3)
#All these fail due to NAs for participant 29.
#Remove student record
glm_data1 <- glm_data[complete.cases(glm_data), ] #remove rows with NAs as otherwise weighted models failed
#Run models without Nas first before calculating weights
nullmodelD <- glm(Ave_direct ~ 1, data=glm_data1) #null model
all_varsD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort, data=glm_data1)
all_vars_intD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort
                     + vet_tech_yrs*Time_cohort + direct_video*Time_cohort, data=glm_data1)  #Drop interactions as not significant
model1D <- glm(Ave_direct ~ vet_tech_yrs + direct_video + Time_cohort, data=glm_data1)
model2D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort, data=glm_data1)
model3D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort + vet_tech_yrs*Time_cohort, data = glm_data1)


wtNull <- 1 / lm(abs(nullmodelD$residuals) ~ nullmodelD$fitted.values)$fitted.values^2  #observations with lower variance are given more weight
wtAll <- 1 / lm(abs(all_varsD$residuals) ~ all_varsD$fitted.values)$fitted.values^2 
wt1 <- 1 / lm(abs(model1D$residuals) ~ model1D$fitted.values)$fitted.values^2 
wt2 <- 1 / lm(abs(model2D$residuals) ~ model2D$fitted.values)$fitted.values^2 
wt3 <- 1 / lm(abs(model3D$residuals) ~ model3D$fitted.values)$fitted.values^2 

nullmodelD <- glm(Ave_direct ~ 1, data=glm_data1, weights=wtNull) #null model
all_varsD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort, data=glm_data1, weights = wtAll)
all_vars_intD <- glm(Ave_direct ~ vet_tech_yrs + vet_board_yrs + direct_video + Time_cohort
                     + vet_tech_yrs*Time_cohort + direct_video*Time_cohort, data=glm_data1, weights=wtAll)  #Drop interactions as not significant
model1D <- glm(Ave_direct ~ vet_tech_yrs + direct_video + Time_cohort, data=glm_data1, weights = wt1)
model2D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort, data=glm_data1, weights=wt2)
model3D <- glm(Ave_direct ~ vet_tech_yrs + Time_cohort + vet_tech_yrs*Time_cohort, data = glm_data1, weights = wt3)


#Use AICcmodavg to compare AIC values
candidates = list(nullmodelD, all_varsD, all_vars_intD, model1D, model2D, model3D)
model.names = c("null", "all_vars", "all_vars_int", "m1", "m2", "m3")
# Be sure that the order of model names matches
# the order used for the candidate models.
aictab(cand.set = candidates, modnames = model.names, sort = TRUE)

#Look at summary AIC value for null and model 3D
summary(nullmodelD)  #AIC of 506.38
summary(model3D)   #AIC of 505.56

#Still plot Q-Q plot
resD <- resid(model3D)
qqnorm(resD)
qqline(resD)

residuals <- residuals(model2D)
standardized_residuals <- residuals / sd(residuals)
shapiro.test(standardized_residuals)  #fails

#Just assess time cohort and direct scores with Kruskal Wallis test
kruskal.test(Ave_direct ~ Time_cohort, data=glm_data)  #Not significant


#glm using stats package as some data is not normally distributed.
nullmodelI <- glm(Ave_indirect ~ 1, data=glm_data)
all_varsI <- glm(Ave_indirect ~ vet_tech_yrs + vet_board_yrs + indirect_video + Time_cohort, data=glm_data)
all_vars_intI <- glm(Ave_indirect ~ vet_tech_yrs + vet_board_yrs + indirect_video + Time_cohort
                     + vet_tech_yrs*Time_cohort + indirect_video*Time_cohort, data=glm_data)
model1I <- glm(Ave_indirect ~ vet_tech_yrs + indirect_video + Time_cohort, data=glm_data)
model2I <- glm(Ave_indirect ~ vet_tech_yrs + Time_cohort, data=glm_data)
model3I <- glm(Ave_indirect ~ vet_tech_yrs + Time_cohort + vet_tech_yrs*Time_cohort, data = glm_data)
model4I <- glm(Ave_indirect ~ + Time_cohort, data = glm_data)

#Use AICcmodavg to compare AIC values
candidates = list(nullmodelI, all_varsI, all_vars_intI, model1I, model2I, model3I, model4I)
model.names = c("null", "all_vars", "all_vars_int", "m1", "m2", "m3", "m4")
# Be sure that the order of model names matches
# the order used for the candidate models.
aictab(cand.set = candidates, modnames = model.names, sort = TRUE)

#Assess top two models
summary(model2I)
summary(model1I)

#Linearity assumption
avPlots(model1I)
avPlots(all_varsI)

#Independence assumption:
durbinWatsonTest(model2I)
durbinWatsonTest(model1I)

#Homoscedasticity assumption
bptest(model2I)
bptest(model1I)

#Indirect OSCE
model2I_post <- emmeans(model2I, ~Time_cohort)
contrast(model2I_post, "pairwise", adjust = "Tukey")


# 12 min is significantly different than 24 min only.
#Make plots
library(viridis)
glm_data %>%
  ggplot(aes(x=Time_cohort, y=Ave_overall, fill=Time_cohort)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(color="black", size=2, alpha=0.9, show.legend = FALSE) +
  labs(y="Overall OSCE Score", x= "Time Cohort (min)") +  #removing title title = "Overall OSCE Fundoscopy Scores", 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  ylim(0,70) +
  theme(axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15), axis.text = element_text(size=15))

glm_data %>%
  ggplot(aes(x=Time_cohort, y=Ave_direct, fill=Time_cohort)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(color="black", size=2, alpha=0.9, show.legend = FALSE) +
  labs(y="Direct OSCE Score", x= "Time Cohort (min)") + #removing title title = "Direct OSCE Fundoscopy Scores", 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  ylim(0,40)  +
  theme(axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15), axis.text = element_text(size=15))

glm_data %>%
  ggplot(aes(x=Time_cohort, y=Ave_indirect, fill=Time_cohort)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(color="black", size=2, alpha=0.9, show.legend = FALSE) +
  labs(y="Indirect OSCE Score", x= "Time Cohort (min)") +  #removing title title = "Indirect OSCE Fundoscopy Scores", 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  ylim(0,40) +
  theme(axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 15), axis.text = element_text(size=15))
