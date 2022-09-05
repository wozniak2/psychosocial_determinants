##-----------------------------------------------------------------------------------------
## The script contains estimation details for the paper:
## Psychosocial determinants of recreational activity within urban green spaces 
## during the COVID-19 pandemic
## by Sandra Wajchman-Switalska, Olga Grabowska-Chenczke, Marcin Wozniak and Bibianna Bałaj
## doi: 
## Session details can be found in Readme file
## ----------------------------------------------------------------------------------------

## Load rec2.csv data to get it work
## TO DO: adjust your path
rec2 <- read.csv("C:/Users/.../rec2.csv")
View(rec2)

## load packages
require(generalhoslem)
require(ResourceSelection)
library(MASS)
library(effects)
library(gridExtra) 
options(contrasts = c("contr.treatment", "contr.poly"))

## -----------------------
## PART 1: Social determinants
## -----------------------

## variables preprocessing

# season of the year
rec$season <- recreation$PoraRokuRekreacji 
rec2$season <- factor(rec2$season, levels = c("1", "2", "3", "4"), ordered = FALSE)

# dependent variable - ordinal response
rec2$Frequency_of_activity <- factor(rec2$Frequency_of_activity, 
                                     levels = c("everyday", "monday-friday", 
                                                "weekends", "few_times_in_month_or_less"), ordered = TRUE)

## categorical variables
# educacation level
rec2$edu <- factor(rec2$edu, levels = c("1", "2", "3", "4", "5"), ordered = FALSE)

# marital status
rec2$status <- factor(rec2$status, levels = c("single", "married", "divorced", "widow(er)"), ordered = FALSE)

# employment status
rec2$job <- factor(rec2$job, levels = c("1", "2", "3", "4", "5", "6", "7"), ordered = FALSE)

#disability
rec2$disability <- factor(rec2$disability, levels = c("Yes", "No"), ordered = FALSE)

# sex
rec2$Sex <- factor(rec2$Sex, levels = c("1", "2"), ordered = FALSE)

# place of residence
rec2$PlaceOfLive <- factor(rec2$PlaceOfLive, 
                           levels = c("village", "city_20000", "city_100000", 
                                      "city_500000", "city_above_500000"), ordered = FALSE)


## numeric variables
# age
rec2$Age <- as.numeric(rec2$Age)
# number of kids
rec2$kids <- as.numeric(rec2$kids)


## ---------------------------------------------
## ordinal logisitc model for social variables
## ---------------------------------------------

## model estimation
m1 <- NULL
m1 <- polr(Frequency_of_activity~PlaceOfLive+status+kids+Sex+job+edu+disability, 
           data=rec2, Hess = TRUE)

summary(m1)

## calculate and store p values

ctable <- coef(summary(m1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
options(scipen=999)
(ctable <- cbind(ctable, "p value" = p))

## confidence intervals for m1
(ci <- confint(m1))

## some additional plots for the m1 (not included in the paper)
pr <- profile(m1)
plot(pr)
pairs(pr)

## Plotting the effects 

p1 <- plot(predictorEffect("PlaceOfLive", m1, residuals=TRUE), 
           lines=list(multiline=TRUE, z.var="", lty=1:4), 
           lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                           columns=1,
                                                                                           border=F,
                                                                                           fontfamily="serif",
                                                                                           cex=1.1,
                                                                                           cex.title=0,
                                                                                           title="")),
           main="", symbols=list(pch=14:17, cex=1.5),
           #    confint=list(style="auto"),
           axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="place of residence"), y=list(type="response"))))

p2 <- plot(predictorEffect("status", m1, residuals=TRUE), 
           lines=list(multiline=TRUE, z.var="", lty=1:4), 
           lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                           columns=1,
                                                                                           border=F,
                                                                                           fontfamily="serif",
                                                                                           cex=1.1,
                                                                                           cex.title=0,
                                                                                           title="")),
           main="", symbols=list(pch=14:17, cex=1.5),
           #    confint=list(style="auto"),
           axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="status"), y=list(type="response"))))



p3 <- plot(predictorEffect("disability", m1, residuals=TRUE), 
           lines=list(multiline=TRUE, z.var="", lty=1:4), 
           lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                           columns=1,
                                                                                           border=F,
                                                                                           fontfamily="serif",
                                                                                           cex=1.1,
                                                                                           cex.title=0,
                                                                                           title="")),
           main="", symbols=list(pch=14:17, cex=1.5),
           #    confint=list(style="auto"),
           axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="disability"), y=list(type="response"))))


p4 <-  plot(predictorEffect("kids", m1, residuals=TRUE), 
            lines=list(multiline=TRUE, z.var="", lty=1:4), 
            lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                            columns=1,
                                                                                            border=F,
                                                                                            fontfamily="serif",
                                                                                            cex=1.1,
                                                                                            cex.title=0,
                                                                                            title="")),
            main="", symbols=list(pch=14:17, cex=1.5),
            #      confint=list(style="auto"),
            axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="number of kids"), y=list(type="response"))))


grid.arrange(p1, p2, p3, p4, ncol = 2)



## testing output
# Lipsitz test
lipsitz.test(m1)

# Pulkstenis-Robinson test
pulkrob.chisq(m1, c("PlaceOfLive", "status", "kids", "Sex", "job", "edu", "dis"))

## ----------------------------------
## PART 2: psychological determinants
## ----------------------------------

## data preprocessing - stress coping strategies

rec2$active_coping <- as.numeric(rec2$active_coping)
rec2$helplesness <- as.numeric(rec2$helplesness)
rec2$support_seeking <- as.numeric(rec2$support_seeking)
rec2$avoidance <- as.numeric(rec2$avoidance)
rec2$acceptance <- as.numeric(rec2$acceptance)
rec2$humor <- as.numeric(rec2$humor)
rec2$religion <- as.numeric(rec2$religion)

## ------------------------------------------------------------------------------------------
## model for 7 simplified strategies extracted on a basis of Carver Brief COPE Inventory (m3)
## ------------------------------------------------------------------------------------------

## model estimation
m3 <- NULL
m3 <- polr(Frequency_of_activity ~ active_coping+helplesness+support_seeking+
             avoidance+acceptance+humor+religion, data=rec2, Hess = TRUE,
           method = "logistic")

summary(m3)
ctable2 <- coef(summary(m3))
## calculate and store p values
p2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2

## combined table
options(scipen=999)
(ctable2 <- cbind(ctable2, "p value" = p2))

(ci <- confint(m3))


ctable3 <- coef(summary(m4))
## calculate and store p values
p3 <- pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2

## combined table
options(scipen=999)
(ctable3 <- cbind(ctable3, "p value" = p3))

## testing output
# Lipsitz test
lipsitz.test(m3)
# Pulkstenis-Robinson test
pulkrob.chisq(m3, c("active_coping", "helplesness", "support_seeking", 
                    "avoidance", "acceptance", "humor", "religion"))

## ---------------------------------------------------------------
## model for raw 14 strategies from the Brief-COPE Inventory (m5)
## ---------------------------------------------------------------

## model estimation
m5 <- NULL
m5 <- polr(Frequency_of_activity ~ active_coping + planning + positive_reframing + 
             acceptance + humor + religion + emotional_support + instrumental_support +
             attention + denial + venting + substance_use + disengagement + self_blame, data=rec2)


summary(m5)

## calculate and store p values
ctable5 <- coef(summary(m5))
p5 <- pnorm(abs(ctable5[, "t value"]), lower.tail = FALSE) * 2

## combined table
options(scipen=999)
(ctable5 <- cbind(ctable5, "p value" = p5))

## testing output
# Lipsitz test
lipsitz.test(m5)
# Pulkstenis-Robinson test
pulkrob.chisq(m5, c("active_coping", "planning", "positive_reframing", "acceptance",
                    "humor", "religion", "emotional_support", "instrumental_support", "attention+denial",
                    "venting", "substance_use", "disengagement", "self_blame"))



## ----------------------------
## Plot results for m5
## ---------------------------
p5 <- plot(predictorEffect("acceptance", m5, residuals=TRUE), 
           lines=list(multiline=TRUE, z.var="", lty=1:4), 
           lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                           columns=1,
                                                                                           border=F,
                                                                                           fontfamily="serif",
                                                                                           cex=1.1,
                                                                                           cex.title=0,
                                                                                           title="")),
           main="", symbols=list(pch=14:17, cex=1.5),
           #      confint=list(style="auto"),
           axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="acceptance strategy"), y=list(type="response"))))

p6 <- plot(predictorEffect("attention", m5, residuals=TRUE), 
           lines=list(multiline=TRUE, z.var="", lty=1:4), 
           lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                           columns=1,
                                                                                           border=F,
                                                                                           fontfamily="serif",
                                                                                           cex=1.1,
                                                                                           cex.title=0,
                                                                                           title="")),
           main="", symbols=list(pch=14:17, cex=1.5),
           #      confint=list(style="auto"),
           axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="self-distraction strategy"), y=list(type="response"))))


p7 <- plot(predictorEffect("denial", m5, residuals=TRUE), 
           lines=list(multiline=TRUE, z.var="", lty=1:4), 
           lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                           columns=1,
                                                                                           border=F,
                                                                                           fontfamily="serif",
                                                                                           cex=1.1,
                                                                                           cex.title=0,
                                                                                           title="")),
           main="", symbols=list(pch=14:17, cex=1.5),
           #      confint=list(style="auto"),
           axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="denial strategy"), y=list(type="response"))))

  
grid.arrange(p5, p6, p7, ncol = 2)


## ----------------------------
## Plot results for m3
## ---------------------------
p8 <- plot(predictorEffect("acceptance", m3, residuals=TRUE), 
           lines=list(multiline=TRUE, z.var="", lty=1:4), 
           lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                           columns=1,
                                                                                           border=F,
                                                                                           fontfamily="serif",
                                                                                           cex=1.1,
                                                                                           cex.title=0,
                                                                                           title="")),
           main="", symbols=list(pch=14:17, cex=1.5),
           #      confint=list(style="auto"),
           axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="acceptance strategy"), y=list(type="response"))))


p9 <- plot(predictorEffect("active_coping", m3, residuals=TRUE), 
           lines=list(multiline=TRUE, z.var="", lty=1:4), 
           lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                           columns=1,
                                                                                           border=F,
                                                                                           fontfamily="serif",
                                                                                           cex=1.1,
                                                                                           cex.title=0,
                                                                                           title="")),
           main="", symbols=list(pch=14:17, cex=1.5),
           #      confint=list(style="auto"),
           axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="active coping strategy"), y=list(type="response"))))


p10 <- plot(predictorEffect("avoidance", m3, residuals=TRUE), 
            lines=list(multiline=TRUE, z.var="", lty=1:4), 
            lattice=list(strip=list(factor.names=TRUE, values=TRUE, cex=1.5), key.args=list(space="top",
                                                                                            columns=1,
                                                                                            border=F,
                                                                                            fontfamily="serif",
                                                                                            cex=1.1,
                                                                                            cex.title=0,
                                                                                            title="")),
            main="", symbols=list(pch=14:17, cex=1.5),
            #      confint=list(style="auto"),
            axes=list(grid=TRUE, x=list(rotate=30, spp=list(lab="avoidance strategy"), y=list(type="response"))))

  
grid.arrange(p8, p9, p10, ncol = 2)

