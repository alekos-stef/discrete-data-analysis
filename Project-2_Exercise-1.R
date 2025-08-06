######## PROJECT 2 - Exercise 1, ΑΣΚΗΣΗ 1   #########

## PART A ##

# Εισαγωγή Δεδομένων
mydata <- read.table("C:/Users/aleks/Desktop/binary.txt", col.names = c("admit", "gre", "gpa", "rank"))
attach(mydata)

table(rank)
table(admit)
table(rank,admit)

mylogit <- glm(admit~gre, family = binomial(link = "logit"), na.action = na.pass)
summary(mylogit)

mylogit$null.deviance - mylogit$deviance    # Τιμή Στατιστικής Συνάρτησης
mylogit$df.null - mylogit$df.residual   # Βαθμός ελευθερίας
1 - pchisq(mylogit$null.deviance - mylogit$deviance, mylogit$df.null - mylogit$df.residual)  # P-value

# 95% διαστήματα εμπιστοσύνης για τους συντελεστές του μοντέλου
confint(mylogit)
exp(confint(mylogit))   # Διαστήματα εμπιστοσύνης για τα odds ratio

# Υπολογισμός πιθανότητας
predict.glm(mylogit, type = "response", newdata = data.frame(gre=587.7))

#Υπολογισμός median effective level
EL50 <- -mylogit$coefficients[1]/mylogit$coefficients[2]
EL50

# Υπολογισμός της κλίσης της προσεγγιστικής ευθείας στο σημέιο 587.7
mylogit$coefficients[2]*predict.glm(mylogit, type="response", newdata = data.frame(gre=587.7)*(1-predict.glm(mylogit, type="response", newdata = data.frame(gre=587.7))))

# Υπολογισμός της σχετικής πιθανότητας (odds) στο 587.7
p <- predict.glm(mylogit, type="response", newdata = data.frame(gre=587.7))
p/(1-p)

# Υπολογισμός 95% διαστηματος εμπιστοσύνης για το logit
vec <- c(1,587.7)
t(vec)%*%summary(mylogit)$cov.unscaled %*%vec
SE <- sqrt(t(vec)%*%summary(mylogit)$cov.unscaled%*%vec);SE
p <- predict(glm(mylogit, type="response", newdata=data.frame(gre=587.7)))
ci <- c(log(p/(1-p)) - 1.96*SE, log(p/(1-p)) +1.96*SE ) ; ci

ci1 <- c(exp(ci[1])/(1+exp(ci[1])), exp(ci[2])/(1+exp(ci[2])) ) ; ci1

# Κατασκευή Πίνακα Ταξινόμησης και Υπολογισμός ποσοστού ορθής ταξινόμησης
admithat <- fitted(mylogit)
thresh <- 0.5
admithatFac <- cut(admithat, breaks = c(-Inf, thresh, Inf), labes = c(0,1))
cTab <- table(admit, admithatFac)
addmargins(cTab)

# Ποσοστό Ορθής Ταξινόμησης 
sum(diag(cTab))/sum(cTab)

## PART B ##

# Προσαρμογή Μοντέλου Λογιστικής Παλινδρόμησης
mylogit <- glm(admit ~gre + gpa + as.factor(rank), family = binomial(link="logit"), na.action=na.pass)
summary (mylogit)

# Έλεγχος Σημαντικότητας του Ολικού Μοντέλου
mylogit$null.deviance - mylogit$deviance
mylogit$df.null - mylogit$df.residual  # Βαθμοί ελευθερίας
# P-value
1-pchisq(mylogit$null.deviance - mylogit$deviance, mylogit$df.null - mylogit$df.residual)

# Υπολογισμός Odds Ratio και 95% διαστημάτων εμπιστοσύνης
confint(mylogit)
exp(mylogit$coefficients)
exp(confint(mylogit))

# Έλεγχος Ολικής Επίδρασης της μεταβλητής rank με τον έλεγχο του Wald 
# και το likelihood ratio test

install.packages("aod")
library(aod)
wald.test(b=coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
mylogit1 <- glm(admit ~ as.factor(rank) + gre + gpa, family = binomial (link = "logit"), na.action = na.pass)
anova(mylogit1, test = "Chisq")

# Έλεγχος για την διαφορά συντελεστών rank=2 και rank=3
l <- cbind(0,0,0,1,-1,0)
wald.test(b=coef(mylogit), Sigma = vcov(mylogit), L=l)

# Υπολογισμός Προβλεπόμενων Πιθανοτήτων
rank <- c(1,2,3,4)
gre <- c(mean(mydata$gre))
gpa <- c(mean(mydata$gpa))
newdata1 <- data.frame(gre, gpa, rank)
newdata1

newdata1$rankP <- predict(mylogit, newdata = newdata1, type="response")
newdata1
newdata2 <- data.frame(gre=seq(200, 800, 100), gpa=mean(mydata$gpa), rank=2)
newdata2$greP <- predict (mylogit, newdata = newdata2, type="response")
newdata2
