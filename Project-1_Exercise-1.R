# ASKHSH 1 / EXERCISE 1 - Project No 1

cardio = matrix(
  c(104,10933,189,10845), # το διάνυσμα των δεδομένων
  nrow=2, # ο αριθμός των γραμμών
  ncol=2, # ο αριθμός των στηλών (προαιρετικά)
  byrow = TRUE) # ξεκινά να συμπληρώνει τον πίνακα ανά γραμμή
dimnames(cardio) = list(
  Group=c("Aspirin","Placebo"), # ονόματα γραμμών
  Myocardial_Infraction=c("Yes","No")) # ονόματα στηλών

cardio # εμφάνιση του πίνακα

prop.test(cardio) # έλεγχος για την ισότητα δυο ποσοστών με διόρθωση συνέχειας
prop.test(cardio,correct=F) # έλεγχος για την ισότητα δυο ποσοστών χωρίς διόρθωση συνέχειας

cardio.test = prop.test(cardio) # ονομάζουμε τον έλεγχο χ2
cardio.test$estimate[1]/ cardio.test$estimate[2] # υπολογισμός του σχετικού κινδύνου

odds <- cardio.test$estimate/(1- cardio.test$estimate) # υπολογισμός των odds
theta <- odds[1]/odds[2] # υπολογισμός του odds ratio
theta # εμφάνιση του odds ratio


ASE <- sqrt(sum(1/cardio)) # υπολογισμός τυπικού σφάλματος του logθ
logtheta.CI <- log(theta) + c(-1,1)*1.96*ASE # υπολογισμός 95% δ.ε. για το logθ
logtheta.CI # εμφάνιση του 95% δ.ε. για το logθ
exp(logtheta.CI) # υπολογισμός 95% δ.ε. για το θ

