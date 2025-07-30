# Project 1 - Exercise 2 / ΑΣΚΗΣΗ 2

party = matrix(
  c(762,327,468,484,239,477), # το διάνυσμα των δεδομένων
  nrow=2, # ο αριθμός των γραμμών
  ncol=3, # ο αριθμός των στηλών (προαιρετικά)
  byrow = TRUE) # ξεκινά να συμπληρώνει τον πίνακα ανά γραμμή
dimnames(party) = list(
  Gender=c("Females","Males"), # ονόματα γραμμών
  Party_identification=c("Democrat","Independent","Republican")) # ονόματα στηλών

party # εμφάνιση του πίνακα

rowtot=margin.table(party, 1) # περιθώρια αθροίσματα γραμμών
rowtot # εμφάνιση των περιθώριων αθροισμάτων γραμμών

coltot=margin.table(party, 2) # περιθώρια αθροίσματα στηλών
coltot # εμφάνιση των περιθώριων αθροισμάτων στηλών

tot=sum(party) # άθροισμα στοιχείων
tot # εμφάνιση του αθροίσματος των στοιχείων

prop.table(party) # σχετικές συχνότητες κελιών
prop.table(party, 1) # σχετικές συχνότητες ανά γραμμή
prop.table(party, 2) # σχετικές συχνότητες ανά στήλη

chitest = chisq.test(party) # έλεγχος χ2
chitest # εμφάνιση αποτελεσμάτων ελέγχου χ2
chitest$expected # αναμενόμενες συχνότητες
chitest$expected - chitest$observed # διαφορά μεταξύ παρατηρούμενων και αναμενόμενων τιμών
chitest$residuals # Std. Residuals
(chitest$observed-chitest$expected)/sqrt(chitest$expected*(1-matrix(rowtot)/tot) %*% t(1-matrix(coltot)/tot)) # Adjusted Residuals είναι τα standardized residuals που υπολογίζει ο Agresti

G2=2 * sum(chitest$observed * log(chitest$observed/chitest$expected)) # Υπολογισμός του G2
G2 # Εμφάνιση του G2

1-pchisq(2 * sum(chitest$observed * log(chitest$observed/chitest$expected)),df=2) # Υπολογισμός του p-value του likelihood ratio test