# Project 1 - Exercise 3, ΑΣΚΗΣΗ 3

malf = matrix(
  c(17066,48,14464,38,788,5,126,1,37,1), # το διάνυσμα των δεδομένων
  nrow=5, # ο αριθμός των γραμμών
  ncol=2, # ο αριθμός των στηλών (προαιρετικά)
  byrow = TRUE) # ξεκινά να συμπληρώνει τον πίνακα ανά γραμμή
dimnames(malf) = list(
  Alcohol=c("0","<1","1-2","3-5",">=6"), # ονόματα γραμμών
  Malformation =c("Absent","Present")) # ονόματα στηλών

malf # εμφάνιση του πίνακα
malf.yes <- malf[,"Present"] # συχνότητες αυτών που έχουν δυσπλασία
malf.yes
malf.total <- margin.table(malf,1) # περιθώρια αθροίσματα γραμμών
malf.total

prop.trend.test(malf.yes,malf.total) # έλεγχος χ2 για γραμμική τάση με raw scores
prop.trend.test(malf.yes,malf.total,c(0,0.5,1.5,4,7)) # έλεγχος χ2 για γραμμική τάση με midpoints
prop.trend.test(malf.yes,malf.total,c(8557.5,24365.5,32013,32473,32555.5)) # έλεγχος χ2 για γραμμική τάση με midranks

b=c(malf.total[1], malf.total[1]+ malf.total[2], malf.total[1]+ malf.total[2]+ malf.total[3], malf.total[1]+ malf.total[2]+ malf.total[3]+ malf.total[4], malf.total[1]+ malf.total[2]+ malf.total[3]+ malf.total[4]+ malf.total[5])

midranks=c((1+b[1])/2,(b[1]+1+b[2])/2,(b[2]+1+b[3])/2,(b[3]+1+b[4])/2,(b[4]+1+b[5])/2)
midranks