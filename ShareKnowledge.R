##########################
# Share knowledge Chi-Sq #
##########################

# Yes and Sometimes responses are combined to avoid expansion and fishers exact needs.

# TST-study vs Non-study controls
Test <- matrix(c(33,9,44,41), nrow = 2, byrow = TRUE)

rownames(Test) <- c("A", "B")
colnames(Test) <- c("Yes_Sometimes", "No")
print("A vs B")
print(Test)
chisq.test(Test)

# Plant-TST vs TST
Test <- matrix(c(18, 4, 15, 5), nrow = 2, byrow = TRUE)

rownames(Test) <- c("A", "B")
colnames(Test) <- c("Yes_Sometimes", "No")
print(Test)
chisq.test(Test)

# Plant-TST vs Control-PlantTST
Test <- matrix(c(18, 4, 11, 10), nrow = 2, byrow = TRUE)

rownames(Test) <- c("A", "B")
colnames(Test) <- c("Yes_Sometimes", "No")
print(Test)
chisq.test(Test)

# TST vs Control-TST
Test <- matrix(c(15,5,7,12), nrow = 2, byrow = TRUE)

rownames(Test) <- c("A", "B")
colnames(Test) <- c("Yes_Sometimes", "No")
print(Test)
chisq.test(Test)

# Control-PlantTST & Control-TST vs Control-Distant
Test <- matrix(c(18,22,26,19), nrow = 2, byrow = TRUE)

rownames(Test) <- c("A", "B")
colnames(Test) <- c("Yes_Sometimes", "No")
print(Test)
chisq.test(Test)

######################
# Learn about worms?##
######################

# Control-PlantTST & Control-TST vs Control-Distant
Worms <- matrix(c(41,1,72,15), nrow = 2, byrow = TRUE)

rownames(Worms) <- c("Participate_Yes", "Participate_No")
colnames(Worms) <- c("Learn_Yes", "Learn_No")
print(Worms)
fisher.test(Worms)
chisq.test(Worms)
