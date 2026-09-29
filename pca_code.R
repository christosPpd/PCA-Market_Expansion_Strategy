# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCA for Market Expansion Strategy
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Dependencies              ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages(c("eurostat", "tidyverse", "janitor"))

library(eurostat)
library(tidyverse)
library(janitor)

# Choose a recent year
year <- 2018

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Eurostat Urban Indicators ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Available datasets
search_results <- search_eurostat("city")
head(search_results)

# Population
df_pop <- get_eurostat("urb_cpop1", time_format = "num")

# Unemployment
df_unemp <- get_eurostat("urb_clma", time_format = "num")

# Education
df_educ <- get_eurostat("urb_ceduc", time_format = "num")

# Tourism (proxy for activity / attractiveness)
df_tourism <- get_eurostat("urb_ctour", time_format = "num")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Data Wrangling
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_pop <- df_pop %>% 
  filter(TIME_PERIOD == year & indic_ur == "DE1001V") %>%
  select(indic_ur, cities, values)

df_unemp <- df_unemp %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

df_educ <- df_educ %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

df_tourism <- df_tourism %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

# Merge the dataset
df <- rbind(df_pop, df_unemp, df_educ, df_tourism)

# Update the labels
df <- label_eurostat(df)

# Remove indicators that are available for the male-female population
df <- df %>%
  filter(!str_detect(indic_ur, regex("male|female", ignore_case = TRUE)))

# Bring to tidy format
df <- pivot_wider(df,
                  names_from = indic_ur,
                  values_from = values)

# Keep only the variables with low missing values
df <- df[, colMeans(is.na(df)) < 0.30]

# Keep cities with no missing values
df <- df[rowMeans(is.na(df)) == 0, ]

#-------------------------------------------------
#ΕΡΓΑΣΙΑ
#-------------------------------------------------
df <- as.data.frame(df)
df <- df[-c(180, 261),] #Αφέραιση των χωρών Γαλλία και Ρουμανία, καθώς αλλιώς θα έχουμε 
                        #πολυ ακραίες τιμές, καθώς τις συγκρίνουμε με πόλεις. Επιπλέον, 
                        #η εργασία ζητάει πόλεις.

#-----TASK1-----

#Υπολογισμός ελάχιστων, μέγιστων και μέσων τιμών των τυχαίων μεταβλητών, καθώς και των 
#τυπικών αποκλίσεων.
max1 <- min1 <- mean1 <- sd1 <- numeric(27)

for (i in 2 : 28){
  max1[i-1] <- max(df[, i])
  min1[i-1] <- min(df[, i])
  mean1[i-1] <- mean(df[, i])
  sd1[i-1]   <- sd(df[, i])
}

#Εισαγωγή των παραπάνω δεδομένων σε data frame
 MMM_SD <- data.frame(Μεταβλητές = colnames(df)[2:28], Μέγιστα = max1, Ελάχιστα = min1, Μέσες_τιμές = mean1, Τυπική_Απόκλιση = sd1)
 MMM_SD[, -1] <- round(MMM_SD[, -1], 2)

 MMM_SD

cat("Με βάση τον παραπάνω πίνακα, πολύ μεγάλη μεταβλητότητα εμφανίζουν κυρίως 
οι μεταβλητές που σχετίζονται με:
  
      1)Το μέγεθος της πόλης και τον πληθυσμό της, όπως:
    Population on the 1st of January, total
    Economically active population, 20–64, total
    Persons employed, 20–64, total
    Total employment/jobs (work place based)
    Persons aged 25-64 with ISCED level 5, 6, 7 or 8 as the highest level of education, from 2014 onwards
      
      2)Τους τομείς απασχόλησης, όπως:
    Employment in trade, transport, hotels, restaurants
    Employment in professional, scientific and technical activities
    Employment in public administration, defence, education, health and social work
      
      3)Τον τουρισμό, όπως:
    Total nights spent in tourist accommodation establishments
    Number of bed-places in tourist accommodation establishments
    
    
    Σε όλες αυτές τις περιπτώσεις έχουμε μεγάλη διαφορά μεταξύ μέγιστων και ελάχιστων 
    τιμών με τις μέσες τιμές, ενώ παρατηρούνται και μεγάλες τιμές των τυπικών αποκλίσεων. 
    Το γεγονός αυτό μας δείχνει ότι πόλεις πολύ μεγάλες, οδηγούν στην εμφάνιση ακραίων τιμών 
    σε αρκετές από τις μεταβλητές.")

cat("Πιο περιορισμένη διασπορά εμφανίζουν οι μεταβλητές που είναι ποσοστά ή αναλογίες, όπως:
    Activity rate, Unemployment rate, 
    Proportion of working age population qualified at level 3 or 4 ISCED, 
    Proportion of population aged 25–64 qualified at level 5 to 8 ISCED
    
    
    Εδώ έχω διαφορετική κλίμακα μέτρησης, καθώς έχω ποσοστά και αναλογίες, ενώ πριν είχα 
    μεταβλητές απόλυτων μεγεθών. Έτσι, οι διαφορές μεταξύ μέγιστων και ελάχιστων τιμών με 
    τις μέσες τιμές είναι μικρές, όπως και οι τιμές των τυπικών αποκλίσεων.")


#Plots, με βάση τα οποία προκύπτει αν υπάρχει κάποια γραμμική συσχέτιση μεταξύ μεταβλητών

# -------------------------
#SCATTERPLOTS

#---
cat("Εδώ έχω 4 plots, τα οποία δείχνουν ότι η μεταβλητή που αφορά τον πληθυσμό κάθε πόλης,
    παρουσιάζει ισχυρή θετική γραμμική συσχέτιση με διάφορες μεταβλητές, όπως είναι οι 
    Συνολικές θέσεις εργασίας, ο οικονομικά ενεργός πληθυσμός και το πλήθος των εργαζομένων 
    στον οικονομικό και στον δημόσιο τομέα.")

par(mfrow = c(2,2))
# Σχέση μεταξύ πληθυσμού και θέσεων εργασίας
plot(x = df$"Population on the 1st of January, total", 
     y = df$"Total employment/jobs (work place based)",
     xlab = "Population", ylab = "Employment",
     main = "Population and Employment")

abline(lm(df$"Total employment/jobs (work place based)" ~ 
            df$"Population on the 1st of January, total"),
       col = "red", lwd = 0.2)


# Σχέση μεταξύ πληθυσμού και οικονομικά ενεργού πληθυσμού
plot(x = df$"Population on the 1st of January, total",
     y = df$"Economically active population, 20-64, total",
     xlab = "Population", ylab = "Active population",
     main = "Population and Active population")

abline(lm(df$"Economically active population, 20-64, total" ~ 
            df$"Population on the 1st of January, total"),
       col = "red", lwd = 0.2)


# Σχέση μεταξύ πληθυσμού και απασχόλησης στον χρηματοπιστωτικό τομέα
plot(x = df$"Population on the 1st of January, total",
     y = df$"Employment (jobs) in financial and insurance activities (NACE Rev. 2, K)",
     xlab = "Population", ylab = "Financial jobs",
     main = "Population and Financial employment")

abline(lm(df$"Employment (jobs) in financial and insurance activities (NACE Rev. 2, K)" ~ 
            df$"Population on the 1st of January, total"),
       col = "red", lwd = 0.2)


# Σχέση μεταξύ πληθυσμού και απασχόλησης στον δημόσιο τομέα
plot(x = df$"Population on the 1st of January, total",
     y = df$"Employment (jobs) in public administration, defence, education, human health and social work activities (NACE Rev. 2, O to Q)",
     xlab = "Population", ylab = "Public sector jobs",
     main = "Population and Public sector employment")

abline(lm(df$"Employment (jobs) in public administration, defence, education, human health and social work activities (NACE Rev. 2, O to Q)" ~ 
            df$"Population on the 1st of January, total"),
       col = "red", lwd = 0.2)

par(mfrow = c(1,1))

#--
cat("Εδώ έχω 4 plots, τα οποία δείχνουν ότι υπάρχουν μεταβλητές που αφορούν τον 
χώρο της εργασίας και της απασχόλησης και παρουσιάζουν ισχυρή γραμμική συσχέτιση 
μεταξύ τους. Ιδιαίτερα η μεταβλητή που αφορά τη συνολική απασχόληση, σχετίζεται 
γραμμικά και ισχυρά με αρκετές μεταβλητές του συγκεκριμένου τομέα. Παράδειγμα 
αποτελούν η σχέση μεταξύ οικονομικά ενεργού πληθυσμού και απασχολούμενων και η σχέση
της συνολικής απασχόλησης με μεταβλητές, όπως οι απασχολούμενοι 20-64 ετών, οι 
απασχολούμενοι στον τομέα υπηρεσιών και οι απασχολούμενοι στον κατασκευαστικό τομέα.")  

par(mfrow = c(2,2))
# Σχέση μεταξύ συνολικής απασχόλησης και απασχολούμενων (20-64)
plot(x = df$"Total employment/jobs (work place based)",
     y = df$"Persons employed, 20-64, total",
     xlab = "Total employment", ylab = "Employed (20-64)",
     main = "Total employment and Employed (20-64)")

abline(lm(df$"Persons employed, 20-64, total" ~ 
            df$"Total employment/jobs (work place based)"),
       col = "red", lwd = 0.2)

# Σχέση μεταξύ οικονομικά ενεργού πληθυσμού και απασχολούμενων
plot(x = df$"Economically active population, 20-64, total",
     y = df$"Persons employed, 20-64, total",
     xlab = "Active population", ylab = "Employed",
     main = "Active population and Employed")

abline(lm(df$"Persons employed, 20-64, total" ~ 
            df$"Economically active population, 20-64, total"),
       col = "red", lwd = 0.2)

# Σχέση μεταξύ συνολικής απασχόλησης και απασχόλησης στον τομέα υπηρεσιών
plot(x = df$"Total employment/jobs (work place based)",
     y = df$"Employment (jobs) in professional, scientific and technical activities; administrative and support service activities (NACE Rev. 2, M and N)",
     xlab = "Total employment",
     ylab = "Services employment",
     main = "Total employment and Services employment")

abline(lm(df$"Employment (jobs) in professional, scientific and technical activities; administrative and support service activities (NACE Rev. 2, M and N)" ~ 
            df$"Total employment/jobs (work place based)"),
       col = "red", lwd = 0.2)

# Σχέση μεταξύ συνολικής απασχόλησης και απασχόλησης στον κατασκευαστικό τομέα
plot(x = df$"Total employment/jobs (work place based)",
     y = df$"Employment (jobs) in construction (NACE Rev. 2, F)",
     xlab = "Total employment", ylab = "Construction employment",
     main = "Total employment and Construction employment")

abline(lm(df$"Employment (jobs) in construction (NACE Rev. 2, F)" ~ 
            df$"Total employment/jobs (work place based)"), 
       col = "red", lwd = 0.2)

par(mfrow = c(1,1))

#---
cat("Εδώ έχω 2 plots, τα οποία δείχνουν ότι υπάρχουν μεταβλητές που αφορούν τον 
χώρο του τουρισμού και παρουσιάζουν γραμμική συσχέτιση τόσο μεταξύ τους όσο και 
με άλλους τομείς. Παράδειγμα αποτελούν η ισχυρά γραμμική σχέση μεταξύ αριθμού 
διανυκτερεύσεων και αριθμού κλινών τουριστικών εγκαταστάσεων και η 
σχέση μεταξύ των θέσεων εργασίας σε τέχνη, διασκέδαση και δραστηριότητες, 
με τις τουριστικές διανυκτερεύσεις.")  

par(mfrow = c(1,2))

# Σχέση μεταξύ αριθμού διανυκτερεύσεων και αριθμού κλινών τουριστικών εγκαταστάσεων
plot(x = df$"Total nights spent in tourist accommodation establishments",
     y = df$"Number of bed-places in tourist accommodation establishments",
     xlab = "Tourism nights", ylab = "Beds",
     main = "Tourism nights and Bed capacity")

abline(lm(df$"Number of bed-places in tourist accommodation establishments" ~ 
            df$"Total nights spent in tourist accommodation establishments"),
       col = "red", lwd = 2)


# Σχέση μεταξύ των θέσεων εργασίας σε τέχνη, διασκέδαση και δραστηριότητες, με τις τουριστικές διανυκτερεύσεις
plot(x = df$"Employment (jobs) in arts, entertainment and recreation; other service activities; activities of household and extra-territorial organizations and bodies (NACE Rev. 2, R to T)",
     y = df$"Total nights spent in tourist accommodation establishments",
     xlab = "Arts & entertainment jobs", ylab = "Tourism nights",
     main = "Arts employment and Tourism nights")

abline(lm(df$"Total nights spent in tourist accommodation establishments" ~ 
            df$"Employment (jobs) in arts, entertainment and recreation; other service activities; activities of household and extra-territorial organizations and bodies (NACE Rev. 2, R to T)"),
       col = "red", lwd = 2)

par(mfrow = c(1,1))

#---
cat("Εδώ έχουμε 2 plots, στα οποία παρατηρούμε ότι υπάρχουν μεταβλητές οι οποίες, 
σε κάθε περίπτωση, δεν παρουσιάζουν κάποια ισχυρή ή σαφή συσχέτιση.")

par(mfrow = c(1,2))
# Σχέση μεταξύ ποσοστού ανεργίας και αριθμού δημόσιων βιβλιοθηκών
plot(x = df$"Unemployment rate",
     y = df$"Number of public libraries (all distribution points)",
     xlab = "Unemployment rate", ylab = "Number of public libraries",
     main = "Unemployment rate and Public libraries")

# Σχέση μεταξύ ποσοστού πληθυσμού με εκπαίδευση ISCED 5-8 και τουριστικών διανυκτερεύσεων
plot(x = df$"Proportion of population aged 25-64 qualified at level 5 to 8 ISCED, from 2014 onwards",
     y = df$"Total nights spent in tourist accommodation establishments",
     xlab = "Proportion ISCED 5-8", ylab = "Tourism nights",
     main = "Higher education and Tourism nights")

par(mfrow = c(1,1))


#Σχόλιο για τα scatterplots:
cat("Από τα παραπάνω scatterplots φαίνεται ότι αρκετές μεταβλητές παρουσιάζουν έντονη 
γραμμική σχέση μεταξύ τους. Ο πληθυσμός συνδέεται πολύ ισχυρά με τη συνολική απασχόληση, 
τον οικονομικά ενεργό πληθυσμό και γενικά με βασικούς δείκτες της αγοράς εργασίας, κάτι 
που δείχνει ότι πολλές από αυτές τις μεταβλητές εκφράζουν ουσιαστικά το μέγεθος της πόλης. 
Το ίδιο παρατηρείται και στις μεταβλητές της απασχόλησης, αφού η συνολική απασχόληση, 
οι απασχολούμενοι και οι επιμέρους κλάδοι απασχόλησης εμφανίζουν ισχυρή μεταξύ τους συσχέτιση, 
ενώ και η σχέση μεταξύ των θέσεων εργασίας σε τέχνη, διασκέδαση και δραστηριότητες, με τις 
τουριστικές διανυκτερεύσεις φαίνεται να είναι θετική, με κάποια διασπορά. Παρόμοια εικόνα 
εμφανίζεται και στον τουρισμό, όπου οι διανυκτερεύσεις και οι κλίνες έχουν πολύ ισχυρή σχέση. 
Επομένως, τα plots δείχνουν ότι στο dataset υπάρχουν ομάδες μεταβλητών που αποτυπώνουν παρόμοιες 
διαστάσεις της αγοράς και συνεπώς, περιέχουν σε σημαντικό βαθμό πλεονάζουσα πληροφορία, γεγονός 
που ενθαρύνει την εφαρμογή της PCA.")


# -------------------------
#HISTOGRAMS

par(mfrow = c(3,3))
# Πληθυσμός
hist((df$"Population on the 1st of January, total"),
     main = "Distribution of Population",
     xlab = "Population")

# Συνολικές τουριστικές διανυκτερεύσεις
hist(df$"Total nights spent in tourist accommodation establishments",
     main = "Distribution of Tourism nights",
     xlab = "Tourism nights")

# Συνολικές θέσεις εργασίας
hist(df$"Total employment/jobs (work place based)",
     main = "Distribution of Total employment",
     xlab = "Total employment")

# Απασχόληση σε επαγγελματικές, επιστημονικές και υποστηρικτικές υπηρεσίες
hist(df$"Employment (jobs) in professional, scientific and technical activities; administrative and support service activities (NACE Rev. 2, M and N)",
     main = "Distribution of Professional and Support Services Employment",
     xlab = "Professional and support services employment")

# Ποσοστό Ανεργίας 
hist(df$"Unemployment rate",
     main = "Distribution of Unemployment rate",
     xlab = "Unemployment rate")

# Ποσοστό φοιτητών
hist(df$"Share of students in higher education in the total population (per 1000 persons)",
     main = "Distribution of Student Share",
     xlab = "Students per 1000 residents")

# Υψηλή εκπαίδευση (ISCED 5-8)
hist(df$"Proportion of population aged 25-64 qualified at level 5 to 8 ISCED, from 2014 onwards",
     main = "Distribution of Higher Education Levels",
     xlab = "Proportion ISCED 5-8")

# Activity rate
hist(df$"Activity rate",
     main = "Distribution of Activity Rate",
     xlab = "Activity rate")

# Βιβλιοθήκες
hist(df$"Number of public libraries (all distribution points)",
     main = "Distribution of Public Libraries",
     xlab = "Number of libraries")

cat("Τα ιστογράμματα δείχνουν ότι οι μεταβλητές δεν ακολουθούν όλες την ίδια μορφή κατανομής 
και παρουσιάζουν σημαντικές διαφορές ως προς τη διασπορά τους.
 
Οι μεταβλητές που σχετίζονται με το μέγεθος της πόλης και της οικονομικής δραστηριότητας, όπως ο πληθυσμός, η συνολική απασχόληση,
η απασχόληση στις υπηρεσίες και οι τουριστικές διανυκτερεύσεις, εμφανίζουν έντονη δεξιά ασυμμετρία. Ανάλογη εικόνα παρουσιάζουν και 
μεταβλητές όπως ο αριθμός δημόσιων βιβλιοθηκών και το ποσοστό φοιτητών.
Έτσι, στις περισσότερες πόλεις οι τιμές αυτών των μεταβλητών είναι σχετικά χαμηλές, ενώ υπάρχουν λίγες πόλεις με πολύ υψηλές τιμές, 
γεγονός που δείχνει μεγάλη ετερογένεια μεταξύ των πόλεων και πιθανή ύπαρξη ακραίων παρατηρήσεων. 
Το ποσοστό ανεργίας διαφοροποιείται επίσης μεταξύ των πόλεων, χωρίς όμως να εμφανίζει τόσο ακραία κατανομή όσο οι παραπάνω μεταβλητές.

Αντίθετα, μεταβλητές όπως το activity rate και το ποσοστό πληθυσμού με εκπαίδευση επιπέδου ISCED 5-8 εμφανίζουν πιο συμμετρική και ομαλή κατανομή, 
κάτι που δείχνει ότι οι πόλεις είναι περισσότερο συγκρίσιμες ως προς αυτούς τους σχετικούς δείκτες. 


Συνολικά, τα ιστογράμματα επιβεβαιώνουν ότι υπάρχουν μεταβλητές με πολύ διαφορετικά εύρη τιμών και διαφορετικό βαθμό μεταβλητότητας. 
Αυτό ενισχύει την ανάγκη χρήσης τυποποιημένων δεδομένων στην PCA, ώστε οι μεταβλητές με μεγάλες κλίμακες ή ακραίες τιμές να μην κυριαρχήσουν στην ανάλυση.")


# -------------------------
#BOXPLOTS

par(mfrow = c(3,3))

# Πληθυσμός
boxplot(df$"Population on the 1st of January, total",
        main = "Population", ylab = "Population")

# Τουριστικές διανυκτερεύσεις
boxplot(df$"Total nights spent in tourist accommodation establishments",
        main = "Tourism nights", ylab = "Tourism nights")

# Συνολική απασχόληση
boxplot(df$"Total employment/jobs (work place based)",
        main = "Total employment", ylab = "Jobs")

# Απασχόληση σε επαγγελματικές, επιστημονικές και υποστηρικτικές υπηρεσίες
boxplot(df$"Employment (jobs) in professional, scientific and technical activities; administrative and support service activities (NACE Rev. 2, M and N)",
        main = "Professional and support services employment", ylab = "Jobs")

# Ποσοστό Ανεργίας
boxplot(df$"Unemployment rate",
        main = "Unemployment rate", ylab = "Rate")

# Ποσοστό φοιτητών
boxplot(df$"Share of students in higher education in the total population (per 1000 persons)",
        main = "Student share", ylab = "Students per 1000")

# Υψηλή εκπαίδευση (ISCED 5-8)
boxplot(df$"Proportion of population aged 25-64 qualified at level 5 to 8 ISCED, from 2014 onwards",
        main = "Higher education", ylab = "Proportion")

# Activity rate
boxplot(df$"Activity rate",
        main = "Activity rate", ylab = "Rate")

# Βιβλιοθήκες
boxplot(df$"Number of public libraries (all distribution points)",
        main = "Public libraries", ylab = "Count")

par(mfrow = c(1,1))


cat("Τα boxplots επιβεβαιώνουν την εικόνα που προέκυψε από τα ιστογράμματα, 
αλλά παράλληλα δείχνουν πιο καθαρά την ύπαρξη ακραίων παρατηρήσεων και το εύρος 
στο οποίο συγκεντρώνεται το μεγαλύτερο μέρος των τιμών κάθε μεταβλητής. 
Ιδιαίτερα στις μεταβλητές που σχετίζονται με το μέγεθος της πόλης, την απασχόληση 
και τον τουρισμό, όπως ο πληθυσμός, οι τουριστικές διανυκτερεύσεις, η συνολική 
απασχόληση και η απασχόληση στις υπηρεσίες, εμφανίζονται πολλές ακραίες τιμές 
πάνω από το άνω άκρο του boxplot. Αυτό δείχνει ότι λίγες πόλεις έχουν πολύ υψηλές
τιμές σε αυτούς τους δείκτες, ενώ οι περισσότερες πόλεις συγκεντρώνονται σε αρκετά 
χαμηλότερα επίπεδα. 

Αντίθετα, μεταβλητές όπως το ποσοστό ανεργίας, το ποσοστό υψηλής εκπαίδευσης 
και το ποσοστό οικονομικής δραστηριότητας εμφανίζουν λιγότερο ακραία συμπεριφορά. 

Συνολικά, τα boxplots ενισχύουν το συμπέρασμα ότι οι μεταβλητές του data set 
έχουν πολύ διαφορετικό βαθμό μεταβλητότητας και ότι αρκετές από αυτές περιλαμβάνουν 
ακραίες παρατηρήσεις, κάτι που δικαιολογεί τη χρήση τυποποιημένων δεδομένων πριν από 
την εφαρμογή της PCA.")


#---

#πίνακας R των συσχετίσεων
R <- round(cor (df[ , 2:28]), 4)
R

cat("Ο πίνακας συσχετίσεων ουσιαστικά επιβεβαιώνει ότι πολλές μεταβλητές, ιδιαίτερα 
όσες σχετίζονται με τον πληθυσμό και την απασχόληση, έχουν πολύ υψηλή θετική συσχέτιση 
και άρα περιέχουν πλεονάζουσα πληροφορία. 
    
Αντίθετα, ορισμένοι ποσοστιαίοι δείκτες, όπως το unemployment rate και το activity rate, 
εμφανίζουν ασθενέστερες σχέσεις.")


#φι
p <- ncol(R) # αριθμός στηλών του R πινακα συσχετισεων
sqrt((sum(R^2) - p)/(p*(p-1)))

cat("Η ποσότητα φ, δίνει ένα συνολικό μέτρο της γραμμικής εξάρτησης μεταξύ των μεταβλητών. 
Η τιμή φ = 0.71 είναι αρκετά υψηλή (> 0.4) και δείχνει ότι υπάρχουν ισχυρές συσχετίσεις 
μεταξύ πολλών μεταβλητών. Το αποτέλεσμα αυτό επιβεβαιώνει τα συμπεράσματα που προέκυψαν 
από τα scatterplots και τον πίνακα συσχετίσεων, ότι αρκετές μεταβλητές περιέχουν 
πλεονάζουσα πληροφορία. Επομένως, η χρήση της PCA κρίνεται κατάλληλη, καθώς μπορεί να
συνοψίσει αυτή τη συσχετισμένη πληροφορία σε μικρότερο αριθμό διαστάσεων.")

#---
#Τελικό Συμπέρασμα
cat("Λαμβάνοντας υπόψιν όλα τα παραπάνω, προκύπτει ότι το συγκεκριμένο data set 
περιλαμβάνει μεταβλητές με πολύ διαφορετικές κλίμακες, διαφορετικό βαθμό μεταβλητότητας 
και αρκετές ακραίες παρατηρήσεις. Επιπλέον, πολλές από τις μεταβλητές εμφανίζουν πολύ 
ισχυρές θετικές συσχετίσεις, γεγονός που δείχνει την ύπαρξη πλεονάζουσας πληροφορίας. 
Αυτό καθιστά την PCA κατάλληλη μέθοδο για τη μείωση της διάστασης του προβλήματος. 
Επειδή όμως οι μεταβλητές δεν είναι άμεσα συγκρίσιμες ως προς την κλίμακα μέτρησής τους, 
η PCA είναι προτιμότερο να εφαρμοστεί σε τυποποιημένα δεδομένα και μέσω του πίνακα 
συσχετίσεων R, αντί του πίνακα συνδιακυμάνσεων S, ώστε να μην κυριαρχήσουν στην ανάλυση 
οι μεταβλητές με τις μεγαλύτερες διασπορές.")



#-----Task2-----
df[,c("Persons unemployed, total", "Unemployment rate")] <- 
  -df[,c("Persons unemployed, total", "Unemployment rate")] #αυτό γίνεται ώστε να υπάρχει κοινή ερμηνεία, 
                                                            #ότι μεγάλη τιμή σημαίνει καλή τιμή

df2 <- df[, 1:28] #για το Task 7

S <-cov (df[ , 2:28]) #πίνακας S των συνδιακυμάνσεων (χρειάζεται στο TASK7)
eigS <- eigen(S) #υπολογισμός ιδιοτιμών και ιδιοδιανυσμάτων για τον S



R <- cor(df[ , 2:28]) #πίνακας R των συσχετίσεων
eigR <- eigen(R) #υπολογισμός ιδιοτιμών και ιδιοδιανυσμάτων για τον R

#Υπολογισμός συντελεστών των Κύριων Συνιστωσών
coefficients <- data.frame(eigR$vectors) #οι συντελεστές προκύπτουν από τα ιδιοδιανύσματα
colnames(coefficients) <- paste0("PC", 1:ncol(coefficients))
rownames(coefficients) <- colnames(df)[2:28]
cat("Coefficients (loadings) of all Principal Components:\n")
coefficients

cat("Ο πίνακας των loadings δείχνει πώς συμβάλλει κάθε μεταβλητή σε κάθε Κύρια Συνιστώσα. 
Παρατηρούμε ότι:

Στην PC1 οι περισσότερες μεταβλητές που σχετίζονται με το μέγεθος της πόλης, 
την απασχόληση και τη γενική οικονομική δραστηριότητα έχουν loadings πολύ κοντά μεταξύ 
τους και με ίδιο πρόσημο. Αυτό δείχνει ότι η PC1 εκφράζει κυρίως μια γενική διάσταση 
μεγέθους της πόλης και της αγοράς της. 

Η PC2 φαίνεται να ερμηνεύει κυρίως τις μεταβλητές που σχετίζονται με τον τουρισμό, 
καθώς οι δύο συντελεστές με τις μεγαλύτερες θετικές τιμές αφορούν και οι δύο τον 
κλάδο του τουρισμού.

Η PC3 φαίνεται να αποτυπώνει κυρίως το εκπαιδευτικό προφίλ του πληθυσμού των πόλεων. 
Αυτό προκύπτει επειδή στην PC3 συμμετέχουν πιο έντονα το ποσοστό του πληθυσμού ηλικίας 
25–64 με εκπαίδευση επιπέδου ISCED 5–8 και το ποσοστό φοιτητών στην ανώτατη εκπαίδευση, 
ενώ αντίθετα το ποσοστό του πληθυσμού με εκπαίδευση επιπέδου ISCED 3–4 συνδέεται με την 
αντίθετη κατεύθυνση της συνιστώσας. Επομένως, η PC3 εκφράζει περισσότερο το σχετικό 
επίπεδο υψηλής εκπαίδευσης.

Στις επόμενες συνιστώσες εμφανίζονται πιο ειδικά χαρακτηριστικά των πόλεων. 
Άρα, ο πίνακας των loadings δείχνει ότι οι αρχικές μεταβλητές οργανώνονται γύρω από λίγες 
βασικές διαστάσεις, με σημαντικότερες το γενικό μέγεθος της αγοράς και τον τουρισμό, ενώ 
οι υπόλοιπες συνιστώσες περιγράφουν πιο ειδικές πλευρές του dataset.")


#Υπολογισμός του ποσοστού της συνολικής διασποράς που εκφράζει κάθε Κύρια Συνιστώσα
PercTotVar <- data.frame(Percentage_of_total_variance_explained_by_each_component = 
                         eigR$values/sum(eigR$values))
rownames(PercTotVar) <- paste0("PC", 1:nrow(PercTotVar))
round(PercTotVar, 4)

cat("Ο πίνακας του ποσοστού της συνολικής διασποράς δείχνει ότι η PC1 εξηγεί το μεγαλύτερο 
μέρος της πληροφορίας του dataset, καθώς εκφράζει περίπου το 71.17% της συνολικής διασποράς. 

Οι PC2 και PC3 εξηγούν σαφώς μικρότερο μέρος της διασποράς, περίπου 8.41% και 6.97% 
αντίστοιχα. Παρ' όλα αυτά, συμβάλλουν και αυτές σημαντικά στην ανάλυση, αφού αποτυπώνουν 
επιμέρους διαστάσεις του dataset που δεν καλύπτονται πλήρως από την PC1. 

Συνολικά, οι τρεις πρώτες Κύριες Συνιστώσες εξηγούν περίπου το 86.55% της συνολικής διασποράς, 
ενώ μετά την τρίτη συνιστώσα το επιπλέον ποσοστό που προσθέτει κάθε νέα συνιστώσα γίνεται αρκετά μικρό. 
Επομένως, η πληροφορία του dataset συγκεντρώνεται κυρίως στις πρώτες συνιστώσες με τη δομή των δεδομένων 
να μπορεί να περιγραφεί ικανοποιητικά με μικρότερο αριθμό διαστάσεων από τις αρχικές 27 μεταβλητές.")


#scree plot
plot(1:length(eigR$values),eigR$values,type = "b",col = "lightblue",lwd = 2,
     xlab = "Component number",ylab = "Eigenvalue",pch = 19)

cat("Το scree plot δείχνει ότι υπάρχει πολύ μεγάλη πτώση της ιδιοτιμής από την PC1 προς την PC2, 
ενώ στη συνέχεια η μείωση γίνεται αισθητά πιο ήπια. Αυτό σημαίνει ότι η πρώτη Κύρια Συνιστώσα 
εξηγεί πολύ μεγαλύτερο μέρος της συνολικής διασποράς σε σχέση με τις επόμενες. 
Άρα, το διάγραμμα επιβεβαιώνει ότι η πληροφορία του dataset δεν κατανέμεται ομοιόμορφα σε όλες 
τις συνιστώσες, αλλά συγκεντρώνεται κυρίως στις πρώτες λίγες Κύριες Συνιστώσες.")

#---
#Τελικό Συμπέρασμα

cat("Συνολικά, η PCA δείχνει ότι οι μεταβλητές του dataset οργανώνονται γύρω από λίγες βασικές διαστάσεις. 
Η PC1 εκφράζει κυρίως το μέγεθος της πόλης και της αγοράς της, η PC2 συνδέεται κυρίως με τον τουρισμό, 
ενώ η PC3 αποτυπώνει περισσότερο το εκπαιδευτικό προφίλ του πληθυσμού των πόλεων. 
Παράλληλα, το ποσοστό της συνολικής διασποράς και το scree plot δείχνουν ότι το μεγαλύτερο μέρος της 
πληροφορίας του dataset συγκεντρώνεται στις πρώτες λίγες Κύριες Συνιστώσες.")


#-----Task3-----

#---
#Επιλογή αριθμού Κύριων Συνιστωσών

cat("Με βάση το παραπάνω Scree plot, χρειάζομαι 2 Κύριες Συνιστώσες")

#Με ποσοστό συνολικής διακύμανσης
eigTable <- data.frame(it = 1:27, EigValueR = eigR$values,
                       PercR = eigR$values/sum(eigR$values))
eigTable$ceigR <- cumsum(eigTable$PercR)
round(eigTable,3)

cat("Με βάση το ποσοστό συνολικής διακύμανσης, χρειάζομαι 2 έως 3 Κύριες Συνιστώσες, 
    για να 'χω ποσοστό κοντά στο 80%")

#Με Κριτήριο του Kaiser
sum(eigR$values)/length(eigR$values)
cat("Καθώς η παραπάνω ποσότητα ισούται με 1, από Κριτήριο Kaiser, 
    διαλέγουμε τις ιδιοτιμές που είναι μεγαλύτερες της μονάδας.")
head(eigTable)
cat("Προκύπτει ότι έχω μόνο 4 ιδιοτιμές πάνω από την μονάδα και επομένως από Κριτήριο Kaiser,
    θα χρειαστώ 4 Κύριες Συνιστώσες")

#Απόφαση
cat("Με βάση το scree plot, η πιο απότομη πτώση παρατηρείται από την PC1 προς την PC2,
ενώ μετά η μείωση των ιδιοτιμών γίνεται σαφώς πιο ήπια. Παράλληλα, οι δύο πρώτες 
Κύριες Συνιστώσες εξηγούν περίπου το 79.58% της συνολικής διασποράς, ποσοστό αρκετά υψηλό
για ικανοποιητική περιγραφή του dataset. Αν και το κριτήριο Kaiser οδηγεί στη διατήρηση 
τεσσάρων συνιστωσών, το κριτήριο αυτό συχνά τείνει να διατηρεί περισσότερες συνιστώσες 
από όσες είναι πρακτικά αναγκαίες. Για τον λόγο αυτό, η επιλογή μόνο δύο συνιστωσών κρίνεται 
προτιμότερη, επειδή συνδυάζει ικανοποιητικό ποσοστό εξηγούμενης διασποράς με σαφή και 
χρήσιμη ερμηνεία.")

#---
# Επιχειρηματική ερμηνεία των δύο πρώτων Κύριων Συνιστωσών
cat("Με βάση όσα έχουμε δει και στα προηγούμενα TASK, συμπεραίνουμε:

Η PC1 μπορεί να ερμηνευθεί επιχειρηματικά ως διάσταση γενικού μεγέθους της αγοράς. 
Αυτό συμβαίνει επειδή σε αυτήν συμμετέχουν έντονα μεταβλητές που σχετίζονται με τον πληθυσμό, 
τη συνολική απασχόληση, τον οικονομικά ενεργό πληθυσμό και γενικότερα τη συνολική οικονομική 
δραστηριότητα της πόλης. Επομένως, πόλεις με υψηλές τιμές στην PC1 τείνουν να αντιστοιχούν 
σε μεγαλύτερες και οικονομικά ισχυρότερες αγορές.

Η PC2 ερμηνεύεται ως διάσταση τουριστικής έντασης, αφού συνδέεται περισσότερο με μεταβλητές
που αφορούν τον τουρισμό, όπως οι διανυκτερεύσεις και οι τουριστικές κλίνες. Άρα, πόλεις με 
υψηλές τιμές στην PC2 τείνουν να έχουν ισχυρότερο τουριστικό προφίλ και μεγαλύτερη τουριστική 
δραστηριότητα σε σχέση με άλλες πόλεις.")

#---
#Υπολογισμός των πρώτων 2 Κύριων Συνιστωσών και των ranks

#Υπολογισμός των πρώτων 2 Κύριων Συνιστωσών
df$Comp1 <- NULL 
df$Comp2 <- NULL
df[,c("Comp1","Comp2")] <- scale(df[ , 2:28]) %*% eigR$vectors[, 1:2]
df$Comp1 <- -df$Comp1  #αλλαγή προσήμου για ευκολότερη ερμηνεία


#rank για την PC1
dd <-df
dd$first_order <- 1:nrow(df)
dd <- dd[order(-dd$Comp1),]
dd$rankComp1 <- 1:nrow(dd)
dd <- dd[order(dd$first_order),]

df$rankComp1 <- dd$rankComp1

#rank για την PC2
dd <- df
dd$first_order <- 1:nrow(df)
dd <- dd[order(-dd$Comp2),]
dd$rankComp2 <- 1:nrow(dd)
dd <- dd[order(dd$first_order),]
df$rankComp2 <- dd$rankComp2


# Πόλεις με τα υψηλότερα scores στην PC1
head(df[order(-df$Comp1), c("cities", "Comp1", "rankComp1")], 10)

# Πόλεις με τα χαμηλότερα scores στην PC1
head(df[order(df$Comp1), c("cities", "Comp1", "rankComp1")], 10)

# Πόλεις με τα υψηλότερα scores στην PC2
head(df[order(-df$Comp2), c("cities", "Comp2", "rankComp2")], 10)

# Πόλεις με τα χαμηλότερα scores στην PC2
head(df[order(df$Comp2), c("cities", "Comp2", "rankComp2")], 10)

cat("Από τις τιμές των δύο πρώτων Κύριων Συνιστωσών φαίνεται πιο καθαρά πώς 
τοποθετούνται οι πόλεις στις δύο βασικές διαστάσεις που ανέδειξε η PCA.

Για την PC1, οι υψηλότερες τιμές εμφανίζονται σε πόλεις όπως το Παρίσι, η Μαδρίτη, 
το Βερολίνο και το Αμβούργο, γεγονός που είναι συνεπές με την ερμηνεία της ως διάστασης 
γενικού μεγέθους της αγοράς. Αντίθετα, οι χαμηλότερες τιμές της PC1 εμφανίζονται 
σε πόλεις όπως το Görlitz, το Calais και το Stralsund, δηλαδή σε πόλεις που αντιστοιχούν 
σε μικρότερες αγορές.

Για την PC2, οι υψηλότερες τιμές εμφανίζονται σε πόλεις όπως το Benidorm, το Torremolinos 
και το Fréjus, οι οποίες έχουν έντονο τουριστικό προφίλ. Αντίθετα, οι χαμηλότερες τιμές 
εμφανίζονται σε πόλεις όπως το Siegen, το Zwickau και το Bocholt, οι οποίες φαίνεται να 
έχουν ασθενέστερο τουριστικό χαρακτήρα.

Συνολικά, οι τιμές αυτές επιβεβαιώνουν ότι η PC1 διαφοροποιεί κυρίως τις πόλεις ως προς 
το μέγεθος της αγοράς τους, ενώ η PC2 τις διαφοροποιεί κυρίως ως προς την τουριστική 
τους ένταση.")


#-----Task4-----
#προς διευκόλυνση, δίνεται αρίθμηση των μεταβλητών
metavlites <- colnames(df)
metavlites <- metavlites[2 : 28]
arithmisi_metavlitwn <- data.frame(Number = 1: length(metavlites), Μεταβλητές = metavlites)
arithmisi_metavlitwn


#Για την PC1
plot(eigR$vectors[,1], pch = 19, xaxt = "n", xlab = "Variable index", ylab = "Loading on PC1",
     main = "Loadings of variables on PC1")
axis(1, at = 1:27, labels = 1:27, cex.axis = 0.7)
abline(h = 0, lty = 2)


cat("Από το plot των loadings στην PC1 μπορούμε να εντοπίσουμε μια βασική ομάδα μεταβλητών 
που εκφράζουν κυρίως τη διάσταση της αγοράς που σχετίζεται με το μέγεθος κάθε πόλης 
και της αντίστοιχης αγοράς. Πιο συγκεκριμένα, ξεχωρίζουν οι μεταβλητές 1, 5-16, 18, 20 
και 22-25, οι οποίες εμφανίζουν αρκετά παρόμοιες τιμές loadings και με ίδιο πρόσημο. 
Οι περισσότερες από αυτές αφορούν το μέγεθος του πληθυσμού, το εργατικό δυναμικό, 
τη συνολική απασχόληση, επιμέρους τομείς οικονομικής δραστηριότητας, καθώς και ορισμένα 
μεγέθη που αφορούν την εκπαίδευση, τον τουρισμό και τις υποδομές. 
Άρα, η PC1 εκφράζει κυρίως μια γενική διάσταση μεγέθους της πόλης και της αγοράς της.

Η ομαδοποίηση εδώ είναι αρκετά καθαρή, επειδή πολλές μεταβλητές συγκεντρώνονται 
πολύ κοντά μεταξύ τους. Ωστόσο, η μεταβλητή 3 συνδέεται επίσης έντονα με την ίδια γενική 
διάσταση, αλλά εμφανίζει αντίθετο πρόσημο από τον βασικό πυρήνα της ομάδας. 
Άρα, παρότι η βασική ομαδοποίηση είναι σαφής, δεν είναι όλες οι μεταβλητές 
απόλυτα ομοιογενείς και ορισμένες δίνουν πιο μικτή πληροφορία.")


#Για την PC2
plot(eigR$vectors[, 2], pch = 19, xaxt = "n", xlab = "Variable index", ylab = "Loading on PC2",
     main = "Loadings of variables on PC2")
axis(1, at = 1:27, labels = 1:27, cex.axis = 0.7)
abline(h = 0, lty = 2)

cat("Από το plot των loadings στην PC2 προκύπτει μια αρκετά καθαρή ομάδα μεταβλητών 
που σχετίζονται με τον τουρισμό. Ιδιαίτερα ξεχωρίζουν οι μεταβλητές 24, 25, 26 και 27, 
δηλαδή οι δείκτες που αφορούν τις τουριστικές διανυκτερεύσεις, τις τουριστικές κλίνες, 
τις διαθέσιμες κλίνες ανά 1000 κατοίκους και τις διανυκτερεύσεις ανά κάτοικο. 
Ανάμεσα σε αυτές, οι 26 και 27 εμφανίζουν τις μεγαλύτερες θετικές τιμές, 
γεγονός που δείχνει ότι η PC2 εκφράζει κυρίως μια διάσταση της αγοράς που αφορά τον τουρισμό.

Η ομαδοποίηση εδώ είναι αρκετά καθαρή, επειδή οι τουριστικές μεταβλητές εμφανίζονται 
με σχετικά μεγαλύτερες θετικές τιμές loadings από τις περισσότερες υπόλοιπες μεταβλητές. 
Ωστόσο, δεν συμμετέχουν όλες με τον ίδιο τρόπο. Οι μεταβλητές 4 και 19 εμφανίζουν αρνητικά loadings, 
οπότε δίνουν μια πιο αντίθετη ή μικτή πληροφορία σε σχέση με τον βασικό τουριστικό πυρήνα. 
Συνολικά, η PC2 δίνει μια αρκετά σαφή ομαδοποίηση γύρω από τον τουρισμό, με λίγες μεταβλητές 
να λειτουργούν περισσότερο ως στοιχεία αντίθεσης παρά ως μέρος του βασικού πυρήνα της ομάδας.")


#Για την PC3
plot(eigR$vectors[,3], pch = 19, xaxt = "n", xlab = "Variable index", ylab = "Loading on PC3",
     main = "Loadings of variables on PC3")
axis(1, at = 1:27, labels = 1:27,las = 2, cex.axis = 0.7)
abline(h = 0, lty = 2)

cat("Από το plot των loadings στην PC3 φαίνεται ότι προκύπτει κυρίως μια διάσταση που σχετίζεται 
με το εκπαιδευτικό προφίλ των πόλεων. Πιο συγκεκριμένα, ξεχωρίζουν κυρίως οι μεταβλητές 17 και 21 
με θετικές τιμές των loadings, ενώ στην αντίθετη κατεύθυνση ξεχωρίζει κυρίως η 19 με έντονα 
αρνητική τιμή. Σε μικρότερο βαθμό συμμετέχει θετικά και η 18. Άρα, η PC3 εκφράζει κυρίως αντίθεση 
ανάμεσα σε δείκτες υψηλότερης εκπαίδευσης και φοιτητικής παρουσίας, από τη μία πλευρά, και σε 
δείκτες μεσαίου εκπαιδευτικού επιπέδου, από την άλλη.

Η ομαδοποίηση εδώ είναι αρκετά καθαρή για τις βασικές εκπαιδευτικές μεταβλητές, αφού αυτές 
ξεχωρίζουν πιο έντονα από τις υπόλοιπες. Ωστόσο, οι 4, 26 και 27 εμφανίζουν αρνητικά loadings, 
άρα δίνουν κάποια πιο μικτή πληροφορία σε σχέση με τον βασικό εκπαιδευτικό πυρήνα.")



par(mfrow = c(3,1))

#Για την PC4
plot(eigR$vectors[,4],
     pch = 19,
     xaxt = "n",
     xlab = "Variable index",
     ylab = "Loading on PC4",
     main = "Loadings of variables on PC4")
axis(1, at = 1:27, labels = 1:27, cex.axis = 0.7)
abline(h = 0, lty = 2)

#Για την PC5
plot(eigR$vectors[,5],
     pch = 19,
     xaxt = "n",
     xlab = "Variable index",
     ylab = "Loading on PC5",
     main = "Loadings of variables on PC5")
axis(1, at = 1:27, labels = 1:27, cex.axis = 0.7)
abline(h = 0, lty = 2)

#Για την PC6
plot(eigR$vectors[,6],
     pch = 19,
     xaxt = "n",
     xlab = "Variable index",
     ylab = "Loading on PC6",
     main = "Loadings of variables on PC6")

axis(1, at = 1:27, labels = 1:27, cex.axis = 0.7)
abline(h = 0, lty = 2)

par(mfrow = c(1,1))

cat("Οι PC4-PC6 φαίνεται να μην οδηγούν σε ιδιαίτερα καθαρό grouping 
των retail evaluation criteria. Γι' αυτό οι πιο σαφείς και χρήσιμες ομάδες 
μεταβλητών προκύπτουν κυρίως από τις PC1, PC2 και σε μικρότερο βαθμό από την PC3, 
ενώ οι επόμενες συνιστώσες αποτυπώνουν περισσότερο ειδικές ή μικτές πληροφορίες.")


#Συμπέρασμα
cat("Συνολικά, η PCA δείχνει ότι τα retail criteria ομαδοποιούνται κυρίως γύρω από 
τρεις βασικές διαστάσεις: το μέγεθος της αγοράς, τον τουρισμό και το εκπαιδευτικό προφίλ. 
Η ομαδοποίηση είναι γενικά αρκετά καθαρή, αν και ορισμένες μεταβλητές δίνουν πιο μικτή ή 
αντίθετη πληροφορία.")

#-----Task5-----

df$group <- NA

#Κάνω grouping με βάση τα πρόσημα των scores των PC1 και PC2
df$group[df$Comp1 >= 0 & df$Comp2 >= 0] <- "Positive on both PCs"
df$group[df$Comp1 >= 0 & df$Comp2 <  0] <- "PC1 positive and PC2 negative"
df$group[df$Comp1 <  0 & df$Comp2 >= 0] <- "PC1 negative and PC2 positive"
df$group[df$Comp1 <  0 & df$Comp2 <  0] <- "Negative on both PCs"

table(df$group) #πόσα στοιχεία έχει κάθε group

df$group <- factor(df$group)
plot(df$Comp1, df$Comp2,
     col = c("purple4", "deepskyblue3", "forestgreen", "darkorange2")[df$group],
     pch = 19,
     xlab = "PC1 score", ylab = "PC2 score",
     main = "Grouping of cities based on PC1 and PC2 scores")

abline(h = 0, v = 0, lty = 2)

#Για να τυπωθεί πάνω στο plot τα χρώματα και το group που αντιστοιχούν
legend("topright",
       legend = levels(df$group),
       col = c("purple4", "deepskyblue3", "forestgreen", "darkorange2"),
       pch = 19,
       cex = 0.8)

df[, c("cities", "Comp1", "Comp2", "group")]




cat("Με βάση τα scores των δύο πρώτων Κύριων Συνιστωσών, οι πόλεις μπορούν να περιγραφούν σε 
τέσσερις βασικές ομάδες, ανάλογα με το αν βρίσκονται στη θετική ή στην αρνητική πλευρά των 
αξόνων PC1 και PC2. Οι πόλεις με θετική PC1 και αρνητική PC2 συνδέονται περισσότερο με το 
μέγεθος της αγοράς παρά με τον τουρισμό, ενώ οι πόλεις με αρνητική PC1 και θετική PC2 
συνδέονται περισσότερο με την τουριστική διάσταση. Στην πρώτη ομάδα ανήκουν χαρακτηριστικά 
πόλεις όπως το Βερολίνο, το Αμβούργο και η Στουτγγάρδη, ενώ στη δεύτερη πόλεις όπως το Benidorm, 
το Torremolinos, η Marbella και οι Cannes-Antibes. Οι πόλεις με θετικές τιμές και στις δύο 
συνιστώσες, όπως το Παρίσι, το Μόναχο, η Málaga και η Palma de Mallorca, εμφανίζουν σχετικά 
ισχυρό προφίλ και στις δύο διαστάσεις. Αντίθετα, στην αρνητική πλευρά και των δύο συνιστωσών 
συγκεντρώνονται κυρίως πόλεις που δεν ξεχωρίζουν έντονα ούτε ως προς το μέγεθος της αγοράς 
ούτε ως προς τον τουρισμό.

Ως προς τη γεωγραφική διάσταση, δεν προκύπτει πλήρως καθαρός διαχωρισμός μεταξύ Βόρειας και 
Νότιας Ευρώπης, με κάθε group να έχει πόλεις από διαφορετικές ευρωπαϊκές χώρες του δείγματος, 
και έτσι δεν μπορεί να υποστηριχθεί ότι υπάρχει αυστηρός περιφερειακός διαχωρισμός.
Ωστόσο, αρκετές πόλεις του ευρωπαϊκού Νότου εμφανίζονται συχνότερα στη θετική πλευρά της 
PC2, δηλαδή στην πλευρά του πιο έντονου τουριστικού προφίλ, ενώ πολλές γερμανικές 
πόλεις εμφανίζονται συχνότερα στη θετική πλευρά της PC1 και στην αρνητική πλευρά της PC2.")

#-----Task6-----


# Κρατάμε τις δύο πρώτες κύριες συνιστώσες
# Βάρη με βάση το ποσοστό διασποράς που εξηγούν
w1 <- eigTable$PercR[1]
w2 <- eigTable$PercR[2]

# Διαμόρφωση Score
df$Ascore <- w1 * df$Comp1 + w2 * df$Comp2

cat("Με αυτόν τον τύπο για το Score έχω ως βάρη τα ποσοστά συνολικής διασποράς,
    που εξηγεί κάθε Κύρια Συνιστώσα. Η PC1 εξηγεί σημαντικά μεγαλύτερο ποσοστό διασποράς,
    σε σύγκριση με την PC2 και άρα επιθυμούμε να της δώσουμε με τον τρόπο αυτό μεγαλύτερο βάρος
    στην διαμόρφωση του τελικού Score. Πέρα από το ποσοστό της συνολικής διασποράς, άλλος ένας
    λόγος που μας κάνει να θέλουμε η PC1 να παίζει μεγαλύτερο ρόλο στην διαμόρφωση του Score,
    είναι το γεγονός ότι, όπως προκύπτει από τις παραπάνω αναλύσεις, η PC1 αποτελεί πιο πολύ
    μέτρο του μεγέθους μιας πόλης και της αγοράς της, ενώ η PC2 αφορά κυρίως τον τουρισμό. ")



# Ranking: μεγαλύτερο score = πιο ελκυστική πόλη
dd <- df
dd$first_order <- 1:nrow(df)

dd <- dd[order(-dd$Ascore), ]
dd$rankAscore <- 1:nrow(dd)

# Επιστροφή στην αρχική σειρά
dd <- dd[order(dd$first_order), ]

df$rankAscore <- dd$rankAscore



#Ποιες πόλεις είναι πρώτες και ποιες τελευταίες με βάση τον παραπάνω τύπο
head(df[order(df$rankAscore),
        c("cities", "Comp1", "Comp2", "Ascore", "rankAscore")], 10)
head(df[order(-df$rankAscore),
        c("cities", "Comp1", "Comp2", "Ascore", "rankAscore")], 10)

#Σχετικά με τα θετικά και τους περιορισμούς της PCA
cat("Η χρήση της PCA για ranking έχει ορισμένα σημαντικά πλεονεκτήματα. 
Αρχικά, επιτρέπει τη συμπύκνωση της πληροφορίας πολλών αρχικών μεταβλητών σε λίγες βασικές 
διαστάσεις, κάτι ιδιαίτερα χρήσιμο όταν πολλές από τις μεταβλητές είναι έντονα συσχετισμένες 
και περιέχουν πλεονάζουσα πληροφορία. Επιπλέον, το τελικό ranking δεν προκύπτει από αυθαίρετη 
επιλογή βαρών, αλλά στηρίζεται στα ίδια τα δεδομένα, καθώς αυτά βασίζονται στο ποσοστό διασποράς 
που εξηγεί κάθε Κύρια Συνιστώσα. 

Ωστόσο, υπάρχουν και περιορισμοί. Η PCA κατασκευάζεται με στόχο να εξηγεί όσο το δυνατόν 
μεγαλύτερο μέρος της συνολικής διασποράς. Έτσι, στη συγκεκριμένη εφαρμογή, το τελικό 
ranking επηρεάζεται σε πολύ μεγάλο βαθμό από την PC1, η οποία εκφράζει κυρίως το μέγεθος 
της πόλης και της αγοράς της, ενώ η PC2 παίζει μικρότερο ρόλο. Αυτό σημαίνει ότι η κατάταξη 
τείνει να δίνει μεγαλύτερη έμφαση στο μέγεθος της αγοράς και λιγότερη σε άλλες πλευρές, 
όπως ο τουρισμός. Επιπλέον, το τελικό αποτέλεσμα εξαρτάται από τον αριθμό των συνιστωσών 
που επιλέγονται, από τα πρόσημα των συντελεστών κάθε Κύριας Συνιστώσας και από τον κανόνα 
με τον οποίο συνδυάζονται οι συνιστώσες σε έναν ενιαίο δείκτη.")


#-----Task7-----

#---
#Υπολογισμός συντελεστών των Κύριων Συνιστωσών
coefficients2 <- data.frame(eigS$vectors) #οι συντελεστές προκύπτουν από τα ιδιοδιανύσματα
colnames(coefficients2) <- paste0("PC", 1:27)
rownames(coefficients2) <- colnames(df2)[2:28]

cat("Coefficients (loadings) of all Principal Components when using centred-only data:\n")
coefficients2

#υπενθύμιση
cat("Coefficients (loadings) of all Principal Components when using standardised data:\n")
coefficients


#Περιγραφική αναπαράσταση των loadings για PC1 και PC2, όταν έχω centred-only data 
arithmisi_metavlitwn
par(mfrow = c(1,2))

#για την PC1
plot(eigS$vectors[,1],
     pch = 19,
     xaxt = "n",
     xlab = "Variable index",
     ylab = "Loading on PC1",
     main = "Loadings of variables on PC1")
axis(1, at = 1:27, labels = 1:27, cex.axis = 0.7)
abline(h = 0, lty = 2)

#για την PC2
plot(eigS$vectors[,2],
     pch = 19,
     xaxt = "n",
     xlab = "Variable index",
     ylab = "Loading on PC2",
     main = "Loadings of variables on PC2")
axis(1, at = 1:27, labels = 1:27, cex.axis = 0.7)
abline(h = 0, lty = 2)

par(mfrow = c(1,1))

#Σχολιασμός και ερμηνεία:
cat("Με standardised data, τα loadings κατανέμονται πιο ισορροπημένα και οι πρώτες συνιστώσες
αποκτούν πιο καθαρή ερμηνεία: η PC1 σχετίζεται κυρίως με το μέγεθος της αγοράς, η PC2 με τον τουρισμό 
και η PC3 με το εκπαιδευτικό προφίλ. Αντίθετα, με centred-only data, η PC1 δεν σχηματίζει καθαρή ομάδα
μεταβλητών, έχοντας μια πολύ μεγάλη τιμή στον συντελεστή που αφορά τις συνολικές τουριστικές 
διανυκτερεύσεις και στην συνέχεια, με σαφώς μικρότερες τιμές συντελεστών, να ακολουθούν ο πληθυσμός και
δείκτες που αφορούν την απασχόληση. Παράλληλα, η PC2 φαίνεται να αποτυπώνει μια διάσταση μεγέθους αγοράς 
και απασχόλησης. Επομένως, με standardised data αποτυπώνονται πιο καθαρά οι διαφορετικές διαστάσεις της
αγοράς, ενώ με centred-only data το αποτέλεσμα φαίνεται να επηρεάζεται από μεταβλητές με μεγάλη διασπορά.

Άρα, η ερμηνεία των πρώτων Κύριων Συνιστωσών αλλάζει αισθητά όταν περνάμε από standardised data σε centred-only
data, καθώς στην πρώτη περίπτωση οι Συνιστώσες αποτυπώνουν πιο καθαρά διαφορετικές διαστάσεις της αγοράς,
ενώ στην δεύτερη περίπτωση η ερμηνεία τους επηρεάζεται έντονα από μεταβλητές πολύ μεγάλης διασποράς.")


#---
#ποσοστό συνολικής διακύμανσης
eigTable2 <- data.frame(it = 1:27, EigValueS = eigS$values, PercS = eigS$values/sum(eigS$values))
eigTable2$ceigS <- cumsum(eigTable2$PercS)
round(eigTable2,3)

#Scree plot
plot(1:length(eigS$values),eigS$values,type = "b",col = "lightblue",lwd = 2,
     xlab = "Component number",ylab = "Eigenvalue",pch = 19)

#υπενθύμιση των αντίστοιχων ποσοστών όταν έχω standardised data:
round(eigTable,3)

#Σχολιασμός:
cat("Με standardised data, η PC1 εξηγούσε το μεγαλύτερο ποσοστό της διασποράς, ενώ οι υπόλοιπες 
    Κύριες Συνιστώσες είχαν μια σχετικά ομαλή πτώση στο ποσοστό αυτό, με το 100% να εξηγείται για
    πρώτη φορά με την PC17. Αντίθετα, με centred-only data και καθώς κυριαρχούν οι μεταβλητές με 
    μεγάλη διασπορά, η εικόνα αυτή αλλάζει, με την PC1 να φτάνει από μόνη της να εξηγεί το 99.7%
    της διασποράς και το 100% τελικά να επιτυγχάνεται μόλις στην PC2.")

#---
#Υπολογισμός των πρώτων 2 Κύριων Συνιστοσών και των scores
df2$Comp2.1 <- NULL 
df2$Comp2.2 <- NULL
df2[,c("Comp2.1","Comp2.2")] <- scale(df2[ , 2:28], center = TRUE, scale = FALSE) %*% (-eigS$vectors[ , 1:2])
df2$Comp2.2 <- -df2$Comp2.2
df2


dd <-df2
dd$first_order <- 1:nrow(df2)
dd <- dd[order(-dd$Comp2.1),]
dd$rankComp2.1 <- 1:nrow(dd)
dd <- dd[order(dd$first_order),]

df2$rankComp2.1 <- dd$rankComp2.1

#
dd <- df2
dd$first_order <- 1:nrow(df2)
dd <- dd[order(-dd$Comp2.2),]
dd$rankComp2.2 <- 1:nrow(dd)
dd <- dd[order(dd$first_order),]
df2$rankComp2.2 <- dd$rankComp2.2


#Εξετάζω τα groups
df2$group2 <- NA

#Κάνω grouping με βάση τα πρόσημα των scores των PC1 και PC2
df2$group2[df2$Comp2.1 >= 0 & df2$Comp2.2 >= 0] <- "Positive on both PCs"
df2$group2[df2$Comp2.1 >= 0 & df2$Comp2.2 <  0] <- "PC1 positive and PC2 negative"
df2$group2[df2$Comp2.1 <  0 & df2$Comp2.2 >= 0] <- "PC1 negative and PC2 positive"
df2$group2[df2$Comp2.1 <  0 & df2$Comp2.2 <  0] <- "Negative on both PCs"

table(df2$group2) #πόσα στοιχεία έχει κάθε group

df2$group2 <- factor(df2$group2)
plot(df2$Comp2.1, df2$Comp2.2,
     col = c("purple4", "deepskyblue3", "forestgreen", "darkorange2")[df2$group2],
     pch = 19,
     xlab = "PC1 score", ylab = "PC2 score",
     main = "Grouping of cities based on PC1 and PC2 scores")

abline(h = 0, v = 0, lty = 2)

#Για να τυπωθεί πάνω στο plot τα χρώματα και το group που αντιστοιχούν
legend("topright",
       legend = levels(df2$group2),
       col = c("purple4", "deepskyblue3", "forestgreen", "darkorange2"),
       pch = 19,
       cex = 0.8)

df2[, c("cities", "Comp2.1", "Comp2.2", "group2")]

#Συμπεράσματα
cat("Παρατηρείται ότι η ομαδοποίηση των πόλεων αλλάζει αισθητά όταν χρησιμοποιούνται 
centred-only data. Με standardised data, οι τέσσερις ομάδες συνδέονταν πιο καθαρά με 
τις δύο βασικές διαστάσεις της PCA, δηλαδή το μέγεθος της αγοράς και τον τουρισμό. 
Αντίθετα, με centred-only data η εικόνα γίνεται λιγότερο ισορροπημένη, καθώς η
ομαδοποίηση επηρεάζεται περισσότερο από μεταβλητές μεγάλης διασποράς και μεγάλου
απόλυτου μεγέθους. Έτσι, εδώ στις ομάδες με θετική PC1 εμφανίζονται κυρίως πολύ μεγάλες
πόλεις, όπως το Παρίσι, το Βερολίνο, το Μόναχο το Αμβούργο, η Στουτγγάρδη και η Λυόν,
αλλά και ορισμένες πόλεις με πολύ ισχυρά απόλυτα τουριστικά μεγέθη, όπως το Benidorm,
η Palma de Mallorca, το Torremolinos, η Marbella, οι Cannes-Antibes. Αντίθετα, πολλές
πόλεις μεσαίου ή μικρότερου μεγέθους συγκεντρώνονται κυρίως στις ομάδες με αρνητική PC1.

Άρα, σε σχέση με την PCA με standardised data, το grouping με centred-only data δίνει 
μεγαλύτερη έμφαση στα απόλυτα μεγέθη της αγοράς και του τουρισμού και λιγότερο σε μια 
πιο ισορροπημένη σύγκριση όλων των μεταβλητών. Επομένως, και ως προς την ομαδοποίηση 
των πόλεων, το standardisation είναι προτιμότερο στη συγκεκριμένη εφαρμογή.")


#---
# Κρατάμε τις δύο πρώτες κύριες συνιστώσες
# Βάρη με βάση το ποσοστό διασποράς που εξηγούν
w2.1 <- eigTable2$PercS[1]
w2.2 <- eigTable2$PercS[2]

# Διαμόρφωση Score
df2$Ascore2 <- w2.1 * df2$Comp2.1 + w2.2 * df2$Comp2.2


# Ranking: μεγαλύτερο score = πιο ελκυστική πόλη
dd <- df2
dd$first_order <- 1:nrow(df2)

dd <- dd[order(-dd$Ascore2), ]
dd$rankAscore2 <- 1:nrow(dd)

# Επιστροφή στην αρχική σειρά
dd <- dd[order(dd$first_order), ]

df2$rankAscore2 <- dd$rankAscore2



compare_ranks <- data.frame(Cities = df[ , 1], 
                            Score_with_standardised_data = df$Ascore, 
                            Rank_with_standardised_data = df$rankAscore, 
                            Score_with_centred_only_data = df2$Ascore2,
                            Rank_with_centred_only_data = df2$rankAscore2)

#Σχολιασμός:
cat("Μεταξύ των Rankings με και χωρίς standardised data παρατηρούνται μεγάλες διαφορές. 
Πιο συγκεκριμένα, αν και παρατηρείται ότι πολύ μεγάλες πόλεις, όπως το Παρίσι, το Βερολίνο,
η Μαδρίτη και το Μόναχο, παραμένουν ψηλά και στις δύο περιπτώσεις, όταν χρησιμοποιούνται 
centred-only data, ανεβαίνουν σημαντικά πόλεις με πολύ έντονο τουριστικό προφίλ, όπως το 
Benidorm, η Palma de Mallorca, το Torremolinos, η Nice, κλπ. Η μεταβολή αυτή είναι αναμενόμενη, 
καθώς με centred-only data η PC1 εξηγεί σχεδόν όλη τη συνολική διασπορά, με αποτέλεσμα η διαμόρφωση 
του τελικού ranking να καθορίζεται κυρίως από αυτή. Όμως, όπως έχει διαπιστωθεί, η συγκεκριμένη PC1 
επηρεάζεται έντονα από τις συνολικές τουριστικές διανυκτερεύσεις και έτσι είναι λογικό 
να ανεβαίνουν στην κατάταξη πόλεις με πολύ έντονο τουριστικό προφίλ. Επομένως, το ranking με 
centred-only data επηρεάζεται πολύ περισσότερο από μεταβλητές μεγάλης διασποράς και μεγάλου απόλυτου 
μεγέθους, ιδίως από τουριστικές μεταβλητές.

Αντίθετα, το ranking με standardised data είναι πιο ισορροπημένο, καθώς λαμβάνει πιο δίκαια υπόψη όλες 
τις μεταβλητές. Για τον λόγο αυτό, στη συγκεκριμένη εφαρμογή το standardisation είναι προτιμότερο.")




