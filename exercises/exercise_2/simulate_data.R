# ---
# Τελική Προσομοίωση: Ολοκληρωμένο Ερευνητικό Δείγμα Νηπιαγωγείου (v3.0)
# Χαρακτηριστικά: Κεφαλαιοποιημένα Ελληνικά, Συγκεκριμένα Labels, Bounding Ηλικίας
# ---

set.seed(1)
save_location <- "data"
if (!dir.exists(save_location)) dir.create(save_location)
sample_size <- 450

# 1. ΠΑΡΑΓΩΓΗ ΠΡΟΓΝΩΣΤΙΚΩΝ ΜΕΤΑΒΛΗΤΩΝ (Συνεχείς)
means <- c(age = 60, peabody = 100, htks = 40)
sds   <- c(age = 7, peabody = 15, htks = 12)
cor_matrix <- matrix(c(
  1.0,  0.45, 0.5, 
  0.45, 1.0,  0.4, 
  0.5,  0.4,  1.0
), nrow = 3, byrow = TRUE)

sigma <- diag(sds) %*% cor_matrix %*% diag(sds)
cont_data <- MASS::mvrnorm(n = sample_size, mu = means, Sigma = sigma) |> round()

df <- data.frame(
  age = as.numeric(cont_data[, "age"]),
  peabody = as.numeric(cont_data[, "peabody"]),
  htks = as.numeric(cont_data[, "htks"])
)

# Επιβολή ορίων (Bounding)
df$age <- pmax(48, pmin(72, df$age)) # Αυστηρά 48-72 μήνες
df$htks <- pmax(0, pmin(60, df$htks))
df$peabody <- pmax(20, df$peabody)

# 2. ΠΑΡΑΓΩΓΗ ΚΑΤΗΓΟΡΙΚΩΝ ΜΕΤΑΒΛΗΤΩΝ (Factors)
# Φύλο: 0: ΑΡΡΕΝ, 1: ΘΗΛΥ
df$sex <- sample(0:1, sample_size, replace = TRUE) |>
  factor(levels = 0:1, labels = c("ΑΡΡΕΝ", "ΘΗΛΥ"))

# Κοινωνικοοικονομικό Επίπεδο (SES)
df$family_ses <- sample(0:2, sample_size, replace = TRUE, prob = c(.4, .4, .2)) |>
  factor(
    levels = 0:2,
    labels = c("ΕΡΓΑΤΙΚΗ", "ΜΕΣΑΙΑ", "ΔΙΕΥΘΥΝΤΙΚΗ_ΕΠΙΣΤΗΜΟΝΙΚΗ")
  )

# Τύπος Σχολείου (Σύνδεση με SES)
df$school_type <- sapply(
  as.character(df$family_ses),
  function(ses) {
    if (ses == "ΕΡΓΑΤΙΚΗ") {
      sample(c("ΔΗΜΟΣΙΟ", "ΙΔΙΩΤΙΚΟ", "ΠΕΙΡΑΜΑΤΙΚΟ"), 1, prob = c(.85, .05, .10))
    } else if (ses == "ΜΕΣΑΙΑ") {
      sample(c("ΔΗΜΟΣΙΟ", "ΙΔΙΩΤΙΚΟ", "ΠΕΙΡΑΜΑΤΙΚΟ"), 1, prob = c(.65, .20, .15))
    } else {
      sample(c("ΔΗΜΟΣΙΟ", "ΙΔΙΩΤΙΚΟ", "ΠΕΙΡΑΜΑΤΙΚΟ"), 1, prob = c(.20, .60, .20))
    }
  }
) |> factor(levels = c("ΔΗΜΟΣΙΟ", "ΙΔΙΩΤΙΚΟ", "ΠΕΙΡΑΜΑΤΙΚΟ"))

# Διγλωσσία
df$bilingual <- sample(0:1, sample_size, replace = TRUE, prob = c(0.8, 0.2)) |>
  factor(levels = 0:1, labels = c("ΟΧΙ", "ΝΑΙ"))

# ---------------------------------------------------------
# 3. BINARY OUTCOME: ΠΑΡΑΠΟΜΠΗ ΚΕΔΑΣΥ (Binary Logistic)
# ---------------------------------------------------------
X_bin <- stats::model.matrix(
  ~ age + peabody + htks + family_ses + school_type + sex + bilingual,
  data = df
)

# Συντελεστές (Βάσει των νέων κεφαλαιοποιημένων ονομάτων)
beta_ref <- c(
  "(Intercept)" = 8.5, 
  "age" = 0.01, 
  "peabody" = -0.08, 
  "htks" = -0.12, 
  "family_sesΜΕΣΑΙΑ" = -0.3, 
  "family_sesΔΙΕΥΘΥΝΤΙΚΗ_ΕΠΙΣΤΗΜΟΝΙΚΗ" = -0.7, 
  "school_typeΙΔΙΩΤΙΚΟ" = -0.8, 
  "school_typeΠΕΙΡΑΜΑΤΙΚΟ" = -0.5, 
  "sexΘΗΛΥ" = -0.8, 
  "bilingualΝΑΙ" = 1.8
)

z_bin <- X_bin %*% beta_ref
prob_bin <- 1 / (1 + exp(-z_bin))
# Δημιουργία factor 0:ΟΧΙ, 1:ΝΑΙ
df$kedasy_referral <- rbinom(n = sample_size, size = 1, prob = prob_bin) |>
  factor(levels = 0:1, labels = c("ΟΧΙ", "ΝΑΙ"))

# ---------------------------------------------------------
# 4. NOMINAL OUTCOME: ΣΤΡΑΤΗΓΙΚΗ ΣΥΓΚΡΟΥΣΗΣ (Multinomial)
# ---------------------------------------------------------
X_multi <- stats::model.matrix(
  ~ age + peabody + htks + family_ses + school_type + sex,
  data = df
)

b_with <- c(
  "(Intercept)" = 6.0,
  "age" = -.04,
  "peabody" = -.01,
  "htks" = -.08,
  "family_sesΜΕΣΑΙΑ" = -.1,
  "family_sesΔΙΕΥΘΥΝΤΙΚΗ_ΕΠΙΣΤΗΜΟΝΙΚΗ" = -.3,
  "school_typeΙΔΙΩΤΙΚΟ" = -.2,
  "school_typeΠΕΙΡΑΜΑΤΙΚΟ" = -.4,
  "sexΘΗΛΥ" = 0.5
)
b_aggr <- c(
  "(Intercept)" = 11.5,
  "age" = -.08,
  "peabody" = -.02,
  "htks" = -.18,
  "family_sesΜΕΣΑΙΑ" = -.3,
  "family_sesΔΙΕΥΘΥΝΤΙΚΗ_ΕΠΙΣΤΗΜΟΝΙΚΗ" = -.7,
  "school_typeΙΔΙΩΤΙΚΟ" = -.5,
  "school_typeΠΕΙΡΑΜΑΤΙΚΟ" = -1.0,
  "sexΘΗΛΥ" = -1.1
)

betas_multi <- cbind(0, b_with, b_aggr)
probs_multi <- exp(X_multi %*% betas_multi) / rowSums(exp(X_multi %*% betas_multi))

conflict_labels <- c("ΔΙΑΠΡΑΓΜΑΤΕΥΣΗ", "ΑΠΟΣΥΡΣΗ", "ΕΠΙΘΕΤΙΚΟΤΗΤΑ")
df$conflict_strategy <- apply(
  probs_multi,
  1,
  function(p) sample(conflict_labels, 1, prob = p)
) |>
  factor(levels = conflict_labels)

# ---------------------------------------------------------
# 5. ORDINAL OUTCOME: ΣΧΟΛΙΚΗ ΕΤΟΙΜΟΤΗΤΑ (Ordinal)
# ---------------------------------------------------------
beta_ord <- c(
  "age" = .06,
  "peabody" = .02,
  "htks" = .10,
  "family_sesΜΕΣΑΙΑ" = .4,
  "family_sesΔΙΕΥΘΥΝΤΙΚΗ_ΕΠΙΣΤΗΜΟΝΙΚΗ" = .9,
  "school_typeΙΔΙΩΤΙΚΟ" = .6,
  "school_typeΠΕΙΡΑΜΑΤΙΚΟ" = 1.2,
  "sexΘΗΛΥ" = .3
)

X_ord <- stats::model.matrix(
  ~ age + peabody + htks + family_ses + school_type + sex,
  data = df
)[, -1]

latent <- X_ord %*% beta_ord + stats::rlogis(sample_size)
df$school_readiness <- factor(
  dplyr::case_when(
    latent < 6.5 ~ "ΧΑΜΗΛΗ",
    latent < 9.5 ~ "ΜΕΤΡΙΑ",
    TRUE ~ "ΥΨΗΛΗ"
  ),
  levels = c("ΧΑΜΗΛΗ", "ΜΕΤΡΙΑ", "ΥΨΗΛΗ"), ordered = TRUE
)

# ---------------------------------------------------------
# 6. ΑΠΟΘΗΚΕΥΣΗ ΚΑΙ ΕΠΑΛΗΘΕΥΣΗ
# ---------------------------------------------------------
readr::write_csv(df, file.path(save_location, "exercise_2_data.csv"))


message("Η προσομοίωση ολοκληρώθηκε. Έλεγχος δεδομένων: \n")
print(summary(df))
