# Set working directory (local path for running the scripts)
setwd("/Users/ji/Library/CloudStorage/OneDrive-QueenMary,UniversityofLondon/Dissertation")

library(dplyr)

# --------------------------------------------------
# 1. Build panel dataset across waves
# --------------------------------------------------
# Loads Innovation Panel files, harmonises key variables, and stacks them
# into a long-format panel suitable for transition modelling.
build_set <- function(path, waves = letters[1:17]) {
  panel_list <- lapply(seq_along(waves), function(i) {
    w <- waves[i]
    file <- file.path(path, paste0(w, "_indresp_ip.tab"))
    data <- read.delim(file, header = TRUE, sep = "\t")
    
    # Wave-specific variable names
    rel_status <- paste0(w, "_mastat_dv")
    income     <- paste0(w, "_prearna")
    age_cat    <- paste0(w, "_agegr5_dv")
    kids       <- paste0(w, "_nchild_dv")
    
    # Harmonise life satisfaction variable (naming varies across waves)
    if (w %in% c("b", "c")) {
      new_var <- paste0(w, "_sclfsato")
      h1 <- paste0(w, "_lfsato_h1")
      h2 <- paste0(w, "_lfsato_h2")
      data[[new_var]] <- ifelse(!is.na(data[[h1]]), data[[h1]], data[[h2]])
      life_sat <- new_var
      
    } else if (w == "f") {
      new_var <- "f_sclfsato"
      data[[new_var]] <- dplyr::coalesce(data$f_lfsato_c,
                                         data$f_lfsato_a,
                                         data$f_lfsato_b)
      life_sat <- new_var
      
    } else if (w %in% c("g", "h", "j", "k")) {
      new_var <- paste0(w, "_sclfsato")
      a <- paste0(w, "_sclfsato_a")
      b <- paste0(w, "_sclfsato_b")
      data[[new_var]] <- dplyr::coalesce(data[[a]], data[[b]])
      life_sat <- new_var
      
    } else {
      life_sat <- paste0(w, "_sclfsato")
    }
    
    # Check required variables exist
    needed <- c("pidp", rel_status, life_sat, income, age_cat, kids)
    missing <- setdiff(needed, names(data))
    if (length(missing) > 0) stop("Wave ", w, " missing: ", paste(missing, collapse=", "))
    
    # Keep and rename variables
    data <- data[, c("pidp", rel_status, life_sat, income, age_cat, kids)]
    names(data) <- c("pidp", "mastat", "sclfsato", "income", "age", "kids")
    
    # Clean satisfaction values
    data$sclfsato[data$sclfsato < 0] <- NA
    
    # Add wave index
    data$wave <- i
    data
  })
  
  do.call(rbind, panel_list)
}

panel_data <- build_set("tab")

# --------------------------------------------------
# 2. Create satisfaction-fall indicator
# --------------------------------------------------
# sat_fall = 1 when satisfaction drops by ≥1 point between waves.
panel_data <- panel_data %>%
  arrange(pidp, wave) %>%
  group_by(pidp) %>%
  mutate(
    lag_sat  = lag(sclfsato),
    sat_drop = lag_sat - sclfsato,
    sat_fall = ifelse(sat_drop >= 1, 1, 0)
  ) %>%
  ungroup()

# --------------------------------------------------
# 3. Define relationship transitions
# --------------------------------------------------
# Identifies whether a respondent moves from a union to a non-union state
# between waves (transition = 1), or remains in a union (transition = 0).
panel_data <- panel_data %>%
  arrange(pidp, wave) %>%
  group_by(pidp) %>%
  mutate(
    in_union_now = mastat %in% c(2, 3, 10),
    post_breakup_now = mastat %in% c(1, 4, 5, 7, 8),
    
    lag_mastat = lag(mastat),
    in_union_lag = lag_mastat %in% c(2, 3, 10),
    
    transition = case_when(
      lag_mastat %in% c(2, 3) & mastat %in% c(4, 5, 7, 8) ~ 1,
      lag_mastat == 10 & mastat == 1 ~ 1,
      in_union_lag & in_union_now ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

panel_transitions <- panel_data %>% filter(!is.na(transition))

# --------------------------------------------------
# 4. Logistic regression model
# --------------------------------------------------
# Estimates the association between satisfaction falls and dissolution risk.
model_satfall <- glm(
  transition ~ sat_fall + income + age,
  data = panel_transitions,
  family = binomial
)

summary(model_satfall)
exp(coef(model_satfall))  # odds ratios

# --------------------------------------------------
# 5. Key quantities for separations-avoided modelling
# --------------------------------------------------

# Baseline probability of a satisfaction fall
p_fall <- mean(panel_transitions$sat_fall == 1, na.rm = TRUE)

# Dissolution probabilities conditional on sat_fall
p_diss_fall <- mean(panel_transitions$transition[panel_transitions$sat_fall == 1] == 1, na.rm = TRUE)
p_diss_nofall <- mean(panel_transitions$transition[panel_transitions$sat_fall == 0] == 1, na.rm = TRUE)

# Summary table
prob_table <- data.frame(
  sat_fall_status = c("Fall = 1", "Fall = 0"),
  dissolution_probability = c(p_diss_fall, p_diss_nofall)
)

# Extract coefficients for predicted probabilities
coefs <- coef(model_satfall)

# Helper functions
logit_inv <- function(x) exp(x) / (1 + exp(x))

predict_dissolution <- function(sat_fall_value, income_value, age_value) {
  xb <- coefs[1] +
    coefs["sat_fall"] * sat_fall_value +
    coefs["income"] * income_value +
    coefs["age"] * age_value
  logit_inv(xb)
}

# Example predictions
predict_dissolution(1, mean(panel_transitions$income, na.rm = TRUE),
                    mean(panel_transitions$age, na.rm = TRUE))

predict_dissolution(0, mean(panel_transitions$income, na.rm = TRUE),
                    mean(panel_transitions$age, na.rm = TRUE))

# --------------------------------------------------
# 6. Apply therapy-induced ΔS = 3.09 and recompute fall probability
# --------------------------------------------------
DeltaS <- 3.09

panel_sim <- panel_transitions %>%
  mutate(
    sclfsato_T = sclfsato + DeltaS,
    delta_sat_T = sclfsato_T - lag_sat,
    sat_fall_T = ifelse(delta_sat_T <= -1, 1, 0)
  )

p_fall_baseline <- mean(panel_transitions$sat_fall == 1, na.rm = TRUE)
p_fall_T <- mean(panel_sim$sat_fall_T == 1, na.rm = TRUE)

# Proportional reduction in falls
reduction_pct <- 1 - (p_fall_T / p_fall_baseline)

# --------------------------------------------------
# 7. Heterogeneity tests
# --------------------------------------------------

# 7.1 Parents vs non-parents
panel_transitions <- panel_transitions %>%
  mutate(
    kids_clean = ifelse(kids < 0, NA, kids),
    parent = ifelse(kids_clean > 0, 1, 0)
  )

model_parents <- glm(
  transition ~ sat_fall + income + age,
  data = panel_transitions %>% filter(parent == 1),
  family = binomial
)

model_nonparents <- glm(
  transition ~ sat_fall + income + age,
  data = panel_transitions %>% filter(parent == 0),
  family = binomial
)

OR_parents    <- exp(coef(model_parents)["sat_fall"])
OR_nonparents <- exp(coef(model_nonparents)["sat_fall"])

# 7.2 Relationship type
panel_transitions <- panel_transitions %>%
  mutate(
    rel_group = case_when(
      lag_mastat == 2  ~ "Married",
      lag_mastat == 3  ~ "Civil",
      lag_mastat == 10 ~ "Cohabiting",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(rel_group))

model_married <- glm(transition ~ sat_fall + income + age,
                     data = panel_transitions %>% filter(rel_group == "Married"),
                     family = binomial)

model_civil <- glm(transition ~ sat_fall + income + age,
                   data = panel_transitions %>% filter(rel_group == "Civil"),
                   family = binomial)

model_cohab <- glm(transition ~ sat_fall + income + age,
                   data = panel_transitions %>% filter(rel_group == "Cohabiting"),
                   family = binomial)

OR_married <- exp(coef(model_married)["sat_fall"])
OR_civil   <- exp(coef(model_civil)["sat_fall"])
OR_cohab   <- exp(coef(model_cohab)["sat_fall"])

# 7.3 Combine heterogeneity results
heterogeneity_results <- data.frame(
  group = c("Parents", "Non‑parents", "Married", "Civil", "Cohabiting"),
  sat_fall_OR = c(OR_parents, OR_nonparents, OR_married, OR_civil, OR_cohab)
)

heterogeneity_results
