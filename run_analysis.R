# Title: Analysis for the International Sex Survey project (ASRS)
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2023-02-21
# Copyright (c) 2023 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Import packages
library(tidyverse)
library(lavaan)
library(semTools)
library(tidySEM)
library(patchwork)

# Define grouping variables to use
grouping_variables <- c(
  "language",
  "country",
  "gender"
)

# Define demographic variables to use
demographic_variables <- c(
  "country",
  "language",
  "sex_birth",
  "gender",
  "Age",
  "sexual_orientation",
  "education",
  "study",
  "work",
  "socioeconomic_status",
  "residence",
  "relationship_status",
  "children"
)

# Define variables to use in correlations
correlation_variables <- c(
  "ASRS_score",
  "Age",
  "phy_ill",
  "men_ill",
  "sex_prob",
  "socioeconomic_status"
)

# Add labels for display
correlation_labels <- correlation_variables
names(correlation_labels) <- c(
  "Adult ADHD", 
  "Age", 
  "Physical Conditions",
  "Mental Conditions",
  "Sexual Conditions",
  "SES"
  )

# Define fit metrics
fit_metrics <- c("chisq", "df", "rmsea", "cfi", "tli")

# Define minimum group size cutoff
min_group_size <- 460

# Set RNG seed for reproducibility
set.seed(42)

# Data import -------------------------------------------------------------

# Read prepared data
asrs_data <- readRDS(file.path("data", "prepared", "iss_asrs.Rds")) %>%
  select(
    id, 
    all_of(grouping_variables),
    all_of(demographic_variables),
    all_of(correlation_variables),
    ASRS1:ASRS6, ASRS_score, ASRS_score_alt
  )

# Perform missing completely at random test
mcar_test <- misty::na.test(asrs_data[, colnames(asrs_data) %in% sprintf("ASRS%i", 1:6)])
mcar_test

# Drop observations with missing values in any ASRS item
asrs_data <- filter(asrs_data, !if_any(c(demographic_variables, sprintf("ASRS%i", 1:6)), is.na))

# Retain groups with min sample size (for measurement invariance tests)
asrs_data_invariance <- asrs_data[!asrs_data$country == "Other", ]
for (group in grouping_variables) {
  group_table <- table(asrs_data_invariance[[group]])
  asrs_data_invariance <- asrs_data_invariance[asrs_data_invariance[[group]] %in% names(group_table[group_table >= min_group_size]), ]
  asrs_data_invariance[[group]] <- factor(asrs_data_invariance[[group]], levels = sort(unique(asrs_data_invariance[[group]])))
}

# Inspect group sizes

# Full sample
sort(table(asrs_data$language))
sort(table(asrs_data$country))
sort(table(asrs_data$gender))

# Retained groups
sort(table(asrs_data_invariance$language))
sort(table(asrs_data_invariance$country))
sort(table(asrs_data_invariance$gender))

# Descriptive statistics --------------------------------------------------

## Sample composition ----

# Create table with sample composition for complete data set
demographic_table <-
  tibble(
    n = round(n_distinct(asrs_data$id), 2),
    mean_age = round(mean(asrs_data$Age), 2),
    sd_age = round(sd(asrs_data$Age), 2),
    n_male = n_distinct(asrs_data[asrs_data$gender == "Man", ]$id),
    perc_male = n_male / n * 100,
    n_female = n_distinct(asrs_data[asrs_data$gender == "Woman", ]$id),
    perc_female = n_female / n * 100,
    n_diverse = n_distinct(asrs_data[asrs_data$gender == "Gender diverse individual", ]$id),
    perc_diverse = n_diverse / n * 100
  )

knitr::kable(t(demographic_table), digits = 2)

## Summary statistics ----

# Create table with summary statistics for complete data set
summary_table <- rstatix::get_summary_stats(asrs_data, ASRS1:ASRS6, ASRS_score, type = "full")
summary_table$skew <- datawizard::skewness(select(asrs_data, ASRS1:ASRS6, ASRS_score))[[2]]
summary_table$kurt <- datawizard::kurtosis(select(asrs_data, ASRS1:ASRS6, ASRS_score))[[2]]
summary_table

# Create table with summary statistics of ASRS score for countries that satisfy min sample size
summary_table_group <-
  asrs_data_invariance %>%
  group_by(country) %>%
  rstatix::get_summary_stats(ASRS_score, show = c("mean", "sd", "min", "max", "ci")) %>%
  ungroup() %>%
  mutate(lower.CL = mean - ci, upper.CL = mean + ci)

summary_table_group

# Visualize ASRS means across countries
asrs_data %>%
  select(ASRS1:ASRS6) %>%
  gather(item, value) %>%
  ggplot(aes(x = item, y = value, color = item, fill = item)) +
  geom_boxplot(width = 0.25, color = "black", outlier.colour = "transparent") +
  stat_summary(geom = "point", fun = mean, shape = 1, color = "black") +
  labs(x = "Item", y = "Value") +
  coord_cartesian(ylim = c(0, 4)) +
  see::scale_fill_see() +
  see::theme_modern() +
  theme(
    legend.position = "none"
  )

# Create a table with sample sizes across countries
demographic_table_countries <- 
  lapply(asrs_data[, demographic_variables], function(x) as.data.frame(table(x))) %>%
  bind_rows(.id = "var") %>%
  rename(n = "Freq") %>%
  mutate(var = factor(var, levels = demographic_variables)) %>%
  group_by(var) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  arrange(var) %>%
  ungroup() %>%
  mutate(
    n = format(as.integer(n), big.mark = ","),
    perc = format(round(perc, 2), nsmall = 2)
  )

# Psychometric analysis ---------------------------------------------------

## Item correlations ----

# Perform Pearson's correlation for ASRS items
asrs_correlations <- Hmisc::rcorr(as.matrix(asrs_data[, colnames(asrs_data) %in% sprintf("ASRS%i", 1:6)]))
corrplot::corrplot(
  asrs_correlations$r,
  method = "shade", type = "full",
  diag = FALSE,
  col = colorRampPalette(c("blue4", "white", "red4"))(100),
  order = "hclust", addrect = 2,
  tl.col = "black",
  addCoef.col = "black",
  p.mat = asrs_correlations$P,
  insig = "blank"
)

## Confirmatory factor analysis ----

# Define CFA model
cfa_model <- "ASRS =~ ASRS1 + ASRS2 + ASRS3 + ASRS4 + ASRS5 + ASRS6"

# Fit CFA model
cfa_fit <- cfa(cfa_model, data = asrs_data, estimator = "DWLS")

# Inspect fitted CFA model
cfa_summary <- summary(cfa_fit, standardize = TRUE, fit.measures = TRUE)
cfa_summary$fit

# Extract edges from CFA model
cfa_edges <- get_edges(cfa_fit, label = "est_sig_std")

# Define layout for CFA model
cfa_layout <- get_layout(
  "", "", "", "", "", "ASRS", "", "", "", "", "",
  "", "", "", "", "", "", "", "", "", "", "",
  "ASRS1", "", "ASRS2", "", "ASRS3", "", "ASRS4", "", "ASRS5", "", "ASRS6",
  rows = 3
)

# Visualize CFA model
cfa_graph <- graph_sem(model = cfa_fit, edges = cfa_edges, layout = cfa_layout, fix_coord = TRUE, angle = 180)
cfa_graph

# Measurement invariance --------------------------------------------------

# Checkpoints
inv_metrics_path <- file.path("checkpoints", "invariance_metrics.Rds")
inv_deltas_path <- file.path("checkpoints", "invariance_deltas.Rds")

if (!file.exists(inv_metrics_path) | !file.exists(inv_deltas_path)) {
  invariance_levels <- c("configural", "metric", "scalar", "residual", "varcov", "means")
  invariance_constraints <- c("none", "loadings", "intercepts", "residuals", "varcov", "means")
  invariance_constraints_labels <- c("", "a", "b", "c", "d")
  invariance_constraints2 <- c("lv.variances", "lv.covariances", "means")
  invariance_operators <- c(NA, "=~", "~1", "~~")
  invariance_metrics <- data.frame(matrix(NA, ncol = 8))
  invariance_deltas <- data.frame(matrix(NA, ncol = 8))
  names(invariance_metrics) <- c("var", "model", "mi", fit_metrics)
  names(invariance_deltas) <- c("var", "comp", "mi", fit_metrics)

  for (v in grouping_variables) {
    cat("\n\n# Testing invariances for variable:", v)
    
    d <- asrs_data_invariance
    stop <- FALSE

    for (i in 2:length(invariance_levels)) {
      cat("\n## Invariance level:", invariance_levels[i])

      if (i == 5) {
        constraints_x <- invariance_constraints[2:4]
        constraints_y <- c(invariance_constraints[2:4], invariance_constraints2[1:2])
      } else if (i == 6) {
        constraints_x <- c(invariance_constraints[2:4], invariance_constraints2[1:2])
        constraints_y <- c(invariance_constraints[2:4], invariance_constraints2[1:3])
      } else if (i == 2) {
        constraints_x <- invariance_constraints[1]
        constraints_y <- invariance_constraints[2:i]
      } else {
        constraints_x <- invariance_constraints[2:(i - 1)]
        constraints_y <- invariance_constraints[2:i]
      }

      model_x <- cfa(cfa_model, data = d, group = v, group.equal = constraints_x, estimator = "DWLS")
      model_x_label <- paste0("m", i - 1)
      model_y <- cfa(cfa_model, data = d, group = v, group.equal = constraints_y, estimator = "DWLS")
      model_y_label <- paste0("m", i)
      fit_y <- fitmeasures(model_y, fit.measures = fit_metrics)
      fit_x <- fitmeasures(model_x, fit.measures = fit_metrics)
      delta_y <- fit_y - fit_x
      comp_label <- paste0(model_y_label, "-", model_x_label)
      invariance_metrics <- rbind(invariance_metrics, c(v, model_x_label, "none", fit_x), c(v, model_y_label, "none", fit_y))
      invariance_deltas <- rbind(invariance_deltas, c(v, comp_label, "none", delta_y))
      is_invariant <- ifelse((abs(delta_y[["rmsea"]]) < 0.015 & abs(delta_y[["cfi"]]) < 0.01), TRUE, FALSE)

      if (!is_invariant & !i > 4) {
        cat("\n### Noninvariance found.")
        cat("\n### Testing partial invariance.")
        cat("\n### Checking mod indices for:", invariance_constraints[i])
        lavaan_test_score <- lavTestScore(model_y, epc = TRUE)
        uni <- lavaan_test_score$uni
        epc <- lavaan_test_score$epc
        mod_indices <- merge(uni, epc, by.x = c("lhs", "rhs"), by.y = c("label", "plabel"), all.x = TRUE)
        mod_indices <- mod_indices[mod_indices$op.y == invariance_operators[i], ]
        mod_indices <- unique(apply(mod_indices[order(-mod_indices$X2), 7:9], 1, paste0, collapse = ""))
        iteration <- 0

        while (!is_invariant) {
          iteration <- iteration + 1
          if (iteration > length(mod_indices)) {
            cat("\nMaximum mod indice reached!")
            stop <- TRUE
            break
          } else {
            mod_indices_subset <- mod_indices[1:iteration]
            cat("\n#### Relaxing constraint:", mod_indices_subset[iteration])
            model_y_iteration <- update(model_y, group.partial = mod_indices_subset)
            fit_y_iteration <- fitmeasures(model_y_iteration, fit.measures = fit_metrics)
            delta_y_iteration <- fit_y_iteration - fit_x
            model_y_label_iteration <- paste0(model_y_label, "_", iteration)
            comp_label_iteration <- paste0(model_y_label_iteration, "-", model_x_label)
            invariance_metrics <- rbind(invariance_metrics, c(v, model_y_label_iteration, mod_indices_subset[iteration], fit_y_iteration))
            invariance_deltas <- rbind(invariance_deltas, c(v, comp_label_iteration, mod_indices_subset[iteration], delta_y_iteration))
            is_invariant <- ifelse((abs(delta_y_iteration[["rmsea"]]) < 0.015 & abs(delta_y_iteration[["cfi"]]) < 0.01), TRUE, FALSE)
          }
        }
      }

      if (is_invariant) {
        cat("\n#", invariance_levels[i], "invariance achieved.\n")
      } else {
        cat("\n#", invariance_levels[i], "invariance NOT achieved.\n")
        stop <- TRUE
      }

      # if (stop) break
    }
  }

  # Format model labels (model metrics)
  invariance_metrics <- drop_na(invariance_metrics)
  invariance_metrics <- distinct(invariance_metrics)
  invariance_metrics$model <- paste(invariance_metrics$model, invariance_levels[as.integer(str_extract(invariance_metrics$model, "(?<=m)\\d{1}"))])
  invariance_metrics$model <- str_replace(invariance_metrics$model, "_\\d{1}", letters[as.integer(str_extract(invariance_metrics$model, "(?<=_)\\d{1}"))])
  invariance_metrics$model <- ifelse(str_detect(invariance_metrics$model, "m\\d{1}\\w{1}"), str_replace(invariance_metrics$model, " ", " partial "), invariance_metrics$model)

  # Format model labels (model metric deltas)
  invariance_deltas <- drop_na(invariance_deltas)
  invariance_deltas <- distinct(invariance_deltas)
  invariance_deltas$comp <- str_replace(invariance_deltas$comp, "_\\d{1}", letters[as.integer(str_extract(invariance_deltas$comp, "(?<=_)\\d{1}"))])

  # Save model metrics checkpoint
  saveRDS(invariance_metrics, inv_metrics_path)

  # Save model deltas checkpoint
  saveRDS(invariance_deltas, inv_deltas_path)
} else {
  # Load model metrics checkpoint
  invariance_metrics <- readRDS(inv_metrics_path)

  # Load model deltas checkpoint
  invariance_deltas <- readRDS(inv_deltas_path)
}

# Format model metrics
metrics <-
  invariance_metrics %>%
  mutate(across(chisq:cfi, as.numeric)) %>%
  arrange(var, model)

# Format model deltas
deltas <-
  invariance_deltas %>%
  mutate(
    across(chisq:cfi, as.numeric),
    inv = if_else(abs(rmsea) <= 0.015 & abs(cfi) <= 0.01, "Yes", "No")
  ) %>%
  arrange(var, comp)

knitr::kable(deltas, digits = 2)
knitr::kable(deltas[deltas$inv == "Yes", ], digits = 2)

metrics_group <- data.frame(matrix(NA, ncol = 7))
names(metrics_group) <- c("var", "group", fit_metrics)

for (v in grouping_variables) {
  groups <- unique(asrs_data_invariance[[v]])

  for (g in groups) {
    data_subset <- asrs_data_invariance[asrs_data_invariance[[v]] == g, ]
    model_x <- cfa(cfa_model, data = data_subset, estimator = "DWLS")
    fit_x <- fitmeasures(model_x, fit.measures = fit_metrics)
    metrics_group <- rbind(metrics_group, c(v, g, fit_x))
  }
}

metrics_group <- arrange(metrics_group, var, group)

# Reliability analysis ----------------------------------------------------

# Calculate Cronbach's alpha
psych::alpha(asrs_data[, colnames(asrs_data) %in% sprintf("ASRS%i", 1:6)])

# Calculate McDonald's omega
psych::omega(asrs_data[, colnames(asrs_data) %in% sprintf("ASRS%i", 1:6)], fm = "ml")

# Validity analysis -------------------------------------------------------

## Gender differences ----

# Check differences in total ASRS score between gender identities
gender_model <- lm(ASRS_score ~ gender, data = asrs_data)
anova(gender_model)
effectsize::eta_squared(gender_model)
emmeans::emmeans(gender_model, pairwise ~ gender, adjust = "fdr")
rstatix::cohens_d(asrs_data, ASRS_score ~ gender)
aggregate(asrs_data$ASRS_score ~ asrs_data$gender, FUN = "mean")
aggregate(asrs_data$ASRS_score ~ asrs_data$gender, FUN = "sd")

## Country differences ----

# Check differences in total ASRS score between countries using t-test

# Test homogeneity of variance
car::leveneTest(ASRS_score ~ country, center = mean, data = asrs_data_invariance)

# Perform and format pairwise comparisons of ASRS score across countries
country_differences_t_test <-
  asrs_data_invariance %>%
  rstatix::t_test(
    ASRS_score ~ country,
    p.adjust.method = "fdr",
    detailed = TRUE
  ) %>%
  mutate(
    contrast = paste0(group1, " - ", group2),
    t = format(round(statistic, 2), nsmall = 2),
    p = if_else(p < 0.001, "< .001", format(round(p, 2), nsmall = 2)),
    p = if_else(p == "0.00", "< .001", p),
    p_adj = if_else(p.adj < 0.001, "< .001", format(round(p.adj, 2), nsmall = 2)),
    p_adj = if_else(p_adj == "0.00", "< .001", p_adj),
    difference = format(round(estimate, 2), nsmall = 2),
  ) %>%
  select(contrast, t, p, p_adj, difference)

# Calculate and format effect sizes for differences in ASRS score across countries
country_differences_effect_sizes <-
  rstatix::cohens_d(asrs_data_invariance, ASRS_score ~ country) %>%
  mutate(
    contrast = paste0(group1, " - ", group2),
    d = format(round(effsize, 2), nsmall = 2)
  ) %>%
  select(contrast, d)

# Combine results of pairwise comparisons
country_differences <- left_join(country_differences_t_test, country_differences_effect_sizes)

# Fit linear regression to check differences in total ASRS score between countries.
# Use gender and age as covs.
country_model <- lm(ASRS_score ~ country + gender + Age, data = asrs_data_invariance)
anova(country_model)
effectsize::eta_squared(country_model)

# Extract marginal means of ASRS score across countries
country_emmeans <- emmeans::emmeans(country_model, pairwise ~ country, infer = TRUE, adjust = "fdr")
country_means <- as.data.frame(country_emmeans$emmeans)
aggregate(asrs_data$ASRS_score ~ asrs_data$country, FUN = "mean")
aggregate(asrs_data$ASRS_score ~ asrs_data$country, FUN = "sd")
country_order <- country_means$country[order(country_means$emmean)]

# Visualize marginal means of ASRS score across countries
country_means_plot <-
  country_means %>%
  mutate(type = "Adjusted") %>%
  select(type, country, value = emmean, lower.CL, upper.CL) %>%
  bind_rows(
    summary_table_group %>%
      mutate(type = "Empirical") %>%
      select(type, country, value = mean, lower.CL, upper.CL)
  ) %>%
  mutate(
    type = factor(type, levels = c("Empirical", "Adjusted")),
    country = factor(country, levels = country_order)
  ) %>%
  ggplot(aes(x = country, y = value, fill = type)) +
  geom_col(
    color = "black", width = 0.75,
    position = position_dodge(width = 0.75)
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    position = position_dodge(width = 0.75)
  ) +
  labs(x = "Country", y = "ASRS Score", fill = "Means") +
  coord_cartesian(y = c(0, 18)) +
  scale_x_discrete(expand = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 18, 3)) +
  scale_fill_manual(values = c("royalblue4", "green4"), breaks = c("Empirical", "Adjusted")) +
  see::theme_modern() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.ticks = element_line()
  )

country_means_plot

# Append marginal means to empirical means ASRS score across countries
summary_table_group_adj <-
  summary_table_group %>%
  left_join(select(country_means, country, emmean, lower.CL, upper.CL)) %>%
  mutate(
    across(c(mean, sd, emmean, lower.CL, upper.CL), ~ format(round(.x, 2), nsmall = 2)),
    m = paste0(mean, " (", sd, ")"),
    m_adj = paste0(emmean, " [", lower.CL, ", ", upper.CL, "]")
  ) %>%
  select(country, n, m, m_adj, min, max)

# Country comparisons
country_comparisons <- as.data.frame(country_emmeans$contrasts)
country_comparisons$difference <- format(round(country_comparisons$estimate, 2), nsmall = 2)
country_comparisons$t <- format(round(country_comparisons$t.ratio, 2), nsmall = 2)
country_comparisons$p <- ifelse(country_comparisons$p.value < 0.001, "< .001", format(round(country_comparisons$p.value, 3), nsmall = 3))
country_comparisons$p <- if_else(country_comparisons$p == "0.00", "< .001", country_comparisons$p)
country_comparisons <- country_comparisons[, c("contrast", "t", "p", "difference")]

## Construct validity ----

# Calculate correlations of ASRS with additional vars
correlations_validity <-
  asrs_data %>%
  select(correlation_labels) %>%
  mutate(SES = as.numeric(SES)) %>%
  correlation::correlation() %>%
  mutate(r = round(r, 2)) %>%
  summary(redundant = FALSE, digits = 2)

# Visualize correlations of ASRS with additional vars
correlation_plot_recipe <-
  correlation::visualisation_recipe(
    correlations_validity,
    # Adjust correlation value labels formatting
    text = list(size = 5, family = "serif"),
    tile = list(color = "white"),
    labs = list(title = "")
  )

correlation_plot <-
  plot(correlation_plot_recipe) +
  scale_fill_gradientn(
    colors = c("royalblue4", "white", "red4"),
    limits = c(-1, 1),
    guide = guide_colorbar(
      title = "r",
      direction = "vertical",
      nbin = 100,
      ticks.colour = "black",
      frame.colour = "black",
      barwidth = 1.5,
      barheight = 26
    )
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    aspect.ratio = 1,
    legend.position = "right",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    # Adjust axis labels formatting
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y = element_text(size = 12)
  )

correlation_plot

# Clinical applicability --------------------------------------------------

## Prevalence by country ----

applicability_country_num <-
  asrs_data %>%
  select(country, ASRS_score) %>%
  mutate(cutoff = if_else(between(ASRS_score, 0, 13), "Lower", "Greater")) %>%
  select(-ASRS_score) %>%
  count(country, cutoff) %>%
  group_by(country) %>%
  mutate(prop = prop.table(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = "cutoff", values_from = c("n", "prop"))

# Create and format a table with occurrence by country
applicability_table <-
  applicability_country_num %>%
  mutate(
    across(c(n_Greater, n_Lower), ~ format(round(.x, 1), nsmall = 1, big.mark = ",")),
    across(c(prop_Greater, prop_Lower), ~ .x * 100),
    across(c(prop_Greater, prop_Lower), ~ format(round(.x, 2), nsmall = 2))
  ) %>%
  select(country, n_Lower, prop_Lower, n_Greater, prop_Greater)

sum(applicability_country_num$n_Lower)
round((sum(applicability_country_num$n_Lower) / (sum(applicability_country_num$n_Greater) + sum(applicability_country_num$n_Lower))) * 100, 2)

sum(applicability_country_num$n_Greater)
round((sum(applicability_country_num$n_Greater) / (sum(applicability_country_num$n_Greater) + sum(applicability_country_num$n_Lower))) * 100, 2)

## Prevalence by gender ----

applicability_gender_num <-
  asrs_data %>%
  select(gender, ASRS_score) %>%
  mutate(cutoff = if_else(between(ASRS_score, 0, 13), "Lower", "Greater")) %>%
  select(-ASRS_score) %>%
  count(gender, cutoff) %>%
  group_by(gender) %>%
  mutate(prop = prop.table(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = "cutoff", values_from = c("n", "prop"))

knitr::kable(applicability_gender_num, digits = 4)

## Alternative threshold analysis ----

applicability_alt <-
  asrs_data %>%
  select(country, ASRS_score_alt) %>%
  mutate(cutoff = if_else(between(ASRS_score_alt, 0, 3), "Lower", "Greater")) %>%
  select(-ASRS_score_alt) %>%
  count(cutoff) %>%
  mutate(prop = prop.table(n)) %>%
  pivot_wider(names_from = "cutoff", values_from = c("n", "prop")) %>%
  drop_na()

knitr::kable(applicability_alt, digits = 4)

# Results export ----------------------------------------------------------

# Save plots
ggsave(file.path("output", "asrs_correlation_plot.png"), correlation_plot, width = 10, height = 10, scale = 0.75)
ggsave(file.path("output", "asrs_country_means.png"), country_means_plot, width = 10, height = 6, scale = 1)

# Save tables
write_csv(summary_table, file.path("output", "asrs_descriptive_statistics.csv"))
write_csv(summary_table_group, file.path("output", "asrs_descriptive_statistics_by_country.csv"))
write_csv(summary_table_group_adj, file.path("output", "asrs_descriptive_statistics_by_country_adjusted.csv"))
write_csv(metrics, file.path("output", "asrs_invariance_metrics.csv"))
write_csv(deltas, file.path("output", "asrs_invariance_deltas.csv"))
write_csv(metrics_group, file.path("output", "asrs_metrics_by_group.csv"))
write_csv(demographic_table_countries, file.path("output", "asrs_demographics.csv"))
write_csv(country_differences, file.path("output", "asrs_country_pairwise_comparisons.csv"))
write_csv(country_comparisons, file.path("output", "asrs_country_comparisons.csv"))
write_csv(applicability_table, file.path("output", "asrs_applicability.csv"))
write_csv(country_means, file.path("output", "asrs_country_estimated_marginal_means.csv"))
