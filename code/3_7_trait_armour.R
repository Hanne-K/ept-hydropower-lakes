#######################################################

# 3.7 Functional trait: Body armour

######################################################

# 0. Structure dataframe ----

# Create event backbone
events_df <- occurrences_df %>%
  dplyr::distinct(Status,locality,internal_eventID)

# Filter to species-level data
occurrences_sp_df <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES")

# Summarize counts
armor_sum_occ <- occurrences_sp_df %>%
  dplyr::select(Status,locality,internal_eventID,scientificName,taxonRank,individualCount,material) %>%
  dplyr::mutate(material = factor(material, levels = c("sclerotized","silk","mineral","mixed","vegetation")),
                Status = factor(Status)) %>%
  dplyr::group_by(Status,locality,internal_eventID,material) %>%
  dplyr::summarise(sum_occ = sum(individualCount, na.rm = TRUE),
                   .groups = "drop")

# Reintroduce missing events and trait levels with zero counts
armor_event_df2 <- events_df %>%
  tidyr::crossing(material = factor(c("sclerotized","silk","mineral","mixed","vegetation"),
                                    levels = c("sclerotized","silk","mineral","mixed","vegetation"))) %>%
  dplyr::left_join(
    armor_sum_occ,
    by = c("Status","locality","internal_eventID","material")) %>%
  dplyr::mutate(sum_occ = tidyr::replace_na(sum_occ, 0))

# Order levels of Status
armor_event_df2$Status <- factor(armor_event_df2$Status, levels = c("control","regulated"))
armor_event_df2$internal_eventID <- factor(armor_event_df2$internal_eventID)

# 1. Test for overdispersion ----

# Fit model with standard distribution for count data
fit_armor <- glmer(
  sum_occ ~ material * Status + (1|locality/internal_eventID),
  family = poisson,
  data = armor_event_df2
)

# Check for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  sqrt(sum(rp^2) / rdf)
}

overdisp_fun(fit_armor)
# 2.76 <- overdispersed, so switching to neg. binomial
# with event: 1.53, slightly oversidpersed

mean(armor_event_df2$sum_occ) # 9
var(armor_event_df2$sum_occ) # 620
# mean << variance

# 2. Fit with negative binomial ----

# Use glmmTMB: can handle more issues than glmer.nb, incl. zero infl.
library(glmmTMB)
citation("glmmTMB")

fit_armor_nb <- glmmTMB(
  sum_occ ~ material * Status + (1|locality/internal_eventID),
  family = nbinom2,
  data = armor_event_df2
)

# Check for zero inflation issues
armor_event_df2 %>% count(sum_occ) # 156 zeros of 300

performance::check_zeroinflation(fit_armor_nb)
# Model seems ok, ratio of observed and predicted zeros is within the tolerance range (p = 0.632, new = 0.736).
# Will not adjust for zero inflation.

# Summary
summary(fit_armor_nb)

# 3. Trait specific tests ----

# Extract contrasts
emm_armour <- emmeans(fit_armor_nb, ~ Status | material)

# Correct for multiple testing 
# (adjusting p-values as we have multiple trait-level comparisons)
contrast_results_armour <- contrast(
  emm_armour,
  method = "pairwise",
  adjust = "holm" # are several options here, "tukey" typical for all pariwise comp
  
)

# 4. Interpretation ----

# Back-transform, but obs then we get ratios because of log link
summary(contrast_results_armour, type = "response")

# Get confidence intervals
confint(contrast_results_armour, type = "response") # or rather profile CI?


# Save df (needed to create Fig 5 forest plot)
contrast_armour_df <- as.data.frame(summary(contrast_results_armour, type = "response"))
confint_armour_df <- as.data.frame(confint(contrast_results_armour, type = "response"))

results_armour_df <- left_join(contrast_armour_df,confint_armour_df)

# Additional checks
# Does effect of regulation differ across traits at all?
anova(fit_nb)
car::Anova(fit_nb, type = 3) # type II/III tests
