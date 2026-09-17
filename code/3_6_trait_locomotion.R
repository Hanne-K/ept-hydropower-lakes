#######################################################

# 3.6 Functional trait: Locomotion and surface relation

######################################################

# 0. Structure dataframe ----

# Create event backbone
events_df <- occurrences_df %>%
  dplyr::distinct(Status,locality,internal_eventID)

# Filter to species-level data
occurrences_sp_df <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES")

# Summarize counts
locomotion_sum_occ <- occurrences_sp_df %>%
  dplyr::select(Status,locality,internal_eventID,scientificName,taxonRank,individualCount,locomotion) %>%
  dplyr::mutate(locomotion = factor(locomotion, levels = c("bur","crw","tat","wsw")),
                Status = factor(Status)) %>%
  dplyr::group_by(Status,locality,internal_eventID,locomotion) %>%
  dplyr::summarise(sum_occ = sum(individualCount, na.rm = TRUE),
                   .groups = "drop")

# Reintroduce missing events and trait levels with zero counts
locomotion_event_df2 <- events_df %>%
  tidyr::crossing(locomotion = factor(c("bur","crw","tat","wsw"),
                                      levels = c("bur","crw","tat","wsw"))) %>%
  dplyr::left_join(
    locomotion_sum_occ,
    by = c("Status","locality","internal_eventID","locomotion")) %>%
  dplyr::mutate(sum_occ = tidyr::replace_na(sum_occ, 0))

# Order levels of Status
locomotion_event_df2$Status <- factor(locomotion_event_df2$Status, levels = c("control","regulated"))
locomotion_event_df2$internal_eventID <- factor(locomotion_event_df2$internal_eventID)


# 1. Test for overdispersion ----

# Fit model with standard distribution for count data
fit <- glmer(
  sum_occ ~ locomotion * Status + (1|locality/internal_eventID),
  family = poisson,
  data = locomotion_event_df2
)

# Check for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  sqrt(sum(rp^2) / rdf)
}

overdisp_fun(fit)
# overdispersed, so switching to neg. binomial

mean(locomotion_event_df2$sum_occ) 
var(locomotion_event_df2$sum_occ) 
# mean << variance

# 2. Fit with negative binomial ----

# Use glmmTMB: can handle more issues than glmer.nb, incl. zero infl.
fit_loc_nb <- glmmTMB(
  sum_occ ~ locomotion * Status + (1|locality/internal_eventID),
  family = nbinom2,
  data = locomotion_event_df2
)

# Check for zero inflation issues
locomotion_event_df2 %>% count(sum_occ) # 114 zeros of 240

performance::check_zeroinflation(fit_loc_nb)
# Model seems ok, ratio of observed and predicted zeros is within the tolerance range (p = 0.480).
# Will not adjust for zero inflation.

summary(fit_loc_nb)

# 3. Trait specific tests ----

# Extract contrasts
emm_loc <- emmeans(fit_loc_nb, ~ Status | locomotion)

# Correct for multiple testing 
# (adjusting p-values as we have multiple trait-level comparisons)
contrast_results_loc <- contrast(
  emm_loc,
  method = "pairwise",
  adjust = "holm" # are several options here, "tukey" typical for all pariwise comp
  
)

# 4. Interpretation ----

# Back-transform, but obs then we get ratios because of log link
summary(contrast_results_loc, type = "response")

# Get confidence intervals
confint(contrast_results_loc, type = "response") # or rather profile CI?

# Save df (needed to create Fig 5 forest plot)
contrast_loc_df <- as.data.frame(summary(contrast_results_loc, type = "response"))
confint_loc_df <- as.data.frame(confint(contrast_results_loc, type = "response"))

results_loc_df <- left_join(contrast_loc_df,confint_loc_df)

# Additional checks
# Does effect of regulation differ across traits at all?
anova(fit_nb)
car::Anova(fit_nb, type = 3) # type II/III tests
