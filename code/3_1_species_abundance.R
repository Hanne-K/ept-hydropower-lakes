##############################################

# 3.1 Species abundance

##############################################

# 1. Create dataframe ----------------------------------------------------------

# Add the events missing EPT species records
event_ab_df <- occurrences_df %>%
  dplyr::group_by(internal_eventID) %>%
  dplyr::summarise(Status = as.factor(paste0(unique(Status), collapse = ", ")),
                   locality = as.factor(paste0(unique(locality), collapse = ", ")),
                   year = as.factor(paste0(unique(year), collapse = ", ")),
                   month = as.numeric(paste0(unique(month), collapse = ", ")),
                   day = as.numeric(paste0(unique(day), collapse = ", ")),
                   eventDate = as.Date(paste0(unique(eventDate), collapse = ", ")),
                   periodNumber = paste0(unique(periodNumber), collapse = ", "), # should this be factor or integer?
                   sum_individualCount = as.numeric(sum(individualCount[taxonRank == "SPECIES"], na.rm = TRUE))
  )

# Order levels in year factor
event_ab_df$year <- factor(event_ab_df$year, levels = c("2023","2024"))


# 2. Investigate dataset structure ---------------------------------------------

## 2.1 Basic stats -------------------------------------------------------------

mean(event_ab_df$sum_individualCount) # 43.45
var(event_ab_df$sum_individualCount) # 2812.828
min(event_ab_df$sum_individualCount) # 0
max(event_ab_df$sum_individualCount) # 322

sd(event_ab_df$sum_individualCount) # 53.0361

stderror <- function(x) sd(x)/sqrt(length(x))
stderror(event_ab_df$sum_individualCount) # 6.846931

## 2.2 Density distribution figure ---------------------------------------------

jpeg(here::here("results","figures","fig_densitydistr_abundance.jpg"), width = 20, height = 12, units="cm", res=300)
ggplot(data = event_ab_df, aes(sum_individualCount)) + 
  geom_histogram() + xlab("Number of individuals (ind/sample)") + 
  ylab("Count (number of samples)") + 
  ggtitle("Number of ind./sample, range 0-322, mean = 44, var = 2813") + 
  geom_vline(xintercept = 43.45, linetype="dotted", color = "red", size=1) +
  theme_classic()

dev.off()


# 3. Find appropriate distribution and consider random effects ----------------- 

## 3.1 Check fit to different distributions ------------------------------------

# Skewness-kurtosis plots for the data, showing observed data and theoretical distributions
fitdistrplus::descdist(data = event_ab_df$sum_individualCount, discrete = TRUE, boot = 1000) 
# the blue are observations, the orange are bootstrap values 
# indicate that a negative binomial may be appropriate

# Fit data to the different relevant distributions
## Poisson and nbinomial
poisson_1 <- fitdist(data = event_ab_df$sum_individualCount, distr = "pois", method = "mle", discrete = TRUE)
nbinom_1 <- fitdist(data = event_ab_df$sum_individualCount, distr = "nbinom", method = "mle", discrete = TRUE)

par(mfrow = c(1,1))
plot(poisson_1, title = "Poisson")
plot(nbinom_1, title = "Negative Binomial")

## Normal
normal_1 <- fitdist(data = event_ab_df$sum_individualCount, distr = "norm")
plot(normal_1)

# Looks like negative binomial may be appropriate

## 3.2 Random effects ----------------------------------------------------------

# Negative binomial model without random effects
Abundance_nb_norandom <- MASS::glm.nb(formula = sum_individualCount ~ Status*year, 
                                      data = event_ab_df, na.action = na.exclude)
MuMIn::AICc(Abundance_nb_norandom) # 570.8977

# Negative binomial model with random effects
Abundance_nb <- lme4::glmer.nb(formula = sum_individualCount ~ Status*year + (1|locality), 
                               data = event_ab_df, na.action = na.exclude)
MuMIn::AICc(Abundance_nb) # 553.9292

# Random effects seem to improve the model, with delta AICc > 2 (= 16.9685)

## 3.3 Model assumptions negative binomial -------------------------------------

# 6:TRUE
# As variance far exceeds the mean, this indicate extreme overdispersion 
# which strongly suggest that a negative binomial regression model may be appropriate. 
# Nb model include a dispersion parameter theta.

# 7: TRUE
# dispersion statistic approximating 1.0 and an AIC/BIC and log-likelihood statistic less than alternative count models.

## Without random effects
# Poisson model without random effects
Abundance_poisson_norandom <- glm(formula = sum_individualCount ~ Status*year, 
                                  data = event_ab_df, na.action = na.exclude, family = "poisson")
msme::P__disp(Abundance_poisson_norandom)
# pearson.chi2   dispersion 
#   3095.35026     55.27411 
# dispersion statistic is high

# Negative binomial model without random effects
Abundance_nb_norandom <- MASS::glm.nb(formula = sum_individualCount ~ Status*year, 
                                      data = event_ab_df, na.action = na.exclude)
msme::P__disp(Abundance_nb_norandom)
# pearson.chi2   dispersion 
#     62.966001     1.124393 
# dispersion statistic approximating 1.

# AIC comparisons
## Comparing AIC of poisson and nb models without random effects
MuMIn::AICc(Abundance_poisson_norandom) # 2698.874
MuMIn::AICc(Abundance_nb_norandom) # 570.8977
# Lowest for nb model, which supports that this is the better model

## Comparing AIC of poisson and nb models with random effects
# Poisson model with random effects
Abundance_poisson <- lme4::glmer(formula = sum_individualCount ~ Status*year + (1|locality), 
                                 data = event_ab_df, na.action = na.exclude, family = "poisson")
MuMIn::AICc(Abundance_poisson) # 1702.367
# Negative binomial model with random effects
MuMIn::AICc(Abundance_nb) # 553.9292

# Both with and without random effects, a negative binomial model is preferred. 


# 4. Model selection nb --------------------------------------------------------

## 4.1 Find dispersion parameter (theta) ---------------------------------------

# Extract dispersion parameter
lme4::getME(Abundance_nb, name = "glmer.nb.theta") 

my_theta <- 1.328949

## 4.2 Fit candidate models ----------------------------------------------------

# Fit candidate models
# Model 1 full
Ab_nb_fix_1 <- glmer(formula = sum_individualCount ~ Status*year + (1|locality), 
                     data = event_ab_df,
                     na.action = na.exclude,
                     family = MASS::negative.binomial(theta = my_theta))

# Model 2 no interaction
Ab_nb_fix_2 <- glmer(formula = sum_individualCount ~ Status + year + (1|locality), 
                     data = event_ab_df,
                     na.action = na.exclude,
                     family = MASS::negative.binomial(theta = my_theta))

# Model 3 only Status
Ab_nb_fix_3 <- glmer(formula = sum_individualCount ~ Status + (1|locality), 
                     data = event_ab_df,
                     na.action = na.exclude,
                     family = MASS::negative.binomial(theta = my_theta))

# Model 4 only year
Ab_nb_fix_4 <- glmer(formula = sum_individualCount ~ year + (1|locality), 
                     data = event_ab_df,
                     na.action = na.exclude,
                     family = MASS::negative.binomial(theta = my_theta))

# Model 5 only intercept
Ab_nb_fix_5 <- glmer(formula = sum_individualCount ~ 1 + (1|locality), 
                     data = event_ab_df,
                     na.action = na.exclude,
                     family = MASS::negative.binomial(theta = my_theta))

# List all candidate models
candidate_models <- list(Ab_nb_fix_1,Ab_nb_fix_2,Ab_nb_fix_3,Ab_nb_fix_4,Ab_nb_fix_5)

# Model selection table
MuMIn::model.sel(candidate_models)

# Model selection table old
#   (Int) Stt yer Stt:yer df   logLik  AICc delta weight
# 3 4.042   +              4 -255.843 520.5  0.00  0.354
# 5 3.439                  3 -257.365 521.2  0.72  0.247
# 2 4.130   +   +          5 -255.246 521.7  1.22  0.192
# 4 3.525       +          4 -256.737 522.3  1.79  0.145

# Model selection table new
#(Int) Stt yer Stt:yer df   logLik  AICc delta weight
#3 4.043   +              4 -270.503 549.7  0.00  0.430
#5 3.364                  3 -272.248 550.9  1.19  0.237
#2 3.929   +   +          5 -270.200 551.5  1.78  0.177
#4 3.242       +          4 -271.917 552.6  2.83  0.104
#1 3.960   +   +       +  6 -270.172 553.9  4.20  0.053

# ratios for models with delta AICc below 2
0.430/0.430 # 1
0.430/0.237 # 1.814346
0.430/0.177 # 2.429379

# 5. Investigate supported models ----------------------------------------------

# Refit without fixed theta

## 5.1 Highest ranked model (Model 3) ------------------------------------------

# Run model
Abundance_nb_3 <- lme4::glmer.nb(formula = sum_individualCount ~ Status + (1|locality), 
                                 data = event_ab_df, 
                                 na.action = na.exclude)
# Summary
summary(Abundance_nb_3)

# Confidence intervals
confint(Abundance_nb_3)
lme4::confint.merMod(Abundance_nb_3, method = "profile")

# Interpretation: 
# Highest ranked model and intercept only model have quite similar support,
# which could be because much of the variance lies in the random effects.
# We know that Gjøljavatnet is an outlier, more similar to controls.

# Check random-effect variance
lme4::VarCorr(Abundance_nb_3)
# Groups   Name        Std.Dev.
# locality (Intercept) 0.71314 

# Calculate conditional vs marginal R2
performance::r2(Abundance_nb_3)
# R2 for Mixed Models
# Conditional R2: 0.627 # variance explained by fixed + random
# Marginal R2: 0.300 # variance explained by fixed effects

### Effect plot ----------------------------------------------------------------

# Basic effect plot
plot(effects::allEffects(Abundance_nb_3))
# Example, if we want to get the values for each year separately
# emmeans_1 <- emmeans::emmeans(Abundance_nb_3, revpairwise ~ Status*order | year)

# Extract estimated means
emm_status <- emmeans::emmeans(Abundance_nb_3, ~ Status, type = "response") 
emm_status_df <- as.data.frame(emm_status)
head(emm_status_df)

# Customised effect plot
jpeg(here::here("results","figures","fig_emmeans_ab_mod_3.jpg"), width = 10, height = 10, units="cm", res=300)

fig_emmeans_ab_mod_3 <- ggplot(event_ab_df, aes(x = Status, y = sum_individualCount, color = Status))+
  geom_jitter(width = 0.1, alpha = 0.7, size = 2)+
  scale_color_manual(values = c("#E6E6E6","#7A7A7A"))+
  theme_classic()+
  theme(legend.position = "none")

fig_emmeans_ab_mod_3 <- fig_emmeans_ab_mod_3 +
  geom_point(data = emm_status_df,
             aes(x = Status, y = response),
             inherit.aes = FALSE,
             size = 3, 
             color = "black") +
  geom_errorbar(data = emm_status_df,
                aes(x = Status, ymin = asymp.LCL, ymax = asymp.UCL),
                inherit.aes = FALSE,
                width = 0.1) +
  labs(y = "Predicted abundance (mean \u00B1 95% CI)",
       x = "Regulation status")

ggMarginal(fig_emmeans_ab_mod_3, 
           groupColour = TRUE, 
           groupFill = TRUE, 
           margins = "y")

dev.off()

# Calculating percentage change in expected abundance
((2.69-4.04)/4.04)*100 # -33.41584
round(((2.69-4.04)/4.04)*100, digits = 0)

## 5.2 Second highest ranked (intercept only)-----------------------------------

Abundance_nb_5 <- lme4::glmer.nb(formula = sum_individualCount ~ 1 + (1|locality), 
                                 data = event_ab_df, 
                                 na.action = na.exclude)

summary(Abundance_nb_5)
lme4::confint.merMod(Abundance_nb_5, method = "profile")

## 5.3 Third highest -----------------------------------------------------------

Abundance_nb_2 <- lme4::glmer.nb(formula = sum_individualCount ~ Status + year + (1|locality), 
                                 data = event_ab_df, 
                                 na.action = na.exclude)

summary(Abundance_nb_2)
lme4::confint.merMod(Abundance_nb_2, method = "profile")

## 5.4 Forth ranked (abundance ~ year) -----------------------------------------

Abundance_nb_4 <- lme4::glmer.nb(formula = sum_individualCount ~ year + (1|locality), 
                                 data = event_ab_df, 
                                 na.action = na.exclude)

summary(Abundance_nb_4)
lme4::confint.merMod(Abundance_nb_4, method = "profile")

## 5.5 Fifth ranked (abundance ~ status*year) ----------------------------------

Abundance_nb_1 <- lme4::glmer.nb(formula = sum_individualCount ~ Status*year + (1|locality), 
                                 data = event_ab_df, 
                                 na.action = na.exclude)

summary(Abundance_nb_1)
lme4::confint.merMod(Abundance_nb_1, method = "profile")


# Species abundance over time --------------------------------------------------

# Plot to investigate if there are any distinct trends 

# 2023
ggplot(subset(event_ab_df, year == 2023), aes(x = eventDate, y = sum_individualCount)) +
  geom_point() +
  facet_wrap(~ locality)

# 2024
ggplot(subset(event_ab_df, year == 2024), aes(x = eventDate, y = sum_individualCount)) +
  geom_point() +
  facet_wrap(~ locality)

ggplot(event_ab_df, aes(x = eventDate, y = sum_individualCount, color = year)) +
  geom_point() +
  facet_wrap(~ locality)
