##############################################

# 3.2 Species richness

##############################################

# 1. Create dataframe ----------------------------------------------------------

# Create dataframe with data grouped by sampling event
event_sp_df <- occurrences_df %>%
  dplyr::group_by(internal_eventID) %>%
  dplyr::summarise(Status = as.factor(paste0(unique(Status), collapse = ", ")),
                   locality = as.factor(paste0(unique(locality), collapse = ", ")),
                   year = as.factor(paste0(unique(year), collapse = ", ")),
                   month = as.numeric(paste0(unique(month), collapse = ", ")),
                   day = as.numeric(paste0(unique(day), collapse = ", ")),
                   periodNumber = paste0(unique(periodNumber), collapse = ", "),
                   N_species = length(unique(taxonKey[taxonRank == "SPECIES"], na.rm = TRUE)))

# Order levels in year factor
event_sp_df$year <- factor(event_sp_df$year, levels = c("2023","2024"))


# 2. Investigate dataset structure ---------------------------------------------

## 2.1 Basic stats -------------------------------------------------------------

mean(event_sp_df$N_species) # 5.583333
var(event_sp_df$N_species) # 12.85734
# Variance is almost double the mean, indicating moderate overdispersion.
min(event_sp_df$N_species) # 0
max(event_sp_df$N_species) # 14

sd(event_sp_df$N_species) # 3.585714
stderror <- function(x) sd(x)/sqrt(length(x))
stderror(event_sp_df$N_species) # 0.4629137


## 2.2 Density distribution figure ---------------------------------------------

jpeg(here::here("results","figures","fig_densitydistr_spRichness.jpg"), width = 20, height = 12, units="cm", res=300)
ggplot(data = event_sp_df, aes(N_species)) + 
  geom_histogram() + xlab("Number of species (species/sample)") + 
  ylab("Count (number of samples)") + 
  ggtitle("Number of EPT species found per sampling event, range 1-14, mean = 5.6, var = 12.9") + 
  geom_vline(xintercept = 5.583333, linetype="dotted", color = "red", size=1) +
  theme_classic()

dev.off()

## 2.3 Species summary ---------------------------------------------------------

# Unique species per order and locality
table_unique_sp <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::group_by(locality,order) %>%
  dplyr::summarise(nr_species = length(unique(scientificName)))

# Unique species overall
unique_sp <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::summarise(nr_species = length(unique(scientificName)),
                   nr_individuals = sum(individualCount))

unique_EPT <- occurrences_df %>%
  dplyr::filter(taxonRank == "SPECIES") %>%
  dplyr::group_by(order) %>%
  dplyr::summarise(nr_species = length(unique(scientificName)))


# 3. Find appropriate distribution and consider random effects ----------------- 

## 3.1 Check fit to different distributions ------------------------------------

# Skewness-kurtosis plots for the data, showing observed data and theoretical distributions
fitdistrplus::descdist(data = event_sp_df$N_species, discrete = TRUE, boot = 1000) 
# the blue are observations, the orange are bootstrap values 
# interpretation: both observational and bootstrapped values are fairly close to ?

# Fit data to the different relevant distributions
## Poisson and nbinomial
poisson_1 <- fitdist(data = event_sp_df$N_species, distr = "pois", method = "mle", discrete = TRUE)
nbinom_1 <- fitdist(data = event_sp_df$N_species, distr = "nbinom", method = "mle", discrete = TRUE)

par(mfrow = c(1,1))
plot(poisson_1, title = "Poisson")
plot(nbinom_1, title = "Negative Binomial")

## Normal
normal_1 <- fitdist(data = event_sp_df$N_species, distr = "norm")
plot(normal_1)

# As we have overdispersed count data, a negative binomial model may be appropriate.

## 3.2 Random effects ----------------------------------------------------------

# Assuming a negative binomial model is appropriate, based on variance > mean.
# As variance is about 2x mean, this indicate only moderate overdispersion.

# Negative binomial model without random effects
SpRich_nb_norandom <- MASS::glm.nb(formula = N_species ~ Status + year + Status:year, 
                                   data = event_sp_df, 
                                   na.action = na.exclude)
MuMIn::AICc(SpRich_nb_norandom) # 320.3448

# Negative binomial model with random effects
SpRich_nb <- lme4::glmer.nb(formula = N_species ~ Status + year + Status:year + (1|locality), 
                            data = event_sp_df, 
                            na.action = na.exclude)
MuMIn::AICc(SpRich_nb) # 299.3404

320.3448-299.3404
# AICc decreases when random effects are included, delta AICc > 2, so model fit seems to increase. 
# Will therefore continue with random effects.

# Will look more into model assumptions for both nb and poisson, as both are still
# being considered.

## 3.3 Model assumptions negative binomial -------------------------------------

# There are certain assumptions that should be met for negative binomial models.
# The list is from Hilbe (2014), chapter 5: 

# Assumptions:
# 1 The response, y, is a count consisting of nonnegative integers.
# 2 As the value of my increases, the probability of 0 counts decreases.
# 3 y must allow for the possibility of 0 counts.
# 4 The fitted or predicted variable,my, is the expected mean of the distribution
# of y.  
# 5 The variance is closely approximated as my + alpha*my2 or (1 + alpha*my).
# 6 A foremost goal of NB regression is to model data in which the value of the
# variance exceeds the mean, or the observed variance exceeds the expected
# variance.
# 7 A well-fitted NB model has a dispersion statistic approximating 1.0 and an
# AIC/BIC and log-likelihood statistic less than alternative count models.
# 8 The model is not misspecified.
# 9 The number of predicted counts is approximately the same as the number
# of observed counts across the distribution of y.

# Checking assumptions, noting specifics:

# 6:TRUE
# Variance exceeds the mean
mean(event_sp_df$N_species) # 5.583333
var(event_sp_df$N_species) # 12.85734
# Variance is almost double the mean, indicating moderate overdispersion.

# 7: TRUE
# dispersion statistic approximating 1.0 and an AIC/BIC and log-likelihood statistic less than alternative count models.

## Without random effects
# Poisson model without random effects
SpRich_poisson_norandom <- glm(formula = N_species ~ Status + year + Status:year, 
                               data = event_sp_df, 
                               na.action = na.exclude, family = "poisson")
msme::P__disp(SpRich_poisson_norandom)
# pearson.chi2   dispersion 
# 127.643189     2.279343 
# dispersion statistic close ish to 1

# Negative binomial model without random effects
SpRich_nb_norandom <- MASS::glm.nb(formula = N_species ~ Status + year + Status:year, 
                                   data = event_sp_df, 
                                   na.action = na.exclude)
msme::P__disp(SpRich_nb_norandom)
# pearson.chi2   dispersion 
#  64.403479     1.150062 
# dispersion statistic approximating 1.

# AIC comparisons
## Comparing AIC of poisson and nb models without random effects
MuMIn::AICc(SpRich_poisson_norandom) # 335.0488
MuMIn::AICc(SpRich_nb_norandom) # 320.3448

## Comparing AIC of poisson and nb models with random effects
# Poisson model with random effects
SpRich_poisson <- lme4::glmer(formula = N_species ~ Status + year + Status:year + (1|locality), 
                              data = event_sp_df, 
                              na.action = na.exclude, family = "poisson")
MuMIn::AICc(SpRich_poisson) # 297.1478

# Negative binomial model with random effects
MuMIn::AICc(SpRich_nb) # 299.3404

# Is delta AICc larger than 2? 
299.3404-297.1478
# 2.1926 yes, lower AICc for poisson model which indicates poisson may be more appropriate
# When random effects are included we get lower AICc for the model with poisson distribution.

# For nested models such as those with negative binomial and poisson, LR test is preferred to information criteria such as AIC, according to Hilbe (2014). 
anova(SpRich_poisson,SpRich_nb, test = "LRT") 

# 9:
# Number of predicted versus observed counts
obspred_nb <- COUNT::nb2.obs.pred(14, SpRich_nb_norandom)

# Reshaping the dataframe
obspred_nb <- reshape2::melt(obspred_nb, id.vars = "Count")
obspred_nb <- obspred_nb %>% filter(variable %in% c("propObsv","propPred"))

# Both observed and predicted on the same plot
ggplot(obspred_nb,aes(Count, value,col = variable)) + geom_point() + stat_smooth()


# 4. Model selection nb --------------------------------------------------------

# It is possible that a poisson distribution could be better than a negative binomial. 
# Try to fit models with nb, and compare models further

# Summary
summary(SpRich_nb)
confint(SpRich_nb)

# Effect plot
plot(effects::allEffects(SpRich_nb))

## 4.1 Find dispersion parameter (theta) ---------------------------------------

# Extract dispersion parameter
lme4::getME(SpRich_nb, name = "glmer.nb.theta") # 61.98325
# As the value of theta is quite high, it could further indicate that 
# a poisson distribution may be more appropriate.

# Several aspects now suggest that a poisson distribution may be a better fit.
# Will therefore continue with a poisson distribution, and look further in to the corresponding assumptions.


# 5. Model selection poisson ---------------------------------------------------

## 5.1 Model assumptions -------------------------------------------------------

### 1. The distribution is discrete with a single parameter, the mean, which is usually symbolized as either (lambda) or (mu). The mean is also understood as a rate parameter. It is the expected number of times that an item or event occurs per unit of time, area, or volume.

### 2. The response terms, or y values, are nonnegative integers; i.e., the distribution allows for the possibility of counts where Y >= 0.

### 3.  Observations are independent of one another.

### 4.  No cell of observed counts has substantially more or less than what is expected based on the mean of the empirical distribution. For example, the data should not have more zero counts than is expected based on a Poisson distribution with a given mean. As the value of increases, the probability of zero (0) counts is reduced.

# Poisson model without random effects
SpRich_poisson_norandom <- glm(formula = N_species ~ Status + year + Status:year, 
                               data = event_sp_df, na.action = na.exclude, 
                               family = "poisson")

obspred_pois <- COUNT::poi.obs.pred(14,SpRich_poisson_norandom)

# Reshaping the dataframe
obspred_pois <- reshape2::melt(obspred_pois, id.vars = "Count")
obspred_pois <- obspred_pois %>% filter(variable %in% c("propObsv","propPred"))

# Both observed and predicted on the same plot on the same plot
ggplot(obspred_pois,aes(Count, value,col = variable)) + geom_point() + stat_smooth()

### 5. The mean and variance of the model are identical, or at least nearly the same; i.e., Poisson distributions with higher mean values have correspondingly greater variability

# Variance is about 2xmean. 

### 6. The Pearson Chi2 dispersion statistic has a value approximating 1.0. A value of 1.0 results when the observed and predicted variances of the response are the same.

## Fit full model: Poisson model with random effects
SpRich_poisson <- lme4::glmer(formula = N_species ~ Status + year + Status:year + (1|locality), 
                              data = event_sp_df, 
                              na.action = na.exclude, family = "poisson")

# Method that works for the glmer model
# get Pearson Chi2
pr <- sum(residuals(SpRich_poisson, type="pearson")^2) 
pr # 56.50366
# get p-value for the Pearson Chi2
pchisq(pr, df.residual(SpRich_poisson), lower=F) # calc p-value
# 0.4185681
 # Not using this measure to consider fit, but rather the Deviance goodness-of-fit.

# get the dispersion statistic (Pearson Chi2 statistic divided by the residual degrees of freedom)
pr/df.residual(SpRich_poisson) # 1.027339
# Dispersion statistic close to 1 is consistent with poisson assumption

## 5.2 Random effects ----------------------------------------------------------

# Model without random effects
MuMIn::AICc(SpRich_poisson_norandom) # 335.0488

# Model with random effects
MuMIn::AICc(SpRich_poisson) # 297.1478

# Adding random effects seem to improve model fit.

## 5.3 Fit candidate models ----------------------------------------------------

# Full model
SpRich_poisson_full <- lme4::glmer(formula = N_species ~ Status + year + Status:year + (1|locality),
                                   data = event_sp_df, 
                                   na.action = na.fail, family = "poisson") # altered na action

# Use dredge to get all candidate models
SpRich_poisson_candmodels <- MuMIn::dredge(SpRich_poisson_full, rank = "AICc")

print(SpRich_poisson_candmodels, abbrev.names = FALSE)
#   (Intercept) Status year Status:year df   logLik  AICc delta weight
#2       1.926      +                   3 -143.185 292.8  0.00  0.385
#1       1.582                          2 -144.511 293.2  0.43  0.310
#4       1.950      +    +              4 -143.029 294.8  1.99  0.142
#3       1.607           +              3 -144.355 295.1  2.34  0.119
#8       1.955      +    +           +  5 -143.018 297.1  4.35  0.044

# Strongest support for the model with status

# AICc: However, all four models seen above are within delta AICc of 2 and should in theory be considered.
# Interaction: None of the models with support include an interaction between status and year.

# Evidence ratio (weight best/weight comparison model): How many more times likely one model is than another
0.385/0.310 # 1.241935, model 1 ca 1.2 x more likely than model 2
0.385/0.142 # 2.711268, model 1 ca 2.7 x more likely than model 3
# ER ≈ 1 means models are about equally supported. So models 1-3 are about equally supported
# ER 2-3 means weak evidence supporting one model
# ER 3-10 moderate evidence
# > 10 strong evidence

m0 <- lme4::glmer(formula = N_species ~ 1 + (1|locality), 
                  data = event_sp_df, 
                  na.action = na.fail, family = "poisson") # altered na action

lme4::VarCorr(m0)

m0_glm <- glm(N_species ~ 1, data = event_sp_df, family = poisson)

MuMIn::AICc(m0)
MuMIn::AICc(m0_glm)
# random effects matter

# What about R2?
MuMIn::r.squaredGLMM(m0)
# As marginal R2 is close to zero and conditional R2 is higher, it indicates that the random effects explain much of the variation.

# I now look at the two highest ranked models, assess the trend. 


# 5.4 Refit models with delta AICc < 2 -------------------------------------------

## Best model: status as explanatory variable
SpRich_poisson_2 <- lme4::glmer(formula = N_species ~ Status + (1|locality), 
                                data = event_sp_df, 
                                na.action = na.exclude, 
                                family = "poisson")

## Second best model: intercept only
SpRich_poisson_1 <- lme4::glmer(formula = N_species ~ 1 + (1|locality), 
                                data = event_sp_df, 
                                na.action = na.exclude, 
                                family = "poisson")

## Third best model
SpRich_poisson_4 <- lme4::glmer(formula = N_species ~ Status + year + (1|locality), 
                                data = event_sp_df, 
                                na.action = na.exclude, 
                                family = "poisson")

## Fourth best model
SpRich_poisson_3 <- lme4::glmer(formula = N_species ~ year + (1|locality), 
                                data = event_sp_df, 
                                na.action = na.exclude, 
                                family = "poisson")


# 6. Investigate supported models ----------------------------------------------

# 6.1 Model 2: N_species ~ Status + (1|locality) -------------------------------

# Model summary
summary(SpRich_poisson_2) 

# Profile confidence intervals
lme4::confint.merMod(SpRich_poisson_2, method = "profile")
#                      2.5 %    97.5 %
# (Intercept)      1.299201 2.5474723
# Statusregulated -1.600112 0.1854730

# Calculate conditional vs marginal R2
performance::r2(SpRich_poisson_2)
# R2 for Mixed Models
# Conditional R2: 0.663 # variance explained by fixed + random
# Marginal R2: 0.251 # variance explained by fixed effects

# Diagnostic plots
plot(SpRich_poisson_2, Status ~ resid(.), abline = 0 )
plot(SpRich_poisson_2, locality ~ resid(.), abline = 0 )

# Confidence intervals
confint(SpRich_poisson_2)
# Statusregulated -1.3569138 0.3000763
lme4::confint.merMod(SpRich_poisson_2, method = "profile")
lme4::confint.merMod(SpRich_poisson_2, method = "boot", nsim = 1000)

# Effect plot
plot(effects::allEffects(SpRich_poisson_2))

# Extract estimated means
emm_status <- emmeans::emmeans(SpRich_poisson_2, ~ Status, type = "response") 
emm_status_df <- as.data.frame(emm_status)
head(emm_status_df)

# Calculating percentage change in expected abundance
# loge richness controls 1.93
# loge richness regulated: 1.24
1.93-0.69

((1.24-1.93)/1.93)*100 # -35.7513

# 6.2 Model 1: intercept -------------------------------------------------------

summary(SpRich_poisson_1)
lme4::confint.merMod(SpRich_poisson_1, method = "profile")

# 6.3 Model 4: N_species ~ Status + year + (1|locality) ------------------------

# Model summary
summary(SpRich_poisson_4)
lme4::confint.merMod(SpRich_poisson_4, method = "profile")

# 6.4 Model 3: N_species ~ year + (1|locality) ---------------------------------

# Model summary
summary(SpRich_poisson_3)
lme4::confint.merMod(SpRich_poisson_3, method = "profile")

summary(SpRich_poisson_3) 
# year2023      0.1168
confint(SpRich_poisson_3)
#                  2.5 %    97.5 %
# year2023    -0.1003707 0.3379277

# Weak support for any difference in species richness between years.
