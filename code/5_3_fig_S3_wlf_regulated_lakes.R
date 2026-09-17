##############################################

# 5.3 Fig S3 WLF regulated lakes

##############################################

# Use tidy data
#load(file = here::here("data","derived_data","waterlevels","stordr_wl.rda"))
#load(file = here::here("data","derived_data","waterlevels","gjolja_wl.rda"))

# Investigate data -------------------------------------------------------------

# Stor-Drakstsjoen ----

# Look for outliers that are likely measurement errors
# Look at whether some years should be excluded
ggplot(stordr_wl, aes(x = Date, y = Waterlevel_m)) +
  geom_point()
# No clear outliers, datapoints are likely valid measurements
# Looks like a continuous yearly measurement

# Start and stop year
max(stordr_wl$year) # 2024
min(stordr_wl$year) # 1923

# Create summary: yearly max, min and max amplitude
stordr_summary <- stordr_wl %>% # remove NAs
  drop_na()

stordr_summary <- stordr_summary %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(max_wl = max(Waterlevel_m),
                   min_wl = min(Waterlevel_m),
                   max_amp = max_wl-min_wl) 

# Mean yearly max amplitude overall (1923-2024)
mean(stordr_summary$max_amp) # 3.179614
median(stordr_summary$max_amp) # 3.44

# Mean for the last 20 years
stordr_20years <- stordr_summary %>% 
  filter(year >=2004 & year <= 2024)
mean(stordr_20years$max_amp) # 3.10479
median(stordr_20years$max_amp) # 3.07

# Mean for the last 10 years
stordr_10years <- stordr_summary %>% 
  filter(year >=2014 & year <= 2024)
mean(stordr_10years$max_amp) # 2.802855
median(stordr_10years$max_amp) # 3.068

# Plot
ggplot(stordr_summary, aes(x = year, y = max_amp)) +
  geom_point() 


# Gjoljavatnet ----

# Look for outliers that are likely measurement errors
# Look at whether some years should be excluded
ggplot(gjolja_wl, aes(x = Date, y = Waterlevel_m)) +
  geom_point()
# ca 10 outliers present between 2010 and 2020, will remove these
# gap in measurements

# Start and stop year
max(gjolja_wl$year) # 2022
min(gjolja_wl$year) # 1950

# Gaps and issues
# 1950-2005 looks good, some small data gaps, but less than 1 year and should not be a problem for yearly amp calculation
# 31.12.2005-01.01.2009 gap 
# 01.01.2009-12.01.2010 KEEP (1 year ish)
# 12.01.2010-28.12.2010 gap 
# 28.12.2010-01.09.2019 KEEP
# onwards, the last few measurements do not cover a whole year REMOVE

# Removing outliers
gjolja_wl <- gjolja_wl %>%
  dplyr::filter(Waterlevel_m < 1 & Waterlevel_m > -5)

# Removing incomplete years
gjolja_wl <- gjolja_wl %>%
  dplyr::filter(year < 2020)

# Investigate a subset
ggplot(subset(gjolja_wl, year >2009 & year < 2011), aes(x = Date, y = Waterlevel_m)) +
  geom_point()

# Summary
gjolja_summary <- gjolja_wl %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(max_wl = max(Waterlevel_m),
                   min_wl = min(Waterlevel_m),
                   max_amp = max_wl-min_wl) 

# Mean yearly max amplitude (data 1950-2005, 2009-2010,2010-2019)
mean(gjolja_summary$max_amp) # 3.285075
median(gjolja_summary$max_amp) # 3.35

# Mean yearly max amplitude, last 10 years of data
gjolja_10years <- gjolja_summary %>% 
  filter(year >= 2010 & year <= 2019)
mean(gjolja_10years$max_amp) # 2.728
median(gjolja_10years$max_amp) # 2.72

# Figure -----------------------------------------------------------------------

# Gjøljavatnet
gjolja_cleaned_df <- gjolja_wl

p1 <- ggplot(gjolja_cleaned_df, aes(x = Date, y = Waterlevel_m)) +
  geom_line() +
  labs(y = "Waterlevel (m)")+
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")+
  geom_hline(yintercept = -4, color = "red", linetype = "dashed")+
  theme_classic()

# Stor-Drakstsjøen
stordr_cleaned_df <- stordr_wl

p2 <- ggplot(stordr_cleaned_df, aes(x = Date, y = Waterlevel_m)) +
  geom_line() +
  labs(y = "Waterlevel (m.a.s.l.)")+
  geom_hline(yintercept = 265.5, color = "red", linetype = "dashed")+
  geom_hline(yintercept = 260.5, color = "red", linetype = "dashed")+
  theme_classic()

# Combine

jpeg(here::here("results","figures","fig_S3_wlf_regulated_lakes.jpg"),
     width = 26, height = 20, units="cm", res=300)

combined_plot <- ggpubr::ggarrange(p1,p2,ncol = 1, nrow = 2, 
                                   labels = c("Gjøljavatnet","Stor-Drakstsjøen"),
                                   label.x = c(0.01,0.01), label.y = c(1,1))
combined_plot

dev.off()
