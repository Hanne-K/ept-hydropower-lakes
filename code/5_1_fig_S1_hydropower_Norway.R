##############################################

# 5.1 Fig S1 Hydropower reservoirs Norway

##############################################

# Create dataframe -------------------------------------------------------------

# Spatial dataframe with info on all reservoirs in Norway
magasin_sf <- magasin_sf %>% 
  dplyr::filter(status == "D") %>% # Only keep reservoirs that are in operation, excluding planned ones etc.
  dplyr::filter(magasinFormal_Liste == "Kraftproduksjon") # only keep those used for hydropower

# Change to projected coordinates
magasin_sf_P <- sf::st_transform(magasin_sf, 32633)
# Change format
magasin_sf_P$lavesteRegulerteVannstand_moh <- as.numeric(magasin_sf_P$lavesteRegulerteVannstand_moh)
magasin_sf_P$hoyesteRegulerteVannstand_moh <- as.numeric(magasin_sf_P$hoyesteRegulerteVannstand_moh)

# Create amplitude variable
magasin_sf_P <- magasin_sf_P %>%
  dplyr::mutate(amplitude = hoyesteRegulerteVannstand_moh-lavesteRegulerteVannstand_moh) %>%
  dplyr::filter(!is.na(amplitude)) # 1114 observations

# How many are missing amplitude data?
1464-1114 # 350 hydropower reservoirs

# Data exploration -------------------------------------------------------------

# Stats
mean(magasin_sf_P$amplitude) # 13.88884
median(magasin_sf_P$amplitude) # 7
sd(magasin_sf_P$amplitude) # 17.77886
max(magasin_sf_P$amplitude) # 140
min(magasin_sf_P$amplitude) # 0
# there are 10 reservoirs with amp = 0, I choose to keep them for calculations

## Explorative plot ----

# Barplot
ggplot(magasin_sf_P, aes(x = amplitude))+
  geom_histogram(bins = 141) +
  geom_vline(xintercept = 4, color = "red")+
  geom_vline(xintercept = 5.9, color = "red")+
  labs(x = "Amplitude (m)", y = "Count")+
  scale_x_continuous(limits = c(-1,150))+
  theme_classic()

# How many hydropower reservoirs have an amplitude of 4-6 meters?
amp_4_6 <- magasin_sf_P %>%
  dplyr::filter(amplitude >= 4 & amplitude <=6)

length(amp_4_6$amplitude) 
# 162 reservoirs with amplitude between 4-6 meters

# Look at the lakes, they are spread throughout Norway
#mapview(amp_4_6)

# How many percent is this of all hydropower reservoirs we have amplitude
# data from?
(162/1114)*100 # 14.54219

# How many percent are below 4?
amp_0_4 <- magasin_sf_P %>%
  dplyr::filter(amplitude >= 0 & amplitude <4) # 371 observations

(371/1114)*100 # 33.30341

#mapview(amp_0_4)

# How many are above 6 meters?
amp_6 <- magasin_sf_P %>%
  dplyr::filter(amplitude > 6) # 581 observations

(581/1114)*100 # 52.1544

#mapview(amp_6)

# Between 4 and 8 meters?
amp_4_8 <- magasin_sf_P %>%
  dplyr::filter(amplitude >= 4 & amplitude <=8)

#mapview(amp_4_8)

# Figure -----------------------------------------------------------------------

# Make a summary dataframe, want to count the number of lakes with 0,1,2 etc
# meter amplitude

magasin_sf_P <- magasin_sf_P %>%
  dplyr::mutate(amp_category = case_when(amplitude < 4 ~ "0-4 m",
                                         amplitude >= 4 & amplitude <=6 ~ "4-6 m",
                                         amplitude > 6 ~ "> 6 m"
  ),
  amp_detailed = case_when(amplitude < 4 ~ "0-3 m",
                           amplitude >= 4 & amplitude <=6 ~ "4-6 m",
                           amplitude > 6 & amplitude <= 10 ~ "7-10 m",
                           amplitude > 10 & amplitude <= 20 ~ "11-20 m",
                           amplitude > 20 ~ "21-140 m"
  ))

magasin_sf_P$amp_category <- factor(magasin_sf_P$amp_category,
                                    levels = c("0-4 m", "4-6 m", "> 6 m"))

hydropower_plot <- ggplot(magasin_sf_P, aes(x = amplitude, fill = amp_category))+
  geom_histogram(bins = 150) +
  scale_fill_manual(values = c("lightgrey","black","lightgrey"))+
  labs(x = "Amplitude (m)", y = "Count")+
  scale_x_continuous(limits = c(0,145))+
  theme_classic()+
  theme(legend.position = "none")


jpeg(here::here("results","figures","fig_S1_hydropower_Norway.jpg"), width = 15, height = 8, units="cm", res=300)

hydropower_plot

dev.off()