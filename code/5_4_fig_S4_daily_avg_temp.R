##############################################

# 5.4 Fig S4 daily average water temperature

##############################################

# Make year a factor
temperature_data_v3$year <- as.factor(temperature_data_v3$year)
temperature_data_v3$locality <- factor(temperature_data_v3$locality, 
                                       levels = c("Barsetvatnet","Roksetvatnet","Kilvatnet",
                                                  "Gjoljavatnet","Storvatnet","Stor-Drakstsjoen"))

# Figure: Daily mean temperature
jpeg(here::here("results","figures","fig_S4_daily_avg_temp.jpg"),
     width = 20, height = 20, units="cm", res=300)

fig_temperature <- ggplot(temperature_data_v3,
                          aes(x = as.Date(paste(2025, strftime(eventDate, "%m-%d"), sep = "-")),
                              y = daily_avg_temp,
                              color = year,
                              shape = year)) +
  geom_point(size = 1) +
  geom_line() +
  scale_color_manual(name = "Year",
                     values = c("2023" = "black", "2024" = "grey")) +
  scale_shape_manual(name = "Year",
                     values = c("2023" = 16, "2024" = 17)) +
  
  labs(x = "Date", y = "Temperature (\u00B0C)") +
  theme_bw() +
  theme(legend.position = "bottom")+
  facet_wrap(~ locality, nrow = 3)

fig_temperature

dev.off()
