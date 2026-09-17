##############################################

# 5.2 Fig S2 WLF control lakes

##############################################

# Make year a factor
GPS_waterlevels$year <- as.factor(GPS_waterlevels$year)

# Figure for each lake ---------------------------------------------------------
# Barsetvatnet
Barsetvatnet_wlf <- ggplot(subset(GPS_waterlevels, locality == "Barsetvatnet"))+ 
  geom_point(aes(x=as.Date(paste(2025,strftime(eventDate, format = "%m-%d"),sep = "-")),
                 y = waterlevel_masl, 
                 shape = year), size=1.5)+
  geom_line(aes(x=as.Date(paste(2025,strftime(eventDate, format = "%m-%d"),sep = "-")),
                y = waterlevel_masl,
                group = year))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  coord_cartesian(xlim = as.Date(c("2025-05-01","2025-09-01")))+
  geom_hline(yintercept = 61.2, color = "darkred", linetype = "dashed")+
  ylim(60,65)+
  labs(x = "Date", y = "Water level (m.a.s.l.)")+
  theme_classic()+
  theme(legend.position = "none") 

# Roksetvatnet
Roksetvatnet_wlf <- ggplot(subset(GPS_waterlevels, locality == "Roksetvatnet")) +
  geom_point(aes(x=as.Date(paste(2025,strftime(eventDate, format = "%m-%d"),sep = "-")),
                 y = waterlevel_masl, 
                 shape = year), size=1.5)+
  geom_line(aes(x=as.Date(paste(2025,strftime(eventDate, format = "%m-%d"),sep = "-")),
                y = waterlevel_masl, 
                group = year))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  coord_cartesian(xlim = as.Date(c("2025-05-01","2025-09-01")))+
  geom_hline(yintercept = 192.4, color = "darkred", linetype = "dashed")+
  ylim(190,195)+
  labs(x = "Date", y = "Water level (m.a.s.l.)")+
  theme_classic()+
  theme(legend.position = "none")

# Jonsvatnet
Jonsvatnet_wlf <- ggplot(subset(GPS_waterlevels, locality == "Kilvatnet"))+
  geom_point(aes(x=as.Date(paste(2025,strftime(eventDate, format = "%m-%d"),sep = "-")),
                 y = waterlevel_masl, 
                 shape= year), size=1.5)+
  geom_line(aes(x=as.Date(paste(2025,strftime(eventDate, format = "%m-%d"),sep = "-")),
                y = waterlevel_masl, 
                group = year))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  coord_cartesian(xlim = as.Date(c("2025-05-01","2025-09-01")))+
  geom_hline(yintercept = 149.3, color = "darkred", linetype = "dashed")+
  ylim(145,150)+
  labs(x = "Date", y = "Water level (m.a.s.l.)", shape = "Year")+
  theme_classic() +
  theme(legend.position = c(0.8,0.2))

# Arrange in the same plot -----------------------------------------------------

jpeg(here::here("results","figures","fig_S2_wlf_control_lakes.jpg"),
     width = 20, height = 10, units="cm", res=300)

fig_waterlevels_unreg <- ggarrange(Barsetvatnet_wlf,Roksetvatnet_wlf,Jonsvatnet_wlf,
                                   labels = c("Barsetvatnet","Roksetvatnet","Jonsvatnet"),
                                   ncol = 3, nrow = 1,
                                   label.x = c(0.1,0.1,0.1), label.y = c(1,1,1),
                                   common.legend = FALSE)

fig_waterlevels_unreg 

dev.off()