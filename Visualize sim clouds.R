


dat19_Ldt<- dat19 %>% mutate(dt = c(NA, diff(ESTtime) %>% as.numeric())) %>% filter(dt > 899)

sim_tracks19<- sim_tracks_df %>% filter(id == 19) %>% mutate(color = ifelse(time %in% dat19_Ldt$ESTtime, "blue", "red"))


ggplot(data = sf_sim_lines %>% filter(id == 19)) +
  geom_sf(aes(color = as.factor(id)), size = 0.25, alpha = 1, show.legend = "line") +
  geom_point(data = sim_tracks19, aes(mu.x, mu.y), color = sim_tracks19$color, alpha = 0.6, size = 1) +
  geom_point(data = dat19_Ldt, aes(utmlong, utmlat), color = "grey45", alpha = 0.6) +
  theme_bw() +
  scale_color_discrete("ID", guide = guide_legend(override.aes = list(size = 1))) +
  labs(x="Longitude", y="Latitude")
