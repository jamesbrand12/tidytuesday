#Load up the packages we'll need
library(afrilearndata)
library(afrihealthsites)
library(dplyr)
library(sf)
library(ggplot2)

#Get the hospital site data for all countries
all_afr <- afrihealthsites(unique(afcountries$iso3c)) %>%
  filter(amenity == "hospital")

#Convert raster into a dataframe
pop_df <- as.data.frame(afripop2020, xy = TRUE) %>%
  filter(!is.na(ppp_2020_1km_Aggregated))

#save the coordinate reference system of the hospital site data
my_crs <- st_crs(all_afr$geometry)
#initialize the distance column
pop_df$dist <- NA
#loop through each pixel from the population data
for(i in 1:nrow(pop_df)) {
  #convert pixel into an sf point
  pop_sf_row <- st_sf(st_sfc(st_point(c(pop_df$x[i], pop_df$y[i]))))
  #assign it the right crs (important for spherical distance math)
  st_crs(pop_sf_row) <- my_crs
  #find the distance to closest hospital
  pop_df$dist[i] <- min(st_distance(pop_sf_row, all_afr$geometry, by_element = TRUE))
  #print statement for monitoring progress
  if(i %% 1000 == 0) {
    print(i)
  }
}

#Distance is in meters, let's change to km
pop_df$dist_km <- pop_df$dist / 1000

#Plot time!
ggplot(data = pop_df, aes(x = x, y = y, fill = dist_km, color = dist_km)) +
  geom_raster() +
  coord_fixed() +
  theme_void() +
  scale_fill_gradientn(colors = rainbow(7),
                       aesthetics = c("fill", "color"),
                       labels = scales::label_comma(suffix = " km")) +
  ggtitle("Distance to Nearest Hospital", 
          subtitle = "Water isn't the only scarce resource in the Sahara") +
  labs(caption = "Data: {afrilearndata} and {afrihealthsites} | Plot: @rstats_james") +
  theme(legend.position = c(0.15, 0.3),
        legend.title = element_blank(),
        legend.text.align = 1,
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(family = "mono", size = 6),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))

#save our plot
ggsave("Africa_Hospital_Distance.png", height = 5, width = 7.84, dpi = 320)

