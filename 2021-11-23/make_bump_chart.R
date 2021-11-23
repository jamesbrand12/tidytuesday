library(dplyr)
library(ggplot2)
#Download this week's data
tuesdata <- tidytuesdayR::tt_load('2021-11-23')

#Extract individual tibbles from the list
writers <- tuesdata$writers
directors <- tuesdata$directors
episodes <- tuesdata$episodes
imdb <- tuesdata$imdb


#Let's make a bump chart!

ep_rank <- episodes %>%
  filter(type == "episode", #We don't want to include the specials
         !is.na(rating),
         season_number < 13) %>% #season 13 not finished airing as of writing this
  #We want each season finale to align vertically, but each season has a different number of episodes
  #We'll set the max episode in each season to be 13, the global max
  group_by(season_number) %>%
  mutate(season_finale_episode = max(episode_number)) %>%
  ungroup() %>%
  mutate(global_max_episode = max(episode_number),
         episode_number_adj = case_when(episode_number == season_finale_episode ~ global_max_episode,
                                        TRUE ~ episode_number)) %>%
  group_by(episode_number_adj) %>%
  arrange(desc(rating), first_aired) %>%
  mutate(rank = row_number()) %>% #use row_number() instead of rank() because we don't want ties in our bump chart
  ungroup() %>%
  select(season_number, episode_number, episode_number_adj, rank, rating)

#Define a named vector which we will use later for our color scale
#I picked a few Doctor Who inspired color hex's
season_color_scale <- c("1" = "#060855",
                        "2" = "#FFA62B",
                        "3" = "gray60",
                        "4" = "#0455EB",
                        "5" = "gray60",
                        "6" = "gray60",
                        "7" = "gray60",
                        "8" = "gray60",
                        "9" = "gray60",
                        "10" = "gray60",
                        "11" = "gray60",
                        "12" = "gray60")

#We want to order the season number factor levels to control line overlap priority
#Also, add labels for premieres and finales
ep_rank <- ep_rank %>%
  mutate(season_factor = factor(season_number, levels = c(as.character(12:5), "3", "1", "4", "2")),
         label_left = case_when(episode_number_adj == 1 ~ paste("Season", season_number),
                                TRUE ~ as.character(NA)),
         label_right = case_when(episode_number_adj == 13 ~ paste("Season", season_number),
                                 TRUE ~ as.character(NA)))

#Now finally make our plot
ggplot(ep_rank, aes(x = episode_number_adj, y = rank, group = season_factor, color = season_factor)) +
  geom_line(size = 1.8, alpha = 0.9) +
  geom_point(size = 4) +
  geom_point(color = "white", size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:13, labels = c(as.character(1:12), "Finale"), expand = expansion(mult = 0.4)) +
  scale_y_reverse(breaks = 1:12) +
  scale_color_manual(values = season_color_scale) + #Here's the color scale we defined earlier
  ggrepel::geom_label_repel(aes(label = label_left, color = season_factor), nudge_x = -3.5, direction = "y", hjust = "left", fill = "white") +
  ggrepel::geom_label_repel(aes(label = label_right, color = season_factor), nudge_x = 3.5, direction = "y", hjust = "right", fill = "white") +
  xlab("Episode Number") +
  ylab("Season Rank of Episode Rating") +
  ggtitle(expression(paste("Ranks of ", italic("Doctor Who"), " Seasons by Episode Number")),
          subtitle = expression(paste("Season 1 had the lowest premiere rating, but the ", 2^"nd", " highest finale rating."))) +
  labs(caption = "Data: {datardis} | Plot: @rstats_james") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "gray95"),
        legend.position = "none",
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(family = "mono", size = 6),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))

#Save our plot
ggsave("Doctor_Who_Season_Ranks.png")
