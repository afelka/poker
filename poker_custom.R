library(readxl)
library(dplyr)
library(ggplot2)
library(ggimage)


poker <- read_xlsx("poker.xlsx")

for (i in unique(poker$player)) {

selected_player <- i

rounds_selected_player <- poker %>% filter(player == i) %>% select(round) %>% unique()


filtered_data <- poker %>% filter(round %in% rounds_selected_player$round) %>%
  mutate(new_round = dense_rank(round)) %>%
  mutate(card_type = case_when(
    new_round %% 4 == 1 ~ "spades",
    new_round %% 4 == 2 ~ "hearts",
    new_round %% 4 == 3 ~ "clubs",
    new_round %% 4 == 0 ~ "diamonds"
  ),
  card_to_use = case_when(
    player == selected_player  ~  paste0(rank,"_of_",card_type,".png"),
    player != selected_player ~ 'back_of_cards.png')

  )

poker_plot <- ggplot(filtered_data, aes(y = rank, x = new_round)) +
  geom_image(aes(image = paste0('./images/',card_to_use)), size = 0.06) +
  labs(title = paste0("Poker Standings for ", selected_player)) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1, max(filtered_data$new_round), 1), position = "top",
                     labels = scales::ordinal_format()) +
  scale_y_reverse() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank() ,
        axis.ticks.y = element_blank() ,
        axis.ticks.x = element_blank() ,
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "white"),
        plot.title = element_text(size = 7 , color = "white"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.background = element_rect(fill = "#35654d", color = NA),
        plot.background = element_rect(fill = "#35654d", color = NA))

applied_width <- ifelse(max(filtered_data$new_round) > 20, 8,
                        ifelse(max(filtered_data$new_round) > 10, 6,
                               ifelse(max(filtered_data$new_round) >= 3, 4,2)))

#save ggplot
ggsave(paste0("poker_",selected_player,".png"), plot = poker_plot, width = applied_width, height = 4, dpi = 300)

}
