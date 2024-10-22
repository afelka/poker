#load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(ggimage)

#read the file (poker.xlsx is not available in repo you can test with example.xlsx instead)
poker <- read_xlsx("poker.xlsx")

#create plot and save for each player
for (i in unique(poker$player)) {

selected_player <- i

#find the rounds selected player played in
rounds_selected_player <- poker %>% filter(player == i) %>% select(round) %>% unique()

#filter for selected player
filtered_data <- poker %>% filter(round %in% rounds_selected_player$round) %>%
#create selected_player_game_no to show game number for selected player
  mutate(selected_player_game_no = dense_rank(round)) %>%
#to create variety change card type
  mutate(card_type = case_when(
    selected_player_game_no %% 4 == 1 ~ "spades",
    selected_player_game_no %% 4 == 2 ~ "hearts",
    selected_player_game_no %% 4 == 3 ~ "clubs",
    selected_player_game_no %% 4 == 0 ~ "diamonds"
  ),
#image of card to use
  card_to_use = case_when(
    player == selected_player  ~  paste0(rank,"_of_",card_type,".png"),
    player != selected_player ~ 'back_of_cards.png')

  )

#create ggplot
poker_plot <- ggplot(filtered_data, aes(y = rank, x = selected_player_game_no)) +
  geom_image(aes(image = paste0('./images/',card_to_use)), size = 0.06) +
  labs(title = paste0("Poker Standings for ", selected_player)) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1, max(filtered_data$selected_player_game_no), 1), position = "top",
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

#depending on how many times selected player played, change the width of saved png
applied_width <- ifelse(max(filtered_data$selected_player_game_no) > 20, 8,
                 ifelse(max(filtered_data$selected_player_game_no) > 10, 6,
                 ifelse(max(filtered_data$selected_player_game_no) >= 3, 4,2)))

#save ggplot
ggsave(paste0("poker_",selected_player,".png"), plot = poker_plot, width = applied_width, height = 4, dpi = 300)

}
