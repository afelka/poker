library(readxl)
library(dplyr)
library(ggplot2)
library(ggimage)


poker <- read_xlsx("poker.xlsx") %>% filter(erdem_in > 0) %>%
              mutate(card_type = case_when(
                erdem_in %% 4 == 1 ~ "spades",
                erdem_in %% 4 == 2 ~ "hearts",
                erdem_in %% 4 == 3 ~ "clubs",
                erdem_in %% 4 == 0 ~ "diamonds"
              ),
              card_to_use = case_when(
               player == "Erdem A"  ~  paste0(rank,"_of_",card_type,".png"),
               player != "Erdem A" ~ 'back_of_cards.png')

              )

poker_plot <- ggplot(poker, aes(y = rank, x = erdem_in)) +
  geom_image(aes(image = paste0('./images/',card_to_use)), size = 0.06) +
  labs(title = "Poker Standings for Erdem") +
  theme_classic() +
  scale_x_continuous(breaks = seq(1, max(poker$erdem_in), 1), position = "top",
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


#save ggplot
ggsave("poker.png", plot = poker_plot, width = 6, height = 4, dpi = 300)
