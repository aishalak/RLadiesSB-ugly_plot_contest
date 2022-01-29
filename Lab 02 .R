library(tidyverse)

sweaters <- read_csv(here::here("data/use_this_data", "holiday_sweaters-2020-12-15-clean.csv")) %>% 
  filter(hs_tf == "Yes") %>% 
  # potentially useful function: separate colors column into rows
  separate_rows(colors, sep = c(", ")) %>% 
  group_by(sweater) %>% 
  mutate(sum = length(colors)) %>% 
  rownames_to_column("values")

ggplot(sweaters, aes(x = sweater, y = sum)) +
  geom_point(aes(color = colors)) + 
  geom_text(label=rownames(sweaters), nudge_x = 0.25, nudge_y=0.25, size = 13, color = "yellow") + 
  labs(x = "ENTRy", 
       y = "Total number of colorsSSZZ") +
  theme(plot.background = element_rect(fill = "wheat4"),
        legend.background = element_rect(fill = "brown"), legend.key = element_rect("firebrick4"), 
        legend.text =element_text(size=6, color = "tan4"), 
        legend.title = element_text(family = "serif", color = "tomato3", size = 20), 
        panel.grid.major = element_line(size = 1, linetype = 'solid',
                                        color = "green"), panel.grid.minor = element_line(size = 10, linetype = 'solid',
                                                                                          color = "blue"))
ggsave(here::here("figures/an-plot.jpg"), width = 7, height = 4, dpi = 150)
                     


                     