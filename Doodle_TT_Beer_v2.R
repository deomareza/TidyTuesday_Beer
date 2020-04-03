library(tidyverse)
library(skimr)
library(extrafont)

loadfonts()

options(scipen = "9999")

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')




####################


beer_taxed %>% 
  mutate(tax_status = as.factor(tax_status),
         type = as.factor(type),
         tax_rate = as.factor(tax_rate)) %>% 
  skim()


beer_label <- beer_states %>% 
  na.omit() %>% 
  mutate(state = factor(state),
         type = factor(type)) %>% 
  filter(state != "total" & type == "On Premises", barrels != 0) %>% 
  group_by(state) %>% 
  summarise(total_barrel = sum(barrels)) %>% 
  arrange(-total_barrel) %>% 
  distinct(state, total_barrel) %>% 
  mutate(id = seq(1,n())) 


label_data <- beer_label
number_of_bar <- nrow(label_data)
label_data$angle_calc <- 90-360 *(label_data$id) / number_of_bar

label_data$hjust <- ifelse(label_data$angle_calc < (-90), 0, 1)
label_data$angle <- ifelse(label_data$angle_calc < (-90), label_data$angle_calc + 180, label_data$angle_calc)


text_y = -1600000

gg_beer <- beer_states %>% 
  na.omit() %>% 
  
  mutate(state = factor(state),
         type = factor(type)) %>% 
  filter(state != "total" & type == "On Premises" & barrels != 0) %>% 
  # View()
  group_by(state) %>%
  summarise(total_barrel = sum(barrels)) %>% 
  
  
  # Start of GGplot
  ggplot(aes(reorder(state, -total_barrel), total_barrel)) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = .5, size = .1,color = "#F5D129") +
  geom_hline(yintercept = 1000000/2, linetype = "dashed", alpha = .5, size = .1,color = "#F5D129") +
  
  geom_col(aes(fill = total_barrel)) +
  
  geom_point(data = beer_label, aes(reorder(state, -total_barrel), -500000,
                                    color = "#F5E90B"), size = .2) +
  
  # State Code
  geom_text(data = label_data, aes(x = id, y = -95000,
                                   label = state, hjust=hjust), color = "#F5D129",
            size = 2.7, angle = label_data$angle, inherit.aes = F,
            family = "Poppins Light") +
  
  
  
  ylim(-1807783,1807900) +
  
  
  scale_fill_gradient(low = "#C74E1E", high = "#F5D129")+
  
  coord_polar() +
  
  
  # Title
  annotate(geom = "text", x = 0, y = text_y, label = "Beer Consumed", family = "Poppins SemiBold",
           color = "#F5D129", size = 4.7) +
  annotate(geom = "text", x = 0, y = text_y-10000, label = "per state", family = "Poppins Light",
           color = "#F5D129", size = 3) +
  
  # Extra theme settings
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#262626"),
        text = element_text(family = "Poppins ExtraLight"),
        legend.position = "none")


ggsave (file = "beer_export.svg", plot = gg_beer, width = 8, height = 8)


