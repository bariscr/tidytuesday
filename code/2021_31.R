library(tidyverse)
library(RColorBrewer)
library(ggrepel)
library(ggdark)

tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics

olympics2 <- 
  olympics %>% 
  filter(medal == "Gold" & sex == "F", year > 2000) %>% 
  distinct(name) %>% 
  mutate(first_name = word(name)) %>% 
  select(first_name) 

first_name <- olympics2 %>% count(first_name, sort=TRUE)

wordcloud::wordcloud(olympics2$first_name, max.words = 100,
                     random.order=FALSE, rot.per=0.35,            
                     colors=brewer.pal(8, "Dark2"))

# Median age
olympics %>% 
  mutate(medal = replace_na(medal, "No medal"),
         medal = fct_relevel(medal, "Gold",
                             "Silver",
                             "Bronze",
                             "No medal")) %>% 
  distinct(name, medal, sex, age, year) %>% 
  group_by(sex, medal, year) %>% 
  summarise(median_age = median(age, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = median_age, color = sex)) + 
  geom_line() +
  scale_colour_discrete(labels = c("Female", "Male")) +
  #  geom_label_repel(aes(label = ifelse(year == min(year), 
  #                                     median_age, NA))) +
  ylim(0, 30) +
  dark_theme_classic() +
  theme(legend.title = element_blank()) +
  labs(x = "Year",
       y = "Median Age",
       title = "Median ages of olympic athletes by sex and medal won") +
  facet_wrap(~ medal)

# Mean height
olympics %>% 
  mutate(medal = replace_na(medal, "No medal"),
         medal = fct_relevel(medal, "Gold",
                             "Silver",
                             "Bronze",
                             "No medal")) %>% 
  distinct(name, medal, sex, height, year) %>% 
  group_by(sex, medal, year) %>% 
  summarise(mean_height = mean(height, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_height, color = sex)) + 
  geom_line() +
  scale_colour_discrete(labels = c("Female", "Male")) +
  #  geom_label_repel(aes(label = ifelse(year == min(year), 
  #                                     median_age, NA))) +
  ylim(100, 200) +
  dark_theme_classic() +
  theme(legend.title = element_blank()) +
  labs(x = "Year",
       y = "Mean Height",
       title = "Mean heights of olympic athletes by sex and medal won") +
  facet_wrap(~ medal)

# Mean height
# New facet label names for supp variable
sex.labs <- c("Female", "Male")
names(sex.labs) <- c("F", "M")

olympics %>% 
  mutate(medal = replace_na(medal, "No medal"),
         medal = fct_relevel(medal, "Gold",
                             "Silver",
                             "Bronze",
                             "No medal"),
         any_medal = ifelse(medal == "No medal", "No medal", "Won medal")) %>% 
  distinct(name, any_medal, sex, height, year, season) %>% 
  group_by(sex, any_medal, year, season) %>% 
  summarise(mean_height = mean(height, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_height, color = any_medal)) + 
  geom_line() +
  #  geom_label_repel(aes(label = ifelse(year == min(year), 
  #                                     median_age, NA))) +
  ylim(120, 190) +
  dark_theme_classic() +
  theme(legend.title = element_blank()) +
  labs(x = "Year",
       y = "Mean Height") +
  ggtitle("Mean heights of olympic athletes \nby season, sex and medal won") +
  facet_grid(sex ~ season,
             labeller = labeller(sex = sex.labs))

# Oldest gold winners 
# Median age
olympics %>% 
  filter(medal == "Gold") %>% 
  distinct(name, sex, age, year, season) %>% 
  group_by(sex, year, season) %>% 
  summarise(max_age = max(age, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = max_age, color = sex)) + 
  geom_line() +
  scale_colour_discrete(labels = c("Female", "Male")) +
  ylim(0, 70) +
  dark_theme_classic() +
  theme(legend.title = element_blank()) +
  labs(x = "Year",
       y = "Maximum Age") +
  ggtitle("Oldest gold winner olympic athletes \nby sex and season") +
  facet_wrap(~ season)

# Mean height
olympics %>% 
  mutate(medal = replace_na(medal, "No medal"),
         medal = fct_relevel(medal, "Gold",
                             "Silver",
                             "Bronze",
                             "No medal")) %>% 
  distinct(name, medal, sex, height, year) %>% 
  group_by(sex, medal, year) %>% 
  summarise(mean_height = mean(height, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_height, color = sex)) + 
  geom_line() +
  scale_colour_discrete(labels = c("Female", "Male")) +
  #  geom_label_repel(aes(label = ifelse(year == min(year), 
  #                                     median_age, NA))) +
  ylim(100, 200) +
  dark_theme_classic() +
  theme(legend.title = element_blank()) +
  labs(x = "Year",
       y = "Mean Height",
       title = "Mean heights of olympic athletes by sex and medal won") +
  facet_wrap(~ medal)