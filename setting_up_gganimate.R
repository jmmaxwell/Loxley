library(ggplot2)
library(cowplot)
library(gapminder)
library(gganimate)
library(knitr)
library(dplyr)
install.packages('cowplot')
install.packages('ggplot2')

theme_set(theme_bw())

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()

gganimate(p)

100 %/% 3

animate_data %>%
  mutate(trial = c(rep(1:10, each = n() %/% 10), rep(10, n() %% 10))) -> animate_data

animate_data %>%
  mutate(trial_ct = c(1:n())) %>%
  mutate(trial = c(rep(1:20, each = n() %/% 20), rep(20, n() %% 20))) %>%
  group_by(arm_choice, has_style, prior_demand) %>%
  mutate(index = c(1:n())) %>%
  mutate(total_demand = cumsum(demand)) %>%
  mutate(prop = total_demand/index) %>%
  filter(index > 50) -> agg_data
 
p4 <- ggplot(animate_data, aes(x = has_style, y = demand, group = has_style, color = factor(arm_choice), frame = trial)) +
  geom_point(aes(cumulative = TRUE, group = arm_choice)) + geom_jitter() + facet_wrap(~prior_demand)

p4

gganimate(p4)

p5 <- ggplot(agg_data, aes(y = prop, x = trial_ct, group = factor(arm_choice), color = factor(arm_choice), frame = trial)) +
  geom_point(aes(cumulative = T)) +
  geom_path(aes(cumulative = T)) +
  facet_wrap(~has_style + prior_demand)

gganimate(p5)
