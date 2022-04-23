
# Libraries
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(extrafont)
loadfonts(device = "win")

# Data
df <- read.table('https://raw.githubusercontent.com/gilbertfontana/DataVisualization/main/Temp/uppsala_tm_1722-2020.dat')

# Data cleaning
df2 <- df %>% 
  mutate(date = make_date(V1, V2, V3)) %>% 
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(mean_temp = mean(V4)) %>% 
  mutate(year=lubridate::year(month),
         month2=lubridate::month(month,label = TRUE,abbr = TRUE))

# Misc
font <- "Teko"
bg <- "#1E1A28"
text <- "#6B7E7F"

# Plot
ggplot(df2, aes(as.factor(month2), year), width=1, height=.9) +
  geom_tile(aes(fill = mean_temp)) +
  coord_polar(clip="off") +
  scale_fill_viridis_c(option="mako",
                       name="Average\ntemperature",
                       breaks=c(-15,22),
                       limits=c(-15,22)
                       ) +
  labs(title = "Average Monthly Temperatures\nBetween 1722 - 2020",
       caption = "Design: Gilbert Fontana | Data: Bergström, H., Moberg, A. (2002)"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family=font, color = text, size=12),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(family=font, color = text, size=30, hjust=.5, margin=margin(30,0,30,0)),
    plot.caption = element_text(family=font, color = text, size=10, hjust=.5, margin=margin(10,0,10,0)),
    panel.background = element_rect(fill = bg, colour = bg),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.position = "bottom",
    legend.background = element_rect(fill=bg,colour = bg),
    legend.title = element_text(family=font, size=14,color = text),
    legend.text = element_text(family=font, size=10,color = text)
    ) +
  guides(fill = guide_colourbar(ticks.colour = text, frame.colour = text, title.position="top", title.hjust=.5)
  )

# Save
ggsave("temp.png",
       height = 20,
       width = 20,
       units = "cm",
       type = "cairo",
       dpi=320
)  


