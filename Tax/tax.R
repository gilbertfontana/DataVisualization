
# Lib
library(tidyverse)
library(janitor)
library(ggrepel)
library(showtext)
library(MetBrewer)

# Data
df <- readxl::read_excel("") %>% 
clean_names() %>% 
  select(country, year, centaxgdp)

df2 <- df %>% 
  filter(country %in% c("Sweden","Norway","Finland","Denmark")) %>% 
  na.omit()

df3 <- readxl::read_excel("",
                          range = ""
                          ) %>%
  pivot_longer(!`GEO/TIME`, names_to = "year", values_to = "centaxgdp") %>% 
  clean_names() %>% 
  rename(country=geo_time) %>% 
  mutate(year=as.numeric(year))

df4 <- bind_rows(df2,df3)


# Misc
font <- "Oswald"
font_add_google(family=font, font)
bg <- "#334756"

# Plot
df4 %>% 
  ggplot(aes(x=year, y=centaxgdp, group=country)) +
  geom_line(aes(color=country)) +
  geom_point(data=df4 %>% filter(year=="2020"),
             aes(color=country)
  ) +
  geom_text_repel(data=df4 %>% filter(year=="2020"),
                  aes(label=country, color=country),
                  hjust=-.4,
                  min.segment.length = Inf,
                  family=font) +
  scale_color_manual(values = met.brewer("Paquin",type = "discrete",n=4)) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  xlab("") +
  ylab("Central government tax revenue as a share of GDP (%)") +
  labs(title = "The Rise of the\nNordic Welfare States",
       caption ="Gilbert Fontana | Data: Andersson & Thomas Brambor (2019) and Eurostat"
  ) +
  coord_cartesian(clip = 'off') +
  theme_minimal(base_family = font) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color="#DEA2A2", size=10),
    axis.title = element_text(color="#DEA2A2", size=12),
    plot.title = element_text(size=36, hjust=.5, color="#DEA2A2",face = "bold", margin = margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, color="#DEA2A2"),
    legend.position = "none",
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(30, 30, 30, 30)
  )

# Save
showtext_auto(enable = TRUE) 
showtext_opts(dpi = 320) 

ggsave("tax.png",
       height = 8,
       width = 10,
       dpi=320,
       
)  

showtext_auto(FALSE)

  

