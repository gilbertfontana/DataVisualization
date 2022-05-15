

# Lib
library(tidyverse)
library(ggridges)
library(showtext)
library(MetBrewer)

# Data
df <- read_csv("https://raw.githubusercontent.com/the-pudding/data/master/foundation-names/allCategories.csv")

common_brands <- df %>% 
  group_by(brand) %>% 
  count(sort=T) %>% 
  ungroup() %>% 
  slice_head(n=20) %>% 
  select(brand)

# Misc
font <- "Yanone Kaffeesatz"
font_add_google(family=font, font)
bg <- "#EEB0B0"
txt_col <- "grey15"
bus <- "#FFB319"
subway <- "#911F27"
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE) 

# Plot
df %>% 
  filter(brand %in% common_brands$brand) %>%
  mutate(brand = fct_reorder(str_to_title(brand), lightness, .fun='median')) %>% 
  ggplot(aes(x=lightness, y=brand,fill=stat(x))) +
  geom_density_ridges_gradient(color="grey15", size=.5) +
  scale_x_continuous(breaks = c(0.1,1), labels = c("Dark", "Light"), expand = c(0,0)) +
  scale_fill_gradientn(colors = rev(met.brewer("Archambault"))) +
  #scale_fill_viridis_c(option = "inferno") +
  labs(
    title = "Foundation shades",
    subtitle = "Variation in shade intensity for the 20 most common foundation brands",
    caption = "Gilbert Fontana | Data: Sephora and Ulta, via The Pudding"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color=txt_col, size=12),
    plot.title = element_text(hjust=0, size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0, size=16, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=10, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
  )


# Save
showtext_opts(dpi = 320) 

ggsave("Foundation.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)



