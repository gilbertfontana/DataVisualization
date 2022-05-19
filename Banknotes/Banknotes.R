
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)





df <- read_csv("https://raw.githubusercontent.com/the-pudding/banknotes/master/src/data/banknotesData.csv")
df2 <- read_csv("https://raw.githubusercontent.com/gilbertfontana/DataVisualization/main/Banknotes/gii_data.csv",
                    skip = 5) %>% 
  clean_names() %>% 
  select(country, x2019) %>% 
  rename(value=x2019) %>% 
  mutate(value=as.numeric(value)) %>% 
  na.omit()

countrycode <- countrycode::codelist %>% 
  select(country.name.en, un.region.name) %>% 
  rename(country=country.name.en,
         region=un.region.name)

share_women <- df %>% 
  group_by(country) %>% 
  count(gender) %>% 
  mutate(share_women = round(n/sum(n)*100)) %>% 
  filter(gender=="F") %>% 
  select(country,share_women) %>% 
  ungroup()

df3 <- df %>% 
  distinct(country) %>% 
  left_join(
    share_women
  ) %>% 
  left_join(
    df2
  ) %>% 
  left_join(
    countrycode
  ) %>% 
  mutate(share_women = ifelse(is.na(share_women), 0, share_women)) %>% 
  na.omit()


# Misc
font <- "Syne"
font_add_google(family=font, font)
bg <- "#A45D5D"
txt_col <- "grey10"
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE) 


# Plot
df3 %>% 
  ggplot(aes(x=value*100, y=share_women)) +
  geom_smooth(method = "lm", se = FALSE, color="black", size=.5) +
  geom_point(aes(color=region),size=2) +
  scale_x_continuous(limits = c(0,75), breaks = seq(0,75,25)) +
  scale_y_continuous(limits = c(0,75), breaks = seq(0,75,25)) +
  scale_color_manual(values = wesanderson::wes_palette("Darjeeling1"), name="Regions") +
  labs(title = "Cash is Queen?",
       subtitle = "The relative share of women depicted on banknotes tend to\nbe lower in countries with higher gender inequality",
       caption = "Gilbert Fontana | Data: The Pudding & UNDP"
  ) +
  xlab("Gender Inequality Index") +
  ylab("Share of women depicted on banknotes (%)") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(hjust=.5, color=txt_col, size=10, margin = margin(10,0,0,0)),
    axis.title.y = element_text(hjust=.5, color=txt_col, size=10, margin = margin(0,10,0,0)),
    axis.text = element_text(color=txt_col), 
    axis.line = element_line(color = txt_col),
    plot.title = element_text(hjust=.5,size=20, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=.5,size=16, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = c(.9,.8),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.title = element_text(color = txt_col, size= 8, face="bold"),
    legend.text = element_text(color = txt_col, size= 8)
  )

# Save
showtext_opts(dpi = 320) 

ggsave("Banknotes.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  





