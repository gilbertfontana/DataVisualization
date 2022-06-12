

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(lubridate)
library(scico)

# Data
df <- readxl::read_excel(
  "C:/Users/Gilbert/Documents/GitHub/DataVisualization/Hong Kong/HKPPD_ver1.1.xlsx",
  sheet = "custodialSentence"
)

df2 <- df %>% 
  filter(sentence_centre==0) %>% 
  select(id, age, sentenced_date, sentence_months) %>% 
  mutate(Date = floor_date(as_date(sentenced_date), "month")) %>% 
  na.omit() %>% 
  filter(!id==107) %>% 
  group_by(Date) %>% 
  summarise(avg_sentence=mean(sentence_months), n=n())

# Misc
font <- "Reem Kufi"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "grey10"


# Plot
df2 %>% 
  ggplot() +
  geom_segment(aes(x=Date, xend=Date, y=0, yend=n)) +
  geom_point(aes(x=Date, y=n, size=avg_sentence, color=Date)) +
  scale_y_continuous(limits = c(0,60), breaks = seq(0,60,20), expand = c(0,0)) +
  scale_color_scico(palette = 'bilbao',
                    begin =.4,
                    guide="none") +
  scale_size_continuous(
    breaks = c(10,20,30),
    limits = c(0,30),
    name = "Average sentence\nlength (months)") +
  labs(
    title="Political prisoners in Hong Kong",
    subtitle = "Number of people sentenced to prison",
    caption = "Gilbert Fontana | Data: Hong Kong Political Prisoners Database"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_text(color=txt_col),
    axis.line.x  = element_line(color=txt_col),
    plot.title = element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,60,30,60),
    legend.position = "bottom",
    legend.title = element_text(color=txt_col),
    legend.justification = "center",
    legend.title.align=0.5,
  ) +
  guides(size = guide_legend(title.position = "top"))
  

# Save
showtext_opts(dpi = 320) 

ggsave("Hongkong.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)



