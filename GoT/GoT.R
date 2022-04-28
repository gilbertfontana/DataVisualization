

# Lib
library(tidyverse)
library(janitor)
library(ggtext)
library(ggstream)
library(MetBrewer)
library(showtext)

# Data
df <- readxl::read_excel("",
                         range = "") %>% 
  clean_names()

top5 <- df %>% 
  group_by(killer) %>%
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice_head(n=5) %>% 
  select(killer)

df <- df %>% 
  mutate(
    killer2= case_when(
      killer %in% top5$killer ~ killer,
      TRUE ~ "Others"
    ))

death_count <- df %>% 
  group_by(season,killer2) %>% 
  count()



# Misc
font_add_google(family="Space Grotesk", "Space Grotesk")
bg <- "#E9E5D6"

showtext_auto(enable = TRUE) 

# Plot
death_count %>% 
  arrange(n) %>%
  mutate(killer2 = factor(killer2, levels=c("Daenerys Targaryen", "Cersei Lannister","Arya Stark", "Wight","Jon Snow", "Others"))) %>%
  ggplot(aes(season, n, fill = killer2)) +
  geom_stream(type = "mirror",n_grid = 1000,
              extra_span = .2) +
  annotate("text", x = 6, y = 100,
           label = "Destruction of the\nGreat Sept of Baelor",
           size=3,
           color="black",
           fontface="bold",
           family="Space Grotesk",
           lineheight=.8) +
  annotate("text", x = 7.5, y = 50,
           label = "Battle of\nKing's Landing",
           size=3,
           color="black",
           fontface="bold",
           family="Space Grotesk",
           lineheight=.8) +
  scale_x_continuous(breaks = c(seq(1,8,1))) +
  scale_y_continuous(limits = c(-800,800)) +
  scale_fill_manual(values=met.brewer("Tam", 6)) +
  labs(title = "TOP <span style='color:#ac0404;'>KILLERS</span> IN GAME OF THRONES",
       caption = "Gilbert Fontana | #MakoverMonday | Data: data.world") +
  xlab("Season") +
  coord_cartesian(clip="off") +
  theme_minimal(base_family = "Space Grotesk")+
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color="black", size=12),
    axis.title.x = element_text(face="bold", size=14,margin=margin(20,0,0,0)),
    
    legend.position = c(0.15,0.8),
    legend.title = element_blank(),
    legend.text = element_text(),
    
    plot.title = element_text(hjust = .5, face = "bold", size=40),
    plot.caption = element_text(hjust=.5,margin=margin(30,0,0,0), face="bold"),
    
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color=bg, fill=bg)
  ) +
  theme(plot.title = element_markdown())


showtext_opts(dpi = 320) 

# Save
ggsave("GoT.png",
       height = 10,
       width = 10,
       dpi=320,
       
)  

showtext_auto(FALSE)

