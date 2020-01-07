## Lumina ----------------------------------------------------------

library(tidyverse)
library(hrbrthemes)
library(janitor)
library(formattable)
library(flextable)
library(reshape2)
library(scales)
library(ggrepel)
Sys.setlocale("LC_TIME", 'portuguese')
options(scipen = 999)


### dados
load(file = "dados_lumina_2018.Rdata")


# Canais ------------------------------------------------------------------

ft <- flextable(select(p, from_name, tipo, likes, Posts, Compartilhamentos))
ft <- autofit(ft)
print(ft, preview = "docx")

# Seguidores --------------------------------------------------------------


table <-  Rmisc::summarySE(p, measurevar="likes",
                           groupvars=c("tipo"))

ft <- flextable(table)
ft <- colformat_num(
  x = ft, col_keys = c("N", "shares_count", "sd", "se", "ci",
                       big.mark=".", digits = 1))
ft <- autofit(ft)
print(ft, preview = "docx")


# Media de compartilhamento -----------------------------------------------

table <-  Rmisc::summarySE(amostra, measurevar="shares_count",
                           groupvars=c("tipo"))

ft <- flextable(table)
ft <- colformat_num(
  x = ft, col_keys = c("N", "shares_count", "sd", "se", "ci",
                       big.mark=".", digits = 1))
ft <- autofit(ft)
print(ft, preview = "docx")
  
  
ggplot(table, ### The data frame to use. 
         aes(x = reorder(tipo, shares_count),
             y = shares_count)) +
    geom_point(size  = 4) +
    geom_errorbar(aes(ymin  = shares_count - ci,
                      ymax  = shares_count + ci),
                  width = 0.2, 
                  size  = 0.7) + coord_flip() + theme_minimal() + 
  labs(x = "", y = "Compartilhamentos",
       caption = "\nFonte: Facebook Graph API \nNota: Barras de erro representam o intervalo de confiança, medido como duas vezes o erro padrão" ) + 
  hrbrthemes::theme_ipsum_rc(grid = "X", 
                             axis_text_size = 20, 
                             caption_size = 14,axis_title_just = "center", axis_title_size =   20)



ggsave("lumina/media_erro.png", width = 12, height = 8)


# Posts por dia -----------------------------------------------------------

amostra %>% 
  group_by(mes_dia) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(mes_dia, n)) + geom_col()

# Compartilhamentos por mes por categoria ---------------------------------


tmp <- amostra %>% 
  group_by(tipo, mes_dia) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() 



# Top 30

ylab <- c(0, 10, 20, 30)

tmp %>% 
  ggplot(aes(x=mes_dia, y=n, group = tipo, color = tipo)) +
  geom_line( size = 4) +
  geom_point( fill = "white", size = 4.5, shape = 21) +
  #facet_wrap(~ NATUREZA) +
  labs(x = "", y = "Total de Compartilhamentos", 
       caption = "\nDados extraídos pela Facebook Graph API")+
  ggrepel::geom_text_repel(data=top_n(tmp, 1, n), aes(label=paste(tipo, prettyNum(n, "."))), 
                           color = "black", size = 6,direction = "both" , alpha = .9, nudge_x = 9, fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )   +
  theme_ipsum_rc(grid = "Y", base_size = 16, 
                 axis_text_size = 20, 
                 caption_size = 14,axis_title_just = "center", axis_title_size =   14) + 
  scale_colour_grey()  +
  theme(legend.position = 'none')  +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab
  ) + scale_x_date(date_breaks = "1 month", date_labels = "%b") + expand_limits(x = as.Date("2018-10-16"))


ggsave("lumina/serietemp.png", width = 12, height = 8)



# Anova -------------------------------------------------------------------

res.aov <- aov(log_shares ~ 
                 tipo, data = amostra)

# Summary of the analysis
t <- summary(res.aov)[[1]] 
t$variable <- rownames(t)
df <- data.frame(t)
# arrumar
names(df)[5] <- "p"
rownames(df) <- NULL



myft <- flextable(df)
#myft <- merge_v(myft, j = c("country", "variable") )
#myft <- color(myft, ~ p < .005, ~ p, color = "red")
myft <- bold(myft, ~ p < .005, ~ p, bold = TRUE)
myft <- autofit(myft)
myft
print(myft, preview = "docx")
