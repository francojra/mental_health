# Gr�ficos animados com ggplot2
# Autora: Jeanne Franco
# R - Ladies Natal

# Pacotes necess�rios -----------------------------------------------------

library(dplyr) # Pacote para selecionar e filtrar dados da tabela
library(tibble) # Pacote para gerar tabela com impress�o organizada
library(ggplot2) # Pacote de gr�ficos ggplot2
#install.packages("ggdark")
library(ggdark) # Pacote para adicionar tema dos gr�ficos em preto
library(gridExtra) # Pacote para unir os gr�ficos em uma �nica janela
library(grid) # Pacote usado para adicionar um t�tulo a janela do gr�fico

# Leitura de dados --------------------------------------------------------

m <- read.csv("prevalence-by-mental-and-substance-use-disorder.csv", 
              header = TRUE) # Importar tabela csv para o R

names(m) # Verificar nomes das colunas
view(m) # Ver tabela

# Renomear nomes das colunas ----------------------------------------------

ment <- rename(m, c("Uso_drogas" = "Prevalence...Drug.use.disorders...Sex..Both...Age..Age.standardized..Percent.",
            "Ansiedade" = "Prevalence...Anxiety.disorders...Sex..Both...Age..Age.standardized..Percent.",
            "Depress�o" = "Prevalence...Depressive.disorders...Sex..Both...Age..Age.standardized..Percent.",
            "Transtornos_alimenta��o" = "Prevalence...Eating.disorders...Sex..Both...Age..Age.standardized..Percent."))

View(ment) # Visualizar tabela data.frame

# Sele��o de dados com pacote dplyr ---------------------------------------

mental <- ment %>% 
  select(Entity, Depress�o, Ansiedade, Uso_drogas, 
         Transtornos_alimenta��o) %>%
  filter(Entity %in% c('Cuba', 'Haiti', 'Dominican Republic',
                          'Argentina', 'Bolivia', 'Brazil',
                         'Chile', 'Colombia', 'Costa Rica',
                         'El Salvador', 'Mexico', 'Guatemala',
                       'Honduras', 'Nicaragua', 'Panama',
                         'Paraguay', 'Peru', 'Uruguay', 'Venezuela'))
         
tibble(mental) # Visualizar tabela simplificada
view(mental)

# Gr�ficos ggplot2 --------------------------------------------------------

a <- ggplot(mental, aes(reorder(Entity, Ansiedade), Ansiedade)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge", 
               fill = "#66c2a5") +
  labs(x = "Pa�ses", y = "Ansiedade (%)") +
  theme(legend.position = "none") +
  coord_flip() +
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#66c2a5", size = 0.1),
        panel.grid.minor = element_line(color = "#66c2a5", size = 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))
a

d <- ggplot(mental, aes(reorder(Entity, Depress�o), Depress�o)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge", 
               fill = "#fc8d62") +
  labs(x = "Pa�ses", y = "Depress�o (%)") +
  theme(legend.position = "none") +
  coord_flip() +
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#fc8d62", size = 0.1),
        panel.grid.minor = element_line(color = "#fc8d62", size = 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))
d

ta <- ggplot(mental, aes(reorder(Entity, Transtornos_alimenta��o), 
                         Transtornos_alimenta��o)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge", 
               fill = "#8da0cb") +
  labs(x = "Pa�ses", y = "Transtorno alimentar (%)") +
  theme(legend.position = "none") +
  coord_flip() +
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#8da0cb", size = 0.1),
        panel.grid.minor = element_line(color = "#8da0cb", size = 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))
ta

ud <- ggplot(mental, aes(reorder(Entity, Uso_drogas), Uso_drogas)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge", 
               fill = "#b2df8a") +
  labs(x = "Pa�ses", y = "Uso de drogas (%)") +
  theme(legend.position = "none") +
  coord_flip() +
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#b2df8a", size = 0.1),
        panel.grid.minor = element_line(color = "#b2df8a", size = 0.1),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13))
ud

# Gr�ficos em uma janela --------------------------------------------------

fig_final <- grid.arrange(a, d, ta, ud, ncol = 2, nrow = 2)

