library(ggplot2)
library(ggh4x)
library(extrafont)

theme_set(
    theme_minimal(base_family = "CMU Serif") +  # police LaTeX
        theme(
            # Titre du plot en gras, centré, et taille 18
            plot.title = element_text(hjust = 0.5, family = "CMU Serif", size = 18, face = "bold"),
            # Titres des axes en gras et taille 14
            axis.title.x = element_text(size = 14, family = "CMU Serif", face = "bold"),
            axis.title.y = element_text(size = 14, family = "CMU Serif", face = "bold"),
            # Texte des axes, taille 14
            axis.text.x = element_text(size = 14, family = "CMU Serif"),
            axis.text.y = element_text(size = 14, family = "CMU Serif"),
            # Légende : position supra, taille 12
            legend.position = "top",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            # Grilles horizontale seulement pour les axes
            panel.grid.major.y = element_line(color = "gray", linewidth = 0.5),
            panel.grid.minor.y = element_line(color = "gray", linewidth = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            # Marges (10,10,10,10) du plot
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
            # Texte des facettes
            strip.text = element_text(size = 14, family = "CMU Serif")))
