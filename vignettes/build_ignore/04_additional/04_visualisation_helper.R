setwd("/Users/mcooper/Documents/my_proj_git/RWorkshop/data/")
library(ggplot2)
library(tidyverse)
library(biometrics)

dat <- read_csv("demo.csv")
dat2 <- read_csv("demo_complications.csv")

theme_set(theme_minimal())
theme_update(
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.line.x = element_line(),
  axis.line.y = element_line()
)

dat %>%
  left_join(dat2) %>%
  pivot_longer(cols = day1:day3) %>%
  group_by(id) %>%
  filter(value < 100) %>%
  mutate(max = case_when(value == max(value, na.rm = T) ~ 1,
                         TRUE ~ 0),
         any_comp = as.factor(case_when(any(!is.na(complications)) ~ "Yes",
                                        TRUE ~ "No"))) %>%
  ungroup() %>%
  mutate(sex = as.factor(case_when(male == T ~ "Male",
                                   TRUE ~ "Female")),
         name = as.numeric(as.factor(name))) %>%
  ggplot(aes(x = name, y = value)) +
  geom_line(aes(colour = any_comp, group = id), alpha = 0.3) +
  geom_point(colour = "Red", alpha = 0.7, size = 4,
             data = . %>% filter(max == 1)) +
  geom_point(aes(colour = any_comp), alpha = 0.7) +
  geom_smooth(aes(colour = any_comp), method = lm, formula = y ~ x, size = 2, se = F) + 
  facet_wrap( ~ sex + intervention) +
  scale_color_manual(values=c("#002D88", "#F3AB00")) +
  labs(colour = "Any Complication",
       x = "Time (days)",
       y = "Value (units)") 
  
  
## Shared Legends Example

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  # return gtable invisibly
  invisible(combined)
}

library(viridis)
library(ggstance)

dat_sl_1 <- tibble(Predictor = c("Asthma", "Asthma", "Asthma", "Allergies", "Allergies", "Allergies", "Hay fever", "Hay fever", "Hay fever", "Eczema", "Eczema", "Eczema", "UTI", "UTI", "UTI", "Cold/flu", "Cold/flu", "Cold/flu", "Chest infection", "Chest infection", "Chest infection", "Herpes", "Herpes", "Herpes", "Other infection", "Other infection", "Other infection", "Other viral infection", "Other viral infection", "Other viral infection"),
               Outcome = c("TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT"),
               var = c(2.29, 1.53, 1.78, 2.08, 1.23, 1.97, 1.82, 1.16, 1.46, 2.20, 1.63, 1.92, 1.48, 0.74, 1.25, 1.53, 1.55, 0.90, 2.99, 2.58, 2.62, 0.32, 0.26, 0.19, 1.70, 2.16, 1.05, 1.61, 1.06, 2.20),
               CILow = c(1.58, 0.86, 1.12, 1.57, 0.75, 1.50, 1.29, 0.65, 0.95, 1.61, 1.06, 1.35, 0.45, -0.24, 0.26, 1.03, 1.07, 0.42, 1.86, 1.51, 1.57, -0.66, -0.66, -0.75, 0.77, 1.24, 0.16, 0.35, -0.16, 1.00),
               CIHigh = c(2.99, 2.20, 2.45, 2.58, 1.71, 2.45, 2.35, 1.67, 1.96, 2.79, 2.19, 2.50, 2.50, 1.73, 2.23, 2.02, 2.02, 1.37, 4.12, 3.65, 3.68, 1.29, 1.18, 1.13, 2.63, 3.08, 1.94, 2.87, 2.29, 3.40)
)

dat_s1_2 <- tibble(Predictor = c("Asthma", "Asthma", "Asthma", "Allergies", "Allergies", "Allergies", "Hay fever", "Hay fever", "Hay fever", "Eczema", "Eczema", "Eczema", "UTI", "UTI", "UTI", "Cold/flu", "Cold/flu", "Cold/flu", "Chest infection", "Chest infection", "Chest infection", "Herpes", "Herpes", "Herpes", "Other infection", "Other infection", "Other infection", "Other viral infection", "Other viral infection", "Other viral infection"),
               Outcome = c("TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT", "TOT", "EXT", "INT"),
               var = c(1.44, 1.28, 1.38, 1.47, 1.26, 1.53, 1.34, 1.20, 1.28, 1.35, 1.28, 1.49, 1.39, 1.08, 1.35, 1.28, 1.25, 1.24, 1.93, 1.77, 1.76, 1.29, 1.10, 1.28, 1.22, 1.51, 1.14, 1.39, 1.31, 1.50),
               CILow = c(1.23, 1.09, 1.17, 1.29, 1.11, 1.35, 1.17, 1.05, 1.12, 1.17, 1.10, 1.30, 1.10, 0.85, 1.07, 1.13, 1.10, 1.09, 1.53, 1.40, 1.39, 1.03, 0.87, 1.03, 0.97, 1.22, 0.91, 1.05, 0.99, 1.13),
               CIHigh = c(1.69, 1.51, 1.62, 1.67, 1.44, 1.74, 1.53, 1.38, 1.47, 1.57, 1.48, 1.72, 1.74, 1.38, 1.70, 1.46, 1.42, 1.41, 2.43, 2.24, 2.24, 1.61, 1.39, 1.61, 1.52, 1.87, 1.44, 1.84, 1.74, 1.99)
               
)

dat_sl_1$xintercept = rep(0, 30)
dat_sl_1 %>%
  mutate(Predictor = case_when(Predictor == "Other infection" ~ "Oth. Inf.",
                               Predictor == "Other viral infection" ~ "Oth. Viral Inf.",
                               Predictor == "Chest infection" ~ "Chest Inf.",
                               TRUE ~ Predictor),
         Outcome = case_when(Outcome == "TOT" ~ "Total Score",
                             Outcome == "EXT" ~ "Externalising Score",
                             Outcome == "INT" ~ "Internalising Score"),
         Predictor = factor(Predictor, levels = rev(c("Asthma", "Allergies", "Hay fever", "Eczema", "UTI", "Cold/flu", "Chest Inf.", "Herpes", "Oth. Inf.", "Oth. Viral Inf."))),
         Outcome = factor(Outcome, levels = c("Total Score", "Externalising Score", "Internalising Score"))) %>%
  ggplot(aes(x = var, y = Predictor, colour = Outcome)) +
  geom_point(position = ggstance::position_dodgev(height = -0.5),
             size = 1) +
  geom_vline(aes(xintercept = xintercept),
             linetype = "dashed", colour = "grey70") +
  ggstance::geom_errorbarh(aes(xmin = CILow, xmax = CIHigh),
                           position = ggstance::position_dodgev(height = -0.5),
                           width = 0.5, 
                           size = 1 ) + 
  theme_classic() +
  theme(strip.background = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey95"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_colour_manual(values = c("#4682B4", "#B4464B", "#B4AF46")) +
  labs(y = "", x = "", colour = "",
       title = "CBCL Score", subtitle = "Beta coefficient 95% CI") -> p_sl_1

dat_s1_2$xintercept = rep(1, 30)
dat_s1_2 %>%
  mutate(Predictor = case_when(Predictor == "Other infection" ~ "Oth. Inf.",
                               Predictor == "Other viral infection" ~ "Oth. Viral Inf.",
                               Predictor == "Chest infection" ~ "Chest Inf.",
                               TRUE ~ Predictor),
         Outcome = case_when(Outcome == "TOT" ~ "Total Score",
                             Outcome == "EXT" ~ "Externalising Score",
                             Outcome == "INT" ~ "Internalising Score"),
         Predictor = factor(Predictor, levels = rev(c("Asthma", "Allergies", "Hay fever", "Eczema", "UTI", "Cold/flu", "Chest Inf.", "Herpes", "Oth. Inf.", "Oth. Viral Inf."))),
         Outcome = factor(Outcome, levels = c("Total Score", "Externalising Score", "Internalising Score"))) %>%
  ggplot(aes(x = var, y = Predictor, colour = Outcome)) +
  geom_point(position = ggstance::position_dodgev(height = -0.5),
             size = 1) +
  geom_vline(aes(xintercept = xintercept),
             linetype = "dashed", colour = "grey70") +
  ggstance::geom_errorbarh(aes(xmin = CILow, xmax = CIHigh),
                           position = ggstance::position_dodgev(height = -0.5),
                           width = 0.5,
                           size = 1) + 
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "grey95"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(breaks = seq(0.5,3.0, 0.5)) +
  scale_colour_manual(values = c("#4682B4", "#B4464B", "#B4AF46")) + 
  labs(y = "", x = "", colour = "",
       title = "CBCL Score > 60", subtitle = "Odds Ratio (95% CI)") -> p_sl_2

grid_arrange_shared_legend(p_sl_1, p_sl_2, ncol = 2)
