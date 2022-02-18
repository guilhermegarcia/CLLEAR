library(tidyverse)
library(plotly)
library(scales)
library(party)
library(lme4)
library(plotly)
library(brms)
library(sjPlot)
library(arm)
library(broom)
library(knitr)

d = read_csv("feedbackData.csv")

dLong = d %>% 
  pivot_longer(names_to = "Task",
               values_to = "Score",
               cols = task_A1:task_B5) %>% 
  mutate(Task = str_sub(Task, start = -2L, end = -1L ),
         Item = str_sub(Task, start = -1L, end = -1L),
         Task = str_sub(Task, start = -2L, end = -2L)) %>% 
  dplyr::select(ID:Task, Item, Score)

dLong = dLong %>% 
  mutate(across(where(is.character), as.factor))

str(dLong)

fit = lmer(Score ~ Feedback * L1 + (1 + Feedback | ID) + 
             (1 | Item), data = dLong)

summary(fit)

ggplot(data = tibble(x = c(-2.5, 2.5)),
       aes(x = x)) + 
  stat_function(fun = dnorm) +
  stat_function(fun = dnorm,
                xlim = c(-0.7, 0.7),
                geom = "area",
                alpha = 0.3) +
  stat_function(fun = dnorm,
                xlim = c(-2, -2),
                geom = "point",
                alpha = 1, size = 8,
                color = "red") +
  theme_classic() + 
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 18)) +
  labs(y = "Figure efficiency",
       x = "Amount of information in figure")

ggplot(data = dLong, aes(x = Feedback, y = Score)) + 
  stat_summary(fun = mean, geom = "bar", 
               alpha = 0.5, color = "black",
               width = 0.5) +
  theme_classic()

ggplot(data = dLong, aes(x = Feedback, y = Score)) + 
  stat_summary() +
  theme_classic()

ggplot(data = dLong, aes(x = Feedback, y = Score)) + 
  geom_violin(alpha = 0.1, fill = "black") +
  geom_boxplot() +
  stat_summary() +
  theme_classic() +
  theme(text = element_text(size = 18))


ggplot(data = dLong, aes(x = Feedback, y = Score, label = L1)) + 
  geom_boxplot(aes(fill = Task)) +
  stat_summary(aes(group = Task), 
               position = position_dodge(width = 0.75),
               color = "black") +
  geom_text(d = dLong %>% 
              group_by(Feedback, Task, L1) %>% 
              summarize(Score = mean(Score)),
            position = position_dodge(width = 0.75),
            aes(group = Task), color = "blue") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("white", "gray")) +
  theme(text = element_text(size = 18))


ggplot(data = dLong, aes(x = Feedback, y = Score, label = L1)) + 
  # geom_boxplot() +
  stat_summary(aes(group = Task), 
               position = position_dodge(width = 0.75),
               color = "black") +
  geom_text(d = dLong %>% 
              group_by(Feedback, Task, L1) %>% 
              summarize(Score = mean(Score)),
            position = position_dodge(width = 0.75),
            aes(group = Task), color = "blue") +
  # stat_summary(aes(group = Item), geom = "line") +
  geom_jitter(alpha = 0.2, aes(group = ID)) +
  facet_grid(~Task, labeller = "label_both") +
  scale_fill_manual(values = c("white", "gray")) +
  theme_classic() +
  theme(legend.position = "top") +
  theme(text = element_text(size = 18))

ggplotly(tooltip = c("L1", "ID", "Score"))




fit = ctree(Score ~ L1 + Sex + Hours + Feedback + Task, data = dLong)

plot(fit,
     inner_panel = node_inner,
     ip_args = list(
       abbreviate = F,
       id = F,
       fill = "gray80"), 
     tp_args = list(col = "black", width = 0.25,
                    fill = alpha(c("gray70", "gray50", "gray30"))))




model = lmer(Score ~ Hours * Feedback + 
               (1 | ID) + (1 | Item), data = dLong)

tab_model(model, show.r2 = T, 
          show.re.var = F, 
          show.icc = F, show.ngroups = F)


dLong = dLong %>% 
  mutate(`Hours (std)` = rescale(Hours),
         `Feedback (std)` = rescale(Feedback))

model.std = lmer(Score ~ `Hours (std)` * Feedback + 
               (1 | ID) + (1 | Item), data = dLong)

tab_model(model.std, show.r2 = T,
          show.re.var = F, 
          CSS = list(css.firsttablecol = "width: 50%"),
          show.icc = F, show.ngroups = F)


ggplot(data = dLong, aes(x = Hours, y = Score)) + 
  geom_point(alpha = 0.1, size = 4) + 
  geom_smooth(method = lm, aes(color = Feedback)) + 
  theme_classic() +
  theme(legend.position = "top") +
  theme(text = element_text(size = 18)) +
  scale_color_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 14.3, linetype = "dashed")
  


plot_model(model.std, show.values = T) + 
  labs(title = NULL) +
  theme_classic() +
  theme(text = element_text(size = 18)) +
  scale_y_discrete(labels = function(x) stringr::str_trunc(x, 3))

model0 = lm(Score ~ Hours*Feedback, data = dLong)

new = tibble(Hours = rep(seq(10, 25, 1), each = 10),
             Feedback = rep(c("Explicit correction",
                              "Recast"), times = 80))

dLong = dLong %>% 
  mutate(H = rescale(Hours),
         Fb = Feedback)
dLong
# fitB = brm(Score ~ H * Fb,
#            data = dLong,
#            family = "Gaussian",
#            cores = 4)


# save(fitB, file = "fitB.RData")
load("fitB.RData")

bayesplot::mcmc_areas(fitB, pars = c("b_H", "b_FbRecast",
                                     "b_H:FbRecast"),
                      prob = 0.95, point_est = "mean") +
  theme(text = element_text(family = "Arial", size = 18),
        axis.text = element_text(face = "plain")) + 
  labs(x = "Estimate")



ggplot(data = dLong, aes(x = Hours, y = Score)) + 
  geom_point(alpha = 0.1, size = 4, aes(group = ID)) + 
  stat_smooth(method = lm, aes(color = Feedback)) + 
  theme_classic() +
  theme(legend.position = "top") +
  theme(text = element_text(size = 18)) +
  scale_color_manual(values = c("red", "blue")) +
  geom_vline(xintercept = 14.3, linetype = "dashed")

p = ggplotly() %>%
  layout(legend = list(
    orientation = "h",
    y = 5)) %>% 
  config(displayModeBar = FALSE,
         displaylogo = FALSE)


library(htmlwidgets)
saveWidget(p, "plotly.html", selfcontained = F, libdir = "lib")


htmltools::tags$iframe(
  src = "plotly.html", 
  scrolling = "no", 
  seamless = "seamless", align = "center",
  frameBorder = "0", height = 400, width = "80%",
)
