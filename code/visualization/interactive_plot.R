#Figures

load("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/data/pass_summary.Rdata")

load.libraries = c("tidyverse", "plotly", "htmlwidgets", "IRdisplay")
install.lib = load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib) {suppressMessages(install.packages(libs, dependencies = TRUE))}
suppressMessages(require(tidyverse)); require(plotly) %>% suppressMessages()
require(htmlwidgets) %>% suppressMessages(); require(IRdisplay) %>% suppressMessages()
current_event2 <- current_event[rowSums(is.na(current_event[,40:50])) == 0,]
current_event3 <- current_event2 %>% group_by(player_name) %>% summarise(team=team_name,
                                                       num_passes = n(), 
                                                       avg_best_case = mean(max_best_case_within_Vel_init),
                                                       avg_keep_possession=mean(max_keep_possession_within_Vel_init),
                                                       avg_expected_pass=mean(max_expected_pass_value_within_Vel_init))
current_event4 <- current_event3[!duplicated(current_event3),]
# Plot the average path deviation by the average yards on the play
p2 = current_event4 %>%
  mutate(team_factor = factor(team)) %>%
  rename(`Passer` = player_name,
         `Team` = team_factor,
         `Best Within` = avg_best_case,
         `Keep Possession` = avg_keep_possession,
         `Number of Passes` = num_passes,
         `Expected Value` = avg_expected_pass
  ) %>%
  mutate(`Line of Best Fit` = "") %>%
  ggplot() +
  scale_fill_manual(values = c("Canada" = "red", "USA" = "blue", "ROC"="green","Switzerland"="orange","Finland"="purple")) +
  geom_point(aes(label2 = `Passer`, label3 = `Team`, label4 = `Number of Passes`, fill = `Team`, x = `Keep Possession`, y = `Expected Value`), size = 3.25, shape = 21, stroke = 0.25) +
  geom_smooth(formula = "y~x", aes(x = `Keep Possession`, y = `Expected Value`, label2 = `Line of Best Fit`), method = "lm", se = F, colour = "black") +
  labs(x = "Keep Possession", y = "Expected Value", title = "Decision Making by Passer", subtitle = "Minimum 2 players per team in shot") +
  ##keep possession vs best case
  #geom_text(aes(x = 0.02, y = 0.1, label = "Poor Decision-Making,\nRisky with Possession"), size = 3) +
  #geom_text(aes(x = 0.1, y = 0.95, label = "Good Decision-Making,\nPrioritized Possession"), size = 3) +
  #geom_text(aes(x = 0.02, y = 0.95, label = "Poor Decision-Making,\nPrioritized Possession"), size = 3) +
  #geom_text(aes(x = 0.1, y = 0.1, label = "Good Decision-Making,\nRisky with Possession"), size = 3) +
  ##best case vs expected value
  #geom_text(aes(x = 0.02, y = -0.01, label = "Poor Decision-Making,\nLow Added Value"), size = 3) +
  #geom_text(aes(x = 0.1, y = 0.15, label = "Good Decision-Making,\nHigh Added Value"), size = 3) +
  #geom_text(aes(x = 0.02, y = 0.15, label = "Poor Decision-Making,\nHigh Added Value"), size = 3) +
  #geom_text(aes(x = 0.1, y = -0.01, label = "Good Decision-Making,\nLow Added Value"), size = 3) +
  ##keep possession vs expected value
  geom_text(aes(x = 0.1, y = -0.02, label = "Poor Decision-Making,\nLow Added Value"), size = 3) +
  geom_text(aes(x = 0.9, y = 0.15, label = "Good Decision-Making,\nHigh Added Value"), size = 3) +
  geom_text(aes(x = 0.1, y = 0.15, label = "Poor Decision-Making,\nHigh Added Value"), size = 3) +
  geom_text(aes(x = 0.9, y = -0.02, label = "Good Decision-Making,\nLow Added Value"), size = 3) +
  theme_bw()

p = ggplotly(p2, tooltip = c("label2", "label3", "label4", "fill"), height = 600, width = 662)
p
dir.create(file.path("plots/"), showWarnings = FALSE)
f <-"plots/table1.html"
saveWidget(p, file.path(normalizePath(dirname(f)),basename(f)))
display_html('<iframe src="plots/table1.html" align="center" width="100%" height="500" frameBorder="0"></iframe>')
