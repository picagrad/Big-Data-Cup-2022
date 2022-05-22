#Figures

# load("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/data/pass_summary.Rdata")
#setwd("C:/Users/thepi/OneDrive/Documents/Python/Hockey/Big-Data-Cup-2022-Private")
setwd("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private")

load("data/pass_summary.Rdata")

load.libraries = c("tidyverse", "plotly", "htmlwidgets", "IRdisplay", "ggimage")
install.lib = load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib) {suppressMessages(install.packages(libs, dependencies = TRUE))}
suppressMessages(require(tidyverse)); require(plotly) %>% suppressMessages()
require(htmlwidgets) %>% suppressMessages(); require(IRdisplay) %>% suppressMessages()
# current_event2 <- current_event[rowSums(is.na(current_event[,40:50])) == 0,]
# current_event3 <- current_event2 %>% group_by(player_name) %>% summarise(team=team_name,
#                                                        num_passes = n(), 
#                                                        avg_best_case = mean(max_best_case_within_Vel_init),
#                                                        avg_keep_possession=mean(max_keep_possession_within_Vel_init),
#                                                        avg_expected_pass=mean(max_expected_pass_value_within_Vel_init))
# current_event4 <- current_event3[!duplicated(current_event3),]


images <- data.frame(team=c("Canada", "USA", "Switzerland", "ROC", "Finland"),
                     url=c("https://cdn.countryflags.com/thumbs/canada/flag-button-round-250.png","https://cdn.countryflags.com/thumbs/united-states-of-america/flag-button-round-250.png",
                           "https://cdn.countryflags.com/thumbs/switzerland/flag-button-round-250.png","images/round_roc_flag.png",
                           "https://cdn.countryflags.com/thumbs/finland/flag-round-250.png"))

passes <- current_event %>%  filter(!is.na(passer_pass_value))
overall_expected_or_hold = passes %>% select(contains("max_expected_pass_value_overall")) #%>% 
cbind(passes$passer_pass_value)
overall_expected_or_hold = passes %>% select(contains("max_expected_pass_value_overall"))
actual_expected = passes$max_expected_pass_value_within_Vel_init


overall_best_case = passes %>% select(contains("max_best_case_overall")) #%>% 
cbind(passes$passer_pass_value)
actual_best_case = passes$max_best_case_within_Vel_init


overall_keep = passes %>% select(contains("max_keep_possession_overall"))
actual_keep = passes$max_keep_possession_within_Vel_init
passes <- passes %>% mutate(best_expected_cl  = do.call(pmax,overall_expected_or_hold),#overall_expected), ask alon
                            best_expected_or_hold_cl = do.call(pmax,overall_expected_or_hold),
                            best_best_case_cl = do.call(pmax, overall_best_case),
                            best_keep_cl = do.call(pmax, overall_keep),
                            actual_keep_cl = actual_keep,
                            actual_best_case_cl = actual_best_case,
                            actual_expected_cl = actual_expected)

pass_sum <- passes %>% mutate(improved_exp_ratio = actual_expected_cl/best_expected_or_hold_cl) %>% 
  group_by(player_name) %>% summarise(team=team_name,
                                      num_passes = n(), 
                                      avg_best_case = mean(actual_best_case_cl),
                                      avg_keep_possession=mean(actual_keep_cl),
                                      avg_actual_expected=mean(actual_expected_cl),
                                      avg_hold_value = mean(passer_pass_value),
                                      avg_best_outcome = mean(best_expected_or_hold_cl),
                                      avg_ratio = mean(improved_exp_ratio)
  ) %>% filter(num_passes>=3) %>% distinct()



# Plot the average path deviation by the average yards on the play
p2 = pass_sum %>%
  left_join(images, by = "team") %>%
  mutate(team_factor = factor(team)) %>%
  rename(`Passer` = player_name,
         `Team` = team,
         `Best Conditional Outcome` = avg_best_case,
         `Keep Possession` = avg_keep_possession,
         `Number of Passes` = num_passes,
         `Expected Value` = avg_actual_expected,
         `Holding Value` = avg_hold_value,
         `Relative Outcome` = avg_ratio,
         `Best Outcome` = avg_best_outcome
  ) %>%
  # mutate(`Line of Best Fit` = "") %>%
  ggplot() +
  # geom_smooth(formula = "y~x", aes(x = `Keep Possession`, y = `Best Conditional Outcome`, label2 = `Line of Best Fit`), method = "lm", se = F, colour = "black") +
  labs(x = "Keep Possession", y = "Best Conditional Outcome", title = "Decision Making by Passer", subtitle = "Minimum 3 passes per player")

p_flags = p2 +  ggimage::geom_image(aes(label = `Passer`, label2 = `Team`, label3 = `Number of Passes`, x = `Keep Possession`, y = `Best Conditional Outcome`, image = url, size = I(`Number of Passes`)/500+0.02)) + 
  theme_bw()
  # scale_fill_manual(values = c("Canada" = "red", "USA" = "blue", "ROC"="green","Switzerland"="orange","Finland"="purple")) +
  # geom_point(aes(label2 = `Passer`, label3 = `Team`, label4 = `Number of Passes`, fill = `Team`, x = `Keep Possession`, y = `Best Conditional Outcome`, size  =  `Number of Passes`), shape = 21, stroke = 0.25) +
p_html = p2 + scale_fill_manual(values = c("Canada" = "red", "USA" = "blue", "ROC"="green","Switzerland"="orange","Finland"="purple")) +
  geom_point(aes(label2 = `Passer`, label3 = `Team`, label4 = `Number of Passes`, fill = `Team`, x = `Keep Possession`, y = `Best Conditional Outcome`, size  =  `Number of Passes`), shape = 21, stroke = 0.25) +
  theme_bw()
p = ggplotly(p_html, tooltip = c("label2", "label3", "label4", "fill"), height = 600, width = 662)
p
dir.create(file.path("plots/"), showWarnings = FALSE)
f <-"plots/DecisionMaking.html"
saveWidget(p, file.path(normalizePath(dirname(f)),basename(f)))
display_html('<iframe src="plots/table1.html" align="center" width="100%" height="500" frameBorder="0"></iframe>')

pdf("plots/DecisionMakingFlags.pdf")
p_flags
dev.off()

p2 = pass_sum %>%
  left_join(images, by = "team") %>%
  mutate(team_factor = factor(team)) %>%
  rename(`Passer` = player_name,
         `Team` = team,
         `Best Conditional Outcome` = avg_best_case,
         `Keep Possession` = avg_keep_possession,
         `Number of Passes` = num_passes,
         `Expected Value` = avg_actual_expected,
         `Holding Value` = avg_hold_value,
         `Relative Outcome` = avg_ratio,
         `Best Outcome` = avg_best_outcome
  ) %>%
  # mutate(`Line of Best Fit` = "") %>%
  ggplot() +
  # geom_smooth(formula = "y~x", aes(x = `Best Outcome`, y = `Relative Outcome`, label2 = `Line of Best Fit`), method = "lm", se = F, colour = "black") +
  labs(x = "Best Outcome", y = "Relative Outcome", title = "Overall Passer Performance", subtitle = "Minimum 3 passes per player")

p_flags = p2 +  ggimage::geom_image(aes(label = `Passer`, label2 = `Team`, label3 = `Number of Passes`,x = `Best Outcome`, y = `Relative Outcome`, image = url, size = I(`Number of Passes`)/500+0.02)) + 
  theme_bw()
# scale_fill_manual(values = c("Canada" = "red", "USA" = "blue", "ROC"="green","Switzerland"="orange","Finland"="purple")) +
# geom_point(aes(label2 = `Passer`, label3 = `Team`, label4 = `Number of Passes`, fill = `Team`, x = `Keep Possession`, y = `Best Conditional Outcome`, size  =  `Number of Passes`), shape = 21, stroke = 0.25) +
p_html = p2 + scale_fill_manual(values = c("Canada" = "red", "USA" = "blue", "ROC"="green","Switzerland"="orange","Finland"="purple")) +
  geom_point(aes(label2 = `Passer`, label3 = `Team`, label4 = `Number of Passes`, fill = `Team`, x = `Best Outcome`, y = `Relative Outcome`, size  =  `Number of Passes`), shape = 21, stroke = 0.25) +
  theme_bw()
p = ggplotly(p_html, tooltip = c("label2", "label3", "label4", "fill"), height = 600, width = 662)
p
dir.create(file.path("plots/"), showWarnings = FALSE)
f <-"plots/OverallPassing.html"
saveWidget(p, file.path(normalizePath(dirname(f)),basename(f)))
display_html('<iframe src="plots/table1.html" align="center" width="100%" height="500" frameBorder="0"></iframe>')

pdf("plots/OverallPassingFlags.pdf")
p_flags
dev.off()

save(pass_sum, file = 'data/final_pass_summary.Rdata')
