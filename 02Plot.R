library("tidyverse")
library("gganimate")
library("ggrepel")

#Load Data
tables <- read_csv("Data/main.csv") 

teams <- tables %>%
	summarize(team = first(team), 
			  col1 = first(col1),
			  col2 = first(col2),
			  col3 = first(col3),
			  col4 = first(col4),
			  col5 = first(col5), .by = "team")

tables <- tables %>%
	split(tables[["team"]])

#Create Win Plots
winplots <- lapply(1 : length(tables), function(i){
	ggplot(tables[[i]], aes(x = Gm., y = wins)) +
		geom_line(aes(linetype = "Actual"), color = teams[["col1"]][i]) +
		geom_segment(aes(xend = 162, yend = projected, linetype = "Projected"), color = teams[["col2"]][i]) +
		geom_text_repel(aes(label = ifelse(Gm.< 162, paste0("Projected Wins: \n", projected),
										   paste0("Final Wins: \n", wins)), x = Gm., y = wins), 
						box.padding = 1, nudge_x = -.5, nudge_y = .5, color = teams[["col1"]][i]) +
		transition_reveal(Gm.) +
		theme_classic() +
		labs(x = "Games",
			 y = "Wins",
			 linetype = "",
			 title = "Number of Wins") +
		scale_x_continuous(expand = expansion(c(0, 0)), breaks = seq.int(0, 162, by = 50)) +
		scale_y_continuous(expand = expansion(c(0, 0)), breaks = seq.int(0, 162, by = 50)) +
		theme(panel.grid.major = element_line(),
			  legend.position = "bottom",
			  plot.title = element_text(hjust = 0.5))
})

names(winplots) <- teams[["team"]]

#Create Win Perc. Plots
percplots <- lapply(1 : length(tables), function(i){
	ggplot(tables[[i]]) +
		geom_line (aes(x = Gm., y = centwinperc), color = teams[["col1"]][i]) +
		geom_ribbon(aes(x = Gm., ymin = cilow, ymax = ciup, fill = "95% Confidence Interval"), alpha = .3) +
		transition_reveal(Gm.) +
		theme_classic() +
		labs(x = "Games",
			 y = "Centered Win %",
			 title = "Win %") +
		scale_x_continuous(expand = expansion(c(0, 0)), breaks = seq.int(0, 162, by = 50)) +
		scale_y_continuous(expand = expansion(c(0, 0)), breaks = seq.int(-5, 5, by = .25)) +
		scale_fill_manual(name = NULL, values = c("95% Confidence Interval" = teams[["col2"]][i])) +
		theme(panel.grid.major.y = element_line(),
			  legend.position = "bottom",
			  plot.title = element_text(hjust = 0.5))
})

names(percplots) <- teams[["team"]]

#Animate Plots
wingifs <- winplots %>%
	lapply(animate, nframes = 162, fps = 6, end_pause = 30, height = 3, width = 3, units = "in", res = 150)

percgifs <- percplots %>%
	lapply(animate, nframes = 162, fps = 6, end_pause = 30, height = 3, width = 3, units = "in", res = 150)


names(wingifs) <- teams[["team"]]
names(percgifs) <- teams[["team"]]

#Save Plots
lapply(1 : length(wingifs), function(i){
	anim_save(wingifs[[i]], filename = paste0(names(wingifs[i]), ".gif"), path = "Plots/Wins")
})

lapply(1 : length(percgifs), function(i){
	anim_save(percgifs[[i]], filename = paste0(names(percgifs[i]), ".gif"), path = "Plots/Perc")
})

#Save Colors
write_csv(teams, file = "Data/colors.csv")
