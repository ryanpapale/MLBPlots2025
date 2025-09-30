library("tidyverse")
library("gganimate")
library("ggrepel")

#Load Data
teams <- list.files("Data/") %>%
	str_sub(1, -5)

tables <- lapply(teams, function(i){
	read_csv(file = paste0("Data/", i, ".csv"))
})

names(tables) <- teams

#Create plots
plots <- lapply(1 : length(tables), function(i){
	ggplot(tables[[i]], aes(x = `Gm.`, y = wins)) +
		geom_line(aes(linetype = "Actual")) +
		geom_segment(aes(xend = 162, yend = projected, linetype = "Projected")) +
		geom_text_repel(aes(label = ifelse(`Gm.`< 162, paste0("Projected Wins: \n", projected),
										   paste0("Final Wins: \n", wins)), x = `Gm.`, y = wins), 
						box.padding = 1, nudge_x = -.5, nudge_y = .5, color = "red") +
		transition_reveal(`Gm.`) +
		theme_classic() +
		labs(x = "Games",
			 y = "Wins",
			 linetype = "",
			 title = paste0(teams[[i]], " 2025")) +
		scale_x_continuous(expand = expansion(c(0, 0)), breaks = seq.int(0, 162, by = 50)) +
		scale_y_continuous(expand = expansion(c(0, 0)), breaks = seq.int(0, 162, by = 50)) +
		theme(panel.grid.major = element_line(),
			  legend.position = "bottom",
			  plot.title = element_text(hjust = 0.5))
})

names(plots) <- teams

#Animate Plots
gifs <- plots %>%
	lapply(animate, nframes = 162, duration = 27, end_pause = 30, height = 4, width = 4, units = "in", res = 150)

names(gifs) <- teams

#Save Plots
lapply(1 : length(gifs), function(i){
	anim_save(gifs[[i]], filename = paste0("Plots/", names(gifs[i]), ".gif"))
})