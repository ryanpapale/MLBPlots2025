library("tidyverse")
library("rvest")


#Get List of Teams Abbreviations for Loop
teams <- read_html("https://www.baseball-reference.com/about/team_IDs.shtml") %>%
	html_table(header = TRUE) %>%
	as.data.frame() %>%
	filter(Last.Year == "Present") %>%
	select(Team.ID, Full.Team.Name)


#Create URLs for Loop
url <- as.list(paste0("https://www.baseball-reference.com/teams/", teams[["Team.ID"]], "/2025-schedule-scores.shtml"))
names(url) <- teams[["Full.Team.Name"]]

#Create List for Pages
html <- vector(mode = "list", length = nrow(teams))
names(html) <- teams[["Full.Team.Name"]]

#Scrape Game Data
for(i in names(url)){
	html[[i]] <- read_html(url[[i]])
	
	Sys.sleep(4)
}

#Clean Tables
tables <- html %>%
	lapply(html_elements, "table") %>%
	lapply(html_table) %>%
	lapply(as.data.frame) %>%
	lapply(select, `Gm.`, `W.L`) %>%
	lapply(filter, `Gm.` != "Gm#") %>%
	lapply(filter, row_number() <= 162) %>%
	lapply(`Gm.` = as.numeric(`Gm.`),
		   mutate, `W.L` = str_sub(`W.L`, 1, 1),
		   wins = cumsum(ifelse(`W.L` == "W", 1, 0)),
		   losses = cumsum(ifelse(`W.L` == "L", 1, 0)),
		   wlratio = wins / (wins + losses), 
		   projected = round(162 * wlratio),
		   final = ifelse(row_number() == 162, 
		   			   paste0("Final Record \n Wins: ", wins, ", Losses: ", losses, "\n W/L Ratio: ", round(wlratio, 4)), 
		   			   ""))

#Save Cleaned Data
lapply(1 : length(tables), function(i){
	write_csv(tables[[i]], file = paste0("Data/", names(tables[i]), ".csv"))
})
