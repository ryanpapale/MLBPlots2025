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
	lapply(select, Gm., `W.L`, R, RA) %>%
	lapply(filter, Gm. != "Gm#") %>%
	lapply(filter, row_number() <= 162) %>%
	lapply(Gm. = as.numeric(Gm.),
		   mutate, `W.L` = str_sub(`W.L`, 1, 1),
		   wins = cumsum(ifelse(`W.L` == "W", 1, 0)),
		   losses = cumsum(ifelse(`W.L` == "L", 1, 0)),
		   winperc = wins / (wins + losses), 
		   projected = round(162 * winperc),
		   final = ifelse(row_number() == 162, paste0("Final Record \n Wins: ", wins, ", Losses: ", 
		   										   losses, "\n W/L Ratio: ", round(winperc, 4)), ""),
		   p = round(max(ifelse(Gm. == 162, winperc, -1)), 4),
		   centwinperc = winperc - p,
		   se = sqrt(p * ( 1 - p) / row_number()),
		   ciup = centwinperc + (1.96 * se), 
		   cilow = centwinperc - (1.96 * se)) %>%
	bind_rows(.id = "team")

#Get Color Codes
coltab <- read_html("https://teamcolorcodes.com/mlb-color-codes/") %>%
	html_elements("table")

#Pull Hex Colors
for(i in 1 : length(coltab)){
	if(str_detect(html_text(coltab[i]), "HEX") == TRUE){
		colors <- coltab[i] %>%
			html_table() %>%
			as.data.frame()
	}
}

#Clean Colors
colors <- colors %>%
	rename(team = X1, 
		   col1 = X2,
		   col2 = X3,
		   col3 = X4,
		   col4 = X5,
		   col5 = X6) %>%
	mutate(across(col1 : col5, ~str_replace_all(., "White #FFFFFF", "")),
		   across(col1 : col5, ~str_replace_all(., "White", "")),
		   across(col1 : col5, ~gsub(".* ", "", .)),
		   team = ifelse(team == "Los Angeles Angels", "Los Angeles Angels of Anaheim", team),
		   team = ifelse( team == "Oakland Athletics", "Athletics", team))

#Merge Colors w/ Data
tables <- tables %>%
	merge(colors, by = "team")

#Save Cleaned Data
write_csv(tables, file = "Data/main.csv")