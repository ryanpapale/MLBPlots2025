library("shiny")
library("stringr")

ui <- fluidPage(
	selectInput("team", "Select an MLB Team.", 
				str_sub(list.files("Plots/"), 1, -5)),
	imageOutput("plot")
)

server <- function(input, output, session) {
	output$plot <- renderImage({
		list(src = file.path("Plots/", paste0(input$team, ".gif")),
			 width = 400,
			 height = 400)
	}, deleteFile = FALSE)
}

shinyApp(ui, server)