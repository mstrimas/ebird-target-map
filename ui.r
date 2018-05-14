ll <- paste0("https://ebird.org/MyEBird?",
             "cmd=list&rtype=custom&r=world&time=life&fmt=csv")

ui <- bootstrapPage(
  tags$head(
    tags$title("eBird Target Map"),
    includeCSS("www/style.css")
  ),
  
  # map
  leafletOutput("map", width = "100%", height = "100%"),
  
  # control panel
  absolutePanel(
    id = "controls", draggable = TRUE, top = 20, right = 20,
    fluidRow(column(12, h3("eBird Target Map", 
                           `data-toggle` = "collapse", 
                           `data-target` = "#colpanel"))),
    div(class = "collapse in", id = "colpanel",
        p("Display the number of lifers you could see in each country. Start ",
          "by downloading a ", a("CSV of your eBird life list", href = ll), 
          "and uploading it below. Select a checklist frequency threshold ",
          "to filter out birds that occur on fewer than the given percent of ",
          "checklists, for example to remove vagrants."),
        fileInput("ll_file", "Upload eBird life list",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        selectInput("month", "Month:", choices = months),
        sliderInput("freq_thresh", "Checklist frequency threshold", 
                    min = 0, max = 1, value = 0.1, step = 0.01, post = "%"),
        fluidRow(
          column(width = 4, actionButton("submit_button", "Submit!")),
          column(width = 8, uiOutput("message", inline = TRUE))
        ),
        div(
          tags$small("By ", 
                     a("Matt Strimas-Mackey", href = "http://strimas.com",
                       target = "_blank"),
                     " • Data from ",
                     a("eBird", href = "https://ebird.org", target = "_blank")
          ), 
          align = "center"
        )
    )
  )
)