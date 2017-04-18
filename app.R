# ---------------------------------------- #
# LIBRARIES
# ---------------------------------------- #

library(dplyr)
library(ggplot2)
library(magrittr)
library(reshape2)
library(ggvis)

# ---------------------------------------- #
# CLEAN AND TRANSFORM DATA 
# ---------------------------------------- #
# READ DATA ------------------------------ #

world <- read.csv('data2.csv', stringsAsFactors = FALSE)
region <- read.csv('Metadata_Country.csv', stringsAsFactors = FALSE)

# RENAME COLUMNS ------------------------- #

r <- seq(1960, 2016, 1)
y <- rep("YR",length(r))
yr <- paste(y,r, sep='')
names(world) <- c("Series.Name", "Series.Code", "Country.Name", "Country.Code", yr)
cols <- names(world)

# TRANSFORM DATA TYPES ------------------------- #

for (c in cols[5:length(cols)]){ 
  world[[c]] <- as.numeric(as.character(world[[c]]))
}

world$Series.Code <-  ifelse(world$Series.Code == "SP.DYN.LE00.IN", "life_exp", 
                       ifelse(world$Series.Code == "SP.DYN.TFRT.IN", "fert_rate","tot_pop" ))
world <- world[,names(world)!="Series.Name"]

world <- melt(world, id.vars = c('Series.Code', 'Country.Name', 'Country.Code'))
world <- dcast(world, ... ~ Series.Code)

world$variable <- as.numeric(substr(as.character(world$variable),3,6))
names(world)[3] <- "Year"
world <- world[rowSums(is.na(world)) == 0,]

region <- region[,!(names(region) %in% c("IncomeGroup", "SpecialNotes", "TableName", "X"))]
region$Country.Code <- as.character(region$Country.Code)

# JOIN REGION TO DATA ------------------------- #
world <- merge(x = world, y = region, by = "Country.Code", all.x = TRUE)
world$Region <- as.factor(world$Region)
world$id <- 1:nrow(world)

# --------------------------------------- #
# PLOTS
# ---------------------------------------- #

# ---------------------------------------- #
# MAKE HOVER PLOT
# ---------------------------------------- #

library(shiny)
library(ggvis)

ui <- fluidPage(
  headerPanel('Country Data'),
  
  fluidRow(
    # Slider Input for Year - Animated
    column(8,
           uiOutput("ggvis_ui"),
           ggvisOutput("ggvis")
           ),
    column(4, 
           sliderInput("time", "Year", min=1960, max=2014, value=1960, step=1, ticks=FALSE,
                       sep='',
                       animate = animationOptions(interval = 100,
                                                  playButton = icon('play', "fa-1x"),
                                                  pauseButton = icon('pause', "fa-1x"))),
           sliderInput("pop", label="Population", min = 1000, max = 10000, value = 10, ticks = FALSE)
          )
        )
      )


server <- function(input, output){
  
    vis <- reactive({

      new_world <- world[world$Year == input$time,]
      new_world$tot_pop_norm  <- (new_world$tot_pop/max(new_world$tot_pop, na.rm=TRUE))*input$pop
    
      hover_labels <- function(x) {
        if(is.null(x)) return(NULL)
        data <- world[world$id == x$id,]
        paste0(c(data$Country.Name, "Fertility  Rate: ", "Life Expectancy: "),
               c('', format(data$fert_rate, digits=2), format(data$life_exp, digits=3)),  collapse = "<br />")
      }
      
    new_world %>%
      ggvis(x=~life_exp, y=~fert_rate, key := ~id) %>%
      layer_points(size:=~tot_pop_norm, fill=~Region) %>%
      add_tooltip(hover_labels, on = "hover") %>%
      add_legend(c("fill"), title = "Region") %>%
      add_axis("x", title = "Life Expectancy") %>%
      add_axis("y", title = "Fertility Rate") %>%
      scale_numeric("y", domain=c(0,9), nice=FALSE)%>%
      scale_numeric("x", domain=c(19,90), nice=FALSE) %>%
      hide_legend(c('size', 'stroke'))
     })
     
    vis %>%  bind_shiny("ggvis", "ggvis_ui")
}

shinyApp(ui = ui, server = server)

