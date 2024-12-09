
#Housekeeping:
#Clear the workspace
rm(list = ls())

#Install and load relevant packages
required_packages <- c("tidyverse", "here", "ggplot2", "gganimate", "gifski", "kableExtra", "plotly") #creates vector with all required packages
all_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])] #creates new vector of packages not installed. ! finds packages not installed, %in% tests whether a package is installed
if (length(all_packages)) install.packages(all_packages) #installs missing packages
lapply(required_packages, library, character.only = TRUE) #loads each package                        

#NOTE:
#Please use code below to convert the into .csv format if required:
#if(!require(readxl)){install.packages("readxl");library(readxl)}
#data <- read_excel(here("data", "download1.xlsx"))

#If the data is downloaded from github, it should be in a folder called "data" and already exist in .csv format.

#Loading in the data and remove first 5 rows as this is just text information about the data
data <- read.csv(here("data","rawdata","trends in life expectancy.csv")) %>% slice(-1:-5)

#We should first remove all N/A values
data_filtered <- data %>% drop_na()

#Now we need to check there are no missing values
if(sum(is.na(data_filtered)) == 0) {
  print("No missing vaules")
} else {
  print(paste("Count of total missing values:", sum(is.na(data_filtered))))
}

#Remove header row and set first row as column names
colnames(data_filtered) <- data_filtered[1, ]
data_filtered <- data_filtered[-1, ]
colnames(data_filtered) <- make.names(colnames(data_filtered))

#Remove data relating to UCI and LCI, so that only healthy life expectancy values remain
data_cleaned <- data_filtered %>%
  select(-contains("LCI"), -contains("UCI"))

#Change names in the Year column
colnames(data_cleaned) <- gsub("^X", "", colnames(data_cleaned)) # Remove the "X" at the start
colnames(data_cleaned) <- gsub("\\.to\\.", "-", colnames(data_cleaned)) #Replace ".to." with "-"
colnames(data_cleaned) <- gsub("\\.HLE$", "", colnames(data_cleaned)) #Remove ".HLE" at the end

#Reshape dataset to long format
data_long <- data_cleaned %>% pivot_longer(cols = starts_with("2"), names_to = "Year", values_to = "Life.Expectancy")

#Sanity check: lets check the data has the first row as column names, displays the correct names, and only contains data for healthy life expectancy
#Display column names
colnames(data_long)

#Calculate midpoint for year and clean up columns
data_long <- data_long %>%
  mutate(
    Start_Year = as.numeric(substr(Year, 1, 4)),  # Extract the start year
    End_Year = as.numeric(substr(Year, 6, 9)),    # Extract the end year
    Midpoint_Year = (Start_Year + End_Year) / 2   # Calculate the midpoint
  ) %>% filter(!is.na(Midpoint_Year)) %>%
  select(-Year, -Start_Year, -End_Year) %>%
  rename(Year = Midpoint_Year) #Remove old "Year", "Start_Year", and "End_Year" columns, then rename Midpoint_Year column

#Check if year and life expectancy columns are numeric
print(sapply(data_long, is.numeric))

#Convert life expectancy to numeric
data_long <- data_long %>% mutate(Life.Expectancy = as.numeric(Life.Expectancy))

#Check data is now numeric
print(is.numeric(data_long$Life.Expectancy))

#Groups dataset by columns and allows flexibility with other datasets. Important if dataset is updated to Include Scotland
summarize_data <- function(data, group_var, value_var) {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarize(mean_value = mean(.data[[value_var]], na.rm = TRUE)) #Calculate mean across gender for each region and year
}

#Calls the summarise function and assigns it to data_final
data_final <- summarize_data(data_long, group_var = c("Area.name", "Year"), value_var = "Life.Expectancy") %>%
  filter(!is.nan(mean_value))  # Ensure no NaN values remain

#Change column names
colnames(data_final) <- c("Region", "Year", "Mean_Healthy_Life_Expectancy")

#Sanity check: Check data contains correct information and only the rows/columns required for visualisation exist
tableh <- knitr::kable(head(data_final, n = 12), "html", col.names = c("Region","Year","Mean_Healthy_Life_Expectancy"))
kableExtra:: kable_styling(tableh,bootstrap_options = c("striped", 'condensed', "scale_down" ),position = "left", font_size = 10) 

#Save the prepped dataset
write.csv(data_final, here("data", "prepped_data", "prepped_data.csv"), row.names = FALSE)

#The graph will contain a range of lines, and the 'custom_colors' function makes it easier to distinguish between the regions than the original colours
custom_colors <- c("red", "turquoise", "magenta", "chartreuse4", "darksalmon", "orange", "violetred3", "blue", "yellow", "purple", "darkcyan", "green")


#Designing plot
plot <- ggplot(data_final, aes(x = Year, y = Mean_Healthy_Life_Expectancy, color = Region, group = Region, 
                               text = paste("Region:", Region, # Defines custom tooltip for interactive plot
                                            "<br>Year:", Year, 
                                            "<br>Mean Healthy Life Expectancy:", 
                                            Mean_Healthy_Life_Expectancy))) + 
  geom_line(linewidth = 0.8, alpha = 0.5) + 
  scale_color_manual(values = custom_colors) + #Here I thinned the lines and added transparency as this makes it clearer to distinguish between the regions when overlapping and in close proximity 
  geom_point(size = 2) + 
  labs(title = "Health Life Expectancy Across the countries\nand 9 English Regions of the UK:", 
       x = "Year", 
       y = "Life Expectancy", 
       color = "Region") + 
  scale_y_continuous(limits = c(58, 68), 
                     breaks = seq(58, 68, by = 2)) + # Set axis limits and changed scale intervals to improve readability. I found these values gave the clearest representation
  scale_x_continuous(breaks = unique(data_final$Year)) + 
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "gray87", size = 0.5), #Changed the strength of the grid lines
    panel.background = element_rect(fill = "seashell", color = NA), #Changed the background colour of the graph
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_blank(),
    legend.box = "vertical",
    theme(axis.text.x = element_text(angle = 45, hjust = 1))# I ensured the x-axis labels remain rotated for better clarity
  )

#View plot
plot

#If you would like to save the plot:
ggsave(here("plots", "plot.png"), bg = "white", width = 10, height = 8, units = "in")

#Animating plot
animated <- plot +
  geom_point(size = 2, aes(group = seq_along(Year))) +
  transition_reveal(Year) +
  ggtitle('Health Life Expectancy Across the countries and 9 English Regions of the UK: {round(frame_along)}')


#This will allow the gif to be knit into the html document
if(knitr::is_html_output()){ anim_save(here("plots" , "animated.gif"), 
                                       animated, renderer = gifski_renderer())}

#Save the animated plot
anim_save(here("plots", "animated.gif"), animated, renderer = gifski_renderer())

#I have included the code for the interaction
#Making plot interactive
interactive_plot <- ggplotly(plot, tooltip = "text")

#Customising hover box
interactive <- interactive_plot %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 10, color = "black"), 
                           bordercolor = "blue"), title = list(text = "Health Life Expectancy Across the countries and 9 English Regions of the UK: "), margin = list(t = 50)) %>% 
  style(line = list(width = 1.5, opacity = 0.5),
        marker = list(size = 6, opacity = 0.4),
        mode = "line+markers")

#View interactive plot
interactive
