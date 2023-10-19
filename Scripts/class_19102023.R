# start by creating a vector with all the packages you need
pkgs = c("tidyverse")
# We check which packages are NOT (!) installed
pkgs.To.Install = ! pkgs %in% installed.packages()
# any() checks if there is at least one TRUE in the vector
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 

# Set a theme for all future plots in this session
theme_set(theme_bw()) 

# Use different colors to fill and remove the legend
colors = c("red", "green", "blue")

ggplot(data = iris,
       aes(x = Species,
           fill = Species)) +
	geom_bar()


# Datasets about US states
US_statesInfo = data.frame(Name = state.name,
			   Region = state.region,
			   Division = state.division)
x = "Number of states"
# Plot the number of states in each division
ggplot(US_statesInfo, aes(fill = Division, y = Region)) +
	geom_bar(stat = "count") +
	xlab(x)

df <- data.frame(
	group = c("Male", "Female", "Child"),
	value = c(25, 25, 50)
)

ggplot(df, aes(x = group, y = value, fill = group)) +
	geom_col() +
	theme_classic()

# Pie chart
ggplot(df, aes(x = "", y = value, fill = group)) +
	geom_col() +
	coord_polar(start = 0) +
	theme_void()

# Barplot like geom_col
ggplot(df, aes(x = "", y = value, fill = group)) +
	geom_col() +
	coord_polar("y", start = 0) +
	theme_void()

# Barplot like geom_col
ggplot(df, aes(x = "", y = value, fill = group)) +
	geom_col() +
	coord_polar("y", start = 10) +
	theme_void()

# Barplot like geom_col
ggplot(df, aes(x = "", y = value, fill = group)) +
	geom_col() +
	coord_polar("y", start = 30) +
	theme_void()


# Barplot like geom_col
ggplot(df, aes(x = "", y = value, fill = group)) +
	geom_col() +
	coord_polar("y", start = 0)


# Barplot like geom_col
ggplot(df, aes(x = group, y = value, fill = group)) +
	geom_col() +
	coord_polar(start = 0)

# Barplot like geom_col
ggplot(df, aes(x = group, y = value, fill = group)) +
	geom_col() +
	coord_polar(start = 0) +
	theme(panel.border = element_blank())

# Barplot like geom_col
ggplot(df, aes(x = group, y = value, fill = group)) +
	geom_col() +
	coord_polar(start = 0) +
	theme(panel.border = element_blank(),
	      axis.title = element_blank(),
	      axis.ticks = element_blank())

# Barplot like geom_col
ggplot(df, aes(x = group, y = value, fill = group)) +
	geom_col() +
	coord_polar(start = 0) +
	theme(panel.border = element_blank(),
	      axis.title = element_blank(),
	      axis.ticks = element_blank(),
	      axis.text.y = element_blank())

# theme
theme_pie = theme_bw() +
	theme(panel.border = element_blank(),
	      axis.title = element_blank(),
	      axis.ticks = element_blank(),
	      axis.text.y = element_blank())

ggplot(df, aes(x = group, y = value, fill = group)) +
	geom_col() +
	coord_polar(start = 0) +
	theme_pie


### Line chart lab ###

# Let's create a time series to plot

LabSize = data.frame(Isabel = c(0, 3, 5, 10, 30),
		     Hugo = c(2, 2, 6, 9, 9),
		     Year = seq(2014, 2022, 2))
LabSize.Long = pivot_longer(LabSize,
			    cols = !Year, # all columns from LabSize, except "Year"
			    names_to = "PI", 
			    values_to = "LabMembers")

ggplot(LabSize.Long, 
       aes(x = Year, y = LabMembers, color = PI)) +
	geom_line(linewidth = 2) +
	scale_color_manual(values = c("green", "red")) + 
	theme(axis.title.y = element_text(size = 20))
ggsave("Plots/LabSize_linechart.jpeg")
