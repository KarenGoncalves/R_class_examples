# start by creating a vector with all the packages you need
pkgs = c("lsmeans", "tidyverse")
# We check which packages are NOT (!) installed
pkgs.To.Install = ! pkgs %in% installed.packages()
# any() checks if there is at least one TRUE in the vector
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 

# Set a theme for all future plots in this session
theme_set(theme_bw()) 

##### Bar plot #####
# Datasets about US states
US_statesInfo = data.frame(Name = state.name,
			   Region = state.region,
			   Division = state.division)
x = "Number of states"
# Plot the number of states in each division
ggplot(US_statesInfo, aes(y = Division, 
			  fill = Region)) +
	geom_bar(stat = "count") +
	xlab(x)

#' If the categories are in the x axis,
#' we need to rotate the text to read it
ggplot(US_statesInfo, aes(x = Region)) +
	geom_bar(stat = "identity") +
	ylab(x) +
	theme(axis.text.x = element_text(angle = 45,
					 vjust = 1, 
					 hjust = 1))


#### Line chart ####
# Let's create a time series to plot

LabSize = data.frame(Isabel = c(0, 3, 5, 10, 30),
		     Hugo = c(2, 2, 6, 9, 9),
		     Year = seq(2014, 2022, 2))


LabSize.Long = pivot_longer(LabSize,
			    cols = !Year, 
			    # all columns from LabSize, except "Year"
			    names_to = "PI", 
			    values_to = "LabMembers")

ggplot(LabSize.Long, 
       aes(x = Year, y = LabMembers, color = PI)) +
	geom_line()

# We can add the value measured at each time point with geom_text/geom_label
# We use nudge_y to push the text in the y axis, so it doesn't overlap with the line
ggplot(LabSize.Long, 
       aes(x = Year, y = LabMembers, color = PI)) +
	geom_line() +
	geom_text(aes(label = LabMembers), 
		  nudge_y = 2)

#### Time points with replicates

# Load the data
timeSeries.File = "https://karengoncalves.github.io/Programming_classes/r/exampleData/TimeSeries_example.csv"
timeSeries = read.csv(timeSeries.File)

# Let's rename the first column that indicate the time points
names(timeSeries)[1] = "TimePoint" 
timeSeries$TimePoint = 
	gsub("Day_", "", timeSeries$TimePoint) |>
	as.numeric()

timeSeriesLong = pivot_longer(
	timeSeries,
	cols = !TimePoint, 
	names_to = c("Treatment", "Replicates"),
	names_sep = "_",
	values_to = "Growth_measure"
)

timeSeriesLong |>
	ggplot() +
	geom_line(aes(x = TimePoint, y = Growth_measure,
		      color = Treatment)) +
	scale_x_continuous(breaks = 1:12)

GrowthMeasureStats = timeSeriesLong %>%
	group_by(Treatment, TimePoint) %>%
	summarise(
		Mean = mean(Growth_measure),
		StdEnv = sd(Growth_measure)
	)

GrowthMeasureStats |>
	ggplot(aes(x = TimePoint,
		   color = Treatment)) +
	geom_line(aes(y = Mean)) +
	scale_x_continuous(breaks = 1:12) +
	geom_errorbar(aes(ymin = Mean - StdEnv,
			  ymax = Mean + StdEnv),
			  width = 0.5)

library(lsmeans)
# Create the linear model of Growth_measure ~ TimePoint, adding the Treatment interaction
linear.model = lm(Growth_measure ~ TimePoint*Treatment, data = timeSeriesLong)
summary(linear.model)

# Calculate the confidence levels for the coefficients of the linear models
linear.model.trends <- lstrends(linear.model, "Treatment", var="TimePoint")
summary(linear.model.trends)

# Compare the coefficients of the different groups of the linear model
pairs(linear.model.trends)
