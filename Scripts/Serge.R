# start by creating a vector with all the packages you need
pkgs = c("readxl", "tidyverse", "multcomp", "emmeans", "lme4")
pkgs.To.Install = ! pkgs %in% installed.packages()
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 

theme_set(theme_bw())

#### Set variables ####
Treatments = c("Control", "Compound S 2.5 uM")
measurement = "OD750"

#### Load input file ####
inputFile = "Inputs/Serge_cellGrowth.xlsx"
sheetsInput = excel_sheets(inputFile)

input = sapply(sheetsInput, simplify = F, \(sheet) {
	read_excel(inputFile, sheet) %>%
		# The Replicates are in wide format, so we put them in long format
		pivot_longer(cols = all_of(paste0("Rep", 1:3)), 
			     names_to = "Replicate", 
			     values_to = measurement) %>%
		# Since we will join all the data sheets, 
		# we need to include a column with the time point
		mutate(TimePoint = gsub("h", "", sheet) %>%
		       	as.numeric,
		       # We create a new columns for the treatment
		       # Using case_when to give conditions on how to replace the values from Treatment
		       Treatment_name = 
		       	factor(
		       		case_when(
		       			# If you find "S_0uM" in the column Treatment, replace it with Treatments[1]
		       			Treatment == "S_0uM" ~ Treatments[1],
		       			# If you find "S_2.5uM" in the column Treatment, replace it with Treatments[2]
		       			Treatment == "S_2.5uM" ~ Treatments[2]
		       		), levels = Treatments
		       	)
		)
		
}) %>% list_rbind # bind the spreadsheets on top of each other

# Order clones by numbers
cloneNames = grep(pattern = "\\d", value = T, input$SampleID) %>%
	gsub(pattern = "C", replacement = "") %>%
	unique() %>% 
	as.numeric() %>% 
	sort()
	
# Organize the data
input$SampleID = factor(input$SampleID,
			levels = c("WT", "EV", 
				   paste0("C", cloneNames))
)

input = input %>%
	arrange(Treatment_name, SampleID)
## Calculate the mean for each sample, treatment and timepoint and plot the curves

# in the column OD750, so we will find the lowest value of OD and add that value to every data point
value_to_add = -1 * min(input$OD750) 
# This will make the lowest value become 0

input = input %>% mutate(newOD750 = OD750 + value_to_add,
			 newOD750_NotNeg = ifelse(OD750 < 0,
						  0, OD750)
)
growthCurveData = input %>%
	#dplyr::select(!Treatment) %>%
	group_by(Treatment_name, SampleID, TimePoint) %>%
	summarize(meanOD = mean(newOD750, na.rm = T),
		  sdOD = sd(newOD750, na.rm = T),
		  meanOD_NotNeg = mean(newOD750_NotNeg, na.rm = T),
		  sdOD_NotNeg = sd(newOD750_NotNeg, na.rm = T)
	)

# Plot growth curve
growthCurveData %>% 
	ggplot(aes(TimePoint/24, color = Treatment_name)) +
	geom_line(aes(y = meanOD), linewidth = 1) +
	geom_errorbar(aes(ymin = meanOD - sdOD,
			  ymax = meanOD + sdOD),
			  # use linewidth for the boldness of the line
			  # use width for its horizontal length
			  linewidth = 1, width = .5) +
	scale_x_continuous(breaks = 0:7) +
	labs(x = "Hours post treatment",
	     y = measurement,
	     color = "") +
	# Use facet_wrap to split the plots by SampleID
	# dir = "h" means that the plot matrix will be filled horizontally
	# check how it changes using dir = "v"
	# ncol is the number of columns of plots desired
	facet_wrap(~SampleID, dir = "h", ncol = 4) +
	theme(legend.position = "bottom")
ggsave("Plots/Serge_lineplots.pdf",
       height = 5.6, width = 8)

# Compare curves
# A warning message comes up because there are negative values

TimePoints = unique(input$TimePoint)

exponential.models = 
	# We will test for each clone if there is difference between the treatments over time
	sapply(levels(input$SampleID), simplify = F, \(clone) {
		model = with(filter(input, SampleID == clone), 
			# We use the function lmer from lme4 package
			# because the OD grows exponentially, we need to do log transformation
			# We add 0.01 to the new OD values to eliminate 0s
			# The dependent variable is the OD
			# The independent variables are TimePoint and Treatment_name
			# and there is a random error related to the TimePoints
			     lmer(log(newOD750+0.01) ~ 
		     	   	TimePoint*Treatment_name + (1|TimePoint))
		)
		# Now, using the model created, we compare the treatments
		# in relation to the variable TimePoint at all the timepoints measured
		emtrends(model, "Treatment_name",
			 var = "TimePoint", at = list(TimePoint = TimePoints)) %>%
			pairs
		# We use pairs to get the pvalue
		})

exponential.models # To see the results
# Compare the results with the plots to see if they make sense
