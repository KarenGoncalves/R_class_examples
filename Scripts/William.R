# start by creating a vector with all the packages you need
pkgs = c("readxl", "tidyverse", "multcomp")
pkgs.To.Install = ! pkgs %in% installed.packages()
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 

# Define the theme for the plots
theme_heatmap = theme_bw(base_size = 10) +
	theme(panel.grid = element_blank(),
	      strip.background = element_blank())
theme_set(theme_heatmap)

#### Set basic variables ####
# Set the name of the input file and
WilliamFile = "Inputs/William_metabolites.xlsx"
sheetsWilliam = excel_sheets(WilliamFile)
# Get sheets with metabolite abundance over time
days = grep("day", sheetsWilliam, value = T)

#### Read input ####
# Get sheet cointaing sample information
metadata_input = read_excel(WilliamFile, sheet = sheetsWilliam[5], na = "NA")

# Load the sheets for abundance per day into a single object
input_days = sapply(days, simplify = F, \(sheet){
	read_excel(WilliamFile, sheet = sheet) |>
		pivot_longer(cols = !Metabolite,
			     # Separate the column names into the treatment and the replicate number
			     names_to = c("Supplementation", "Replicate"),
			     names_sep = "_",
			     values_to = "Metabolite_abundance") |>
		# Add the column day to identify the sheet from which we got the data
		mutate(Day = gsub("day_", "Day ", sheet))
}) |> list_rbind() # join all tables by attaching the rows

supplemented_days_means = input_days |>
	# Calculate the mean abundance for each metabolite, day and supplentation/treatment
	group_by(Metabolite, Supplementation, Day) |>
	summarise(meanAbundance = mean(Metabolite_abundance, na.rm = T))

maxAbundance_byMet = supplemented_days_means |>
	group_by(Metabolite) |>
	# get the maximum mean abundance for each metabolite
	summarize(maxAbundance = max(meanAbundance))

# Join the tables: metadata_input, supplemented_days_means and maxAbundance_byMet
# The function left_join joins two tables using the ids in the column 
# specified with "by" (if the columns have different names, we use "join_by()")
# It includes all the rows from the first table,
# and eliminates the ones from the right not present in the left one.
full_norm_supplemented_days =
	left_join(supplemented_days_means, 
		  maxAbundance_byMet,
		  by = "Metabolite") |>
	left_join(metadata_input[,-ncol(metadata_input)],
		  by = join_by("Supplementation" == "SampleName")) |>
	#' We use mutate to create a new column that binds 
	#' the treatment name and the concentration
	#' Since the control does not have a concentration, we need to skip it
	mutate(fullTreatment = 
	       	ifelse(Treatment == "Control",
	       	       "Control",
	       	       paste0(Treatment,"\n",Concentration)
	       	)
	)

#' Use the column "Order_days" to 
#' specify the order the treatments should appear
#' The function arrange sorts the table in relation to a column 
order_fullTreatment = 
	arrange(full_norm_supplemented_days, 
		by = Order_days)$fullTreatment |>
	unique()
full_norm_supplemented_days$fullTreatment =
	factor(full_norm_supplemented_days$fullTreatment,
	       levels = order_fullTreatment
	       )
		
# To normalize, we divide the mean by the maximum abundance,
# But, we need to prevent division by 0
full_norm_supplemented_days$normalizedAbundance = 
	with(full_norm_supplemented_days, 
	     (meanAbundance/maxAbundance) * 100
	)


# Create heatmap splitting the plots by the treatment
full_norm_supplemented_days |>
	ggplot(aes(x = Day,
		   y = Metabolite,
		   fill = normalizedAbundance)) + 
	geom_tile() + # the boxes to represent the data are tiles
	scale_fill_gradient(low = "white",
			    high = "black") + 
	facet_wrap(~fullTreatment) + 
	labs(fill = "Normalized\nabundance (%)")
ggsave("Plots/William_Day_by_treatment.pdf",
       width = 5.4,
       height = 3.5)

# Create heatmap splitting the plots by the day
full_norm_supplemented_days |>
	ggplot(aes(x = fullTreatment, # instead of Day
		   y = Metabolite,
		   fill = normalizedAbundance)) + 
	geom_tile() +
	scale_fill_gradient(low = "white",
			    high = "black") + 
	facet_wrap(~Day) + # instead of fullTreatment
	# below, the \n indicates where to split the text into 
	# different lines
	labs(fill = "Normalized\nabundance (%)") +
	theme(axis.title = element_blank())
ggsave("Plots/William_Treatment_day.pdf",
       width = 10.5,
       height = 3)

