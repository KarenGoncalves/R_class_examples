# start by creating a vector with all the packages you need
pkgs = c("readxl", "tidyverse", "ggpubr")
# We check which packages are NOT (!) installed
pkgs.To.Install = ! pkgs %in% installed.packages()
# any() checks if there is at least one TRUE in the vector
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 
# curPkg is a variable that takes the value of each element in pkgs
theme_set(theme_bw())

#### Set basic variables ####
# Set the name of the input file and
ElisaFile = "Inputs/ELISA_YFP_Cys & YPF clones_EF20230330.xls"
columnNames = c("chlorophyll_pos", "YFP_pos", "Mean_YFP_pos")
colors = c("black", "blue", "forestgreen",
	   "red", "magenta", "orange")
chlorophyll_threshold = 75
pValue_threshold = .01
sheetsElisa = excel_sheets(ElisaFile)

#### Read input file and prepare it ####
Elisa_Chlorophyll_YFP = lapply(sheetsElisa, \(sheet) {
	# All the sheets will be saved into a list
	currentSheet = read_excel(ElisaFile, sheet = sheet, na = "n/a")
	if (sheet != "Metadata") { 
		# Just execute the code inside the {} if sheet is not "Metadata"
		# Create a column to indicate the technical replicates (name of the sheet)
		currentSheet = currentSheet %>%
			rename(Clone = "...1") %>%
			mutate(Technical_replicate = sheet,
			       Plate = case_when(
			       	Clone == "Sh ble_plate1" ~ "Plate1",
			       	Clone == "Sh ble_plate2" ~ "Plate2",
			       	grepl("YFP_\\d+", Clone) ~ "Plate2",
			       	.default = "Plate1"
			       ),
			       Control = grepl("Sh", Clone)
			)
		controlData = currentSheet %>% filter(Control == T)
		normalizedData = sapply(1:nrow(controlData), simplify = F, \(ROW_control) {
			PLATE = controlData$Plate[ROW_control]
			YFP_control = controlData$Mean_YFP_pos[ROW_control]
			currentSheet %>% filter(Plate == PLATE) %>%
				mutate(Normalized_MFI_YFP = Mean_YFP_pos - YFP_control,
				       NonNeg_Normalized_MFI_YFP = ifelse(Normalized_MFI_YFP < 0,
				       				   0, Normalized_MFI_YFP))
		}) %>% list_rbind()
		currentSheet = normalizedData
	}
	currentSheet
})

names(Elisa_Chlorophyll_YFP) = sheetsElisa

# Specify which sheet doesn't contain measurements
notMetadata = sheetsElisa != "Metadata"
# Put all the sheets containing measurements into one table
Elisa_Chlorophyll_YFP_all = 
	list_rbind(Elisa_Chlorophyll_YFP[notMetadata])

keptReplicates = (aov(chlorophyll_pos ~ Technical_replicate,
    data = Elisa_Chlorophyll_YFP_all) %>%
	TukeyHSD())[[1]] %>% data.frame %>%
	filter(p.adj < pValue_threshold) %>% rownames %>%
	str_split_i(pattern = "-", i = 1) %>% unique

filtered_Reps = 
	Elisa_Chlorophyll_YFP_all %>%
	filter(Technical_replicate  %in% keptReplicates)


#### Summarize filter ####
mean_Chlorophyll = filtered_Reps %>%
	group_by(Clone) %>%
	summarise(Mean_chlorophyll = mean(chlorophyll_pos, na.rm = T)) %>%
	filter(Mean_chlorophyll < chlorophyll_threshold)

filtered_Reps_HighChloro = 
	filtered_Reps %>%
	filter(!Clone %in% mean_Chlorophyll$Clone) %>%
	mutate(Construct = case_when(
		Control == T ~ "EV",
		grepl("YFP_\\d+", Clone) ~ "YFP",
		.default = "PtCys_YFP"
	) %>% factor(levels = c("EV", "YFP", "PtCys_YFP"))
	)

#### PLOTS ####
filtered_Reps_HighChloro %>% 
	filter(Construct != "EV") %>%
	ggplot(aes(x = Construct, y = NonNeg_Normalized_MFI_YFP)) +
	geom_boxplot(notch = T) +
	labs(y = "\u0394MFI YFP")
