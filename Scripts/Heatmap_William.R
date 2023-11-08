# start by creating a vector with all the packages you need
pkgs = c("readxl", "tidyverse", "multcomp")
pkgs.To.Install = ! pkgs %in% installed.packages()
if (any(pkgs.To.Install)) install.packages(pkgs[pkgs.To.Install])
for (curPkg in pkgs) library(curPkg, character.only = T) 

# Select sheets
inputFile = "Inputs/William_metabolites.xlsx"
sheets = excel_sheets(inputFile)
sheetsDays = grep("day", sheets, value = T)

input = sapply(sheetsDays, simplify = F, \(curSheet) {
	read_excel(path = inputFile,
		   sheet=curSheet) %>%
		pivot_longer(cols = !Metabolite,
			     names_sep = "_",
			     names_to = c("Treatment", "Replicate"),
			     values_to = "Metabolite_abundance") %>%
		mutate(Day = curSheet %>% gsub(pattern = "day_",
					       replacement = "") %>%
		       	as.numeric)
}) %>% list_rbind # do.call(what = "rbind")

metadata = read_excel(path = inputFile,
		      sheet=sheets[length(sheets)])

input %>% 
	ggplot(aes(x = Treatment,
		   y = Metabolite,
		   fill = Metabolite_abundance)) +
	geom_tile() +
	facet_wrap(~Day)

metabolite_stats = 
	input %>% group_by(Metabolite, Treatment, Day) %>%
	summarise(Mean_abundance = mean(na.rm = T, Metabolite_abundance),
		  sd_abundance = sd(na.rm = T, Metabolite_abundance))


max_input = 
	metabolite_stats %>%
	group_by(Metabolite) %>%
	summarize(Metabolite_abundance = 
		  	max(Mean_abundance,
		  	    na.rm = T))

normalized_input =
	sapply(max_input$Metabolite, simplify = F, \(met) {
	max_value = (max_input %>%
		filter(Metabolite == met))$Metabolite_abundance
	met_abundance = metabolite_stats %>%
		filter(Metabolite == met) %>%
		mutate(Normalized_abundance =
		       	Mean_abundance / 
		       	max_value
		       )
}) %>% list_rbind

normalized_input %>% 
	ggplot(aes(x = Treatment,
		   y = Metabolite,
		   fill = Normalized_abundance)) +
	geom_tile() +
	scale_fill_gradient(high = "black",
			    low = "white",
			    na.value = "white") +
	facet_wrap(~Day)

normalized_input %>%
	mutate(Zero_to_na = ifelse(Normalized_abundance == 0,
				   NA, Normalized_abundance)) %>%
	ggplot(aes(x = Treatment,
		   y = Metabolite,
		   fill = Zero_to_na)) +
	geom_tile() +
	scale_fill_gradient(high = "black",
			    low = "grey70",
			    na.value = "white") +
	facet_wrap(~Day)
