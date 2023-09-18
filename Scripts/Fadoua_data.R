library(tidyverse)
library(readxl)

# Set the name of the file
FadouaInput = "Inputs/Fadoua_Electroporation_13-09-2023.xlsx"

# Define the theme of the plots
theme_set(theme_bw())

# Use the function excel_sheets from the package readxl to get the names of the sheets in the input
(sheetsFadoua = excel_sheets(path = FadouaInput))

Fadoua_electroporation = lapply(sheetsFadoua,\(sheet) { # repeat the code below for each element in sheetsFadoua
	currentSheet = read_excel(path = FadouaInput,
				  sheet = sheet,
				  col_names = T) %>%
		as.data.frame
	# The last column indicates what was measured in that sheet, but that is also the name of the sheet
	# So we can remove it
	currentSheet[14] = NULL
	names(currentSheet) = c("Well_letter", paste0("N_", 1:12))
	currentSheet
})
names(Fadoua_electroporation) = sheetsFadoua

# Get the names of the clones into a vector
Clones = unlist(Fadoua_electroporation$Well_ID[, -1]) %>%
	unname %>% unique() %>% sort

Fadoua_electroporation_tidy = lapply(sheetsFadoua[-1], \(sheet) {
	lapply(Clones, \(clone) {
		# Since all the sheets are organized in the same way
		# We can get the clones' measurement using their coordinates from the well id's table
		cloneWells = Fadoua_electroporation$Well_ID == clone
		
		# We unlist and unname the data so we do not need to bother with columns and rows in the selection
		unlistedData = Fadoua_electroporation[[sheet]] %>% unlist %>% unname
		data.frame(Clone = clone,
			   MeasurementType = sheet,
			   MeasurementValue = unlistedData[cloneWells])
	}) %>% do.call(what = "rbind") # The result is a list, so we join all the lists row by row
})  %>% do.call(what = "rbind") # same here

# Make sure that the measurements are read as numbers again
Fadoua_electroporation_tidy$MeasurementValue = 
	as.numeric(Fadoua_electroporation_tidy$MeasurementValue)

# Use grep the way you would use CTRL+F
Fadoua_electroporation_tidyRFU = Fadoua_electroporation_tidy[
	grep("MeanRFU", Fadoua_electroporation_tidy$MeasurementType),]

# The ^ indicates that the phrase begins that way "Read" is at the beginning
Fadoua_electroporation_tidyReads = Fadoua_electroporation_tidy[
	grep("^Read", Fadoua_electroporation_tidy$MeasurementType),]


ggplot(Fadoua_electroporation_tidyReads, 
       aes(MeasurementValue)) +
	geom_histogram() +
	facet_wrap(~MeasurementType)

#### Compare the two Reads

# Since we don't know if the variances are comparable, 
# We should indicate to t.test that they are not with var.equal = F
t.test(data = Fadoua_electroporation_tidyReads, 
       MeasurementValue ~ MeasurementType,
       var.equal = F)
