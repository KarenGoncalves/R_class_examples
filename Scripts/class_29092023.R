library(readxl)
ELISA = "Inputs/ELISA_YFP_Cys & YPF clones_EF20230330.xls"

sheetsElisa = excel_sheets(ELISA)

# sheetToRead = sheetsElisa[1]
# 
# ELISA_YFP_Cys_YPF_clones_EF20230330_1 = 
# 	read_excel(path = ELISA, sheet = sheetToRead, na = "n/a")
# 

#dataSheets = sheetsElisa[1:6]

ELISA_YFP_Cys = sapply(simplify = F,
		       sheetsElisa,
		       \(sheet) {
		       	read_excel(path = ELISA, sheet = sheet, na = "n/a")
		       })
