## Script to parse xlsx files
# extracts observations and variable names by selecting specific values of interest
# creates csv files for each value of interested
# e.g., value==10, it creates csv files to store all variable and observation names with value==10.

# load packages
library(openxlsx) # to install it: install.packages("openxlsx", dependencies = TRUE) # just run once

# input parameters
workingdir <- ''
setwd(workingdir)
filename <- file.path('.xlsx')
sheetname <- ''
outfile <- ''
values <- c() # enter values of interest, e.g., values <- c(0,5,10)

# function that parses the file by looking for selected values, and writes a csv output file
parsing <- function(values){
  for (i in seq_along(values)){
    # read input file xlsx
    file <- read.xlsx(filename, sheet = sheetname, rowNames = T)
    # search for values of interest and generate list of lists, one list per protein
    outlist <- lapply(file,function(x) {x == values[i]})
    # function to convert unwanted values to NA values, it keeps sample names of interested values
    def <- function(cellvalue){
      ifelse(cellvalue == T, rownames(file), NA)
    }
    # apply function to convert sample names, and keep interested sample names, if value is = to selected value
    outlist <- lapply(outlist,def)
    # remove NA values from list of lists
    outlist <- lapply(outlist,function(x) {x[!is.na(x)]})
    # from list to formatted list table
    outfinal <- format(outlist)
    # write csv output file
    write.table(outfinal,paste0(outfile,values[i],".csv"), sep = ',',col.names = F)
  }
}
# run function with values of interest, already selected
parsing(values)
