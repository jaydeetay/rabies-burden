# Custom run of the burden model across all countries with custom country data.
# To run this with R studio set the home directory to the location of the R files with
# Session/set working directory

print("Custom burden model run")
# This data is calculated by burden_1.R - we will assume this a given for
# now.
pPEPcountry <- read.csv("pPEPzeroMDV.csv", row.names = 2)
# Strip non-numeric data from pPEPcountry as it buggers up the editable table types.
# Actually - don't do this just yet - we'll show the data horizontally for now pPEPcountry <- pPEPcountry[,!(colnames(pPEPcountry) %in% c("NUM","Country","Cluster","Continent","Code","CODE_GDP", "WHO"))]

print("Sourcing burden model")
source('burden_model.R')
print("Burden model sourced")
source('helper.R')
print("Running calculation...")
output = calculate_burden(countryCode = NULL, provideProgress = FALSE,
                 customPPEPdata = pPEPcountry)
print("...done.")
output_filename = 'louises_output.csv'
write.csv(output, output_filename, row.names=TRUE)
cat("Results written to ", output_filename)
