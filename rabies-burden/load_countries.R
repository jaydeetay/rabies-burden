# Loads the countries

if (!exists("country_data")) {
  country_data=read.csv("vcountry2.csv", row.names = 2)
}
