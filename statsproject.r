#For my R code, I changed the file path to a local directory in my computer called C:/Users/shind/Downloads/archive (5)/.RData]
#To run it on a different computer, remember to download the dataset linked on the written part. Click download on the top right and download the dataset as zip, then import it into RStudio.


# Load the data
df <- rbind(
  read.csv("05-2019.csv"),
  read.csv("06-2019.csv"),
  read.csv("07-2019.csv"),
  read.csv("08-2019.csv"),
  read.csv("09-2019.csv"),
  read.csv("10-2019.csv"),
  read.csv("11-2019.csv"),
  read.csv("12-2019.csv")
)

# Summary of the data
summary(df)

# Subset the data for specific carrier codes
dfSA <- subset(df, df$carrier_code %in% c("WN", "AA"))

# Perform a Welch Two Sample t-test
carrier_code <- df$carrier_code
departure_delay <- df$departure_delay
print(t.test(departure_delay ~ carrier_code, data = dfSA))

# Subset the data for multiple carrier codes
dfSADU <- subset(df, carrier_code %in% c("WN", "AA", "UA", "DL"))

# Perform ANOVA on departure delay by carrier code
summary(aov(departure_delay ~ carrier_code, data = dfSADU))

# Perform Bonferroni post-hoc test
bonf <- pairwise.t.test(dfSADU$departure_delay, dfSADU$carrier_code, p.adjust.method = "bonferroni")
print(bonf)

# Calculate Pearson correlation between departure delay and flight number
print(cor.test(departure_delay, df$flight_number))

# Downsample the data for regression
newdf <- df[seq(1, nrow(df), by = 5000), ]

# Perform full and reduced regression models
full <- lm(newdf$departure_delay ~ newdf$carrier_code * newdf$origin_airport, data = newdf)
reduced <- lm(newdf$departure_delay ~ newdf$carrier_code + newdf$origin_airport, data = newdf)

