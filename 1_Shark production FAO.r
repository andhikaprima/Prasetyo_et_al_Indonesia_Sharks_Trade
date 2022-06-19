## Scripts related to manuscript titled 
## Shark and ray trade in and out of Indonesia: Addressing knowledge gaps on the path to sustainability
#
# https://doi.org/10.1016/j.marpol.2021.104714

# Library
library(readxl)
library(ggplot2)

CatchData <- read_excel("1_production/Indo_Production_FAO.xlsx", sheet = "Indo_Production_FAO")
CatchData <- data.frame(CatchData)
CatchData

# Basic plot
ggplot(data = CatchData, mapping = aes(x = Year, y = Amount_1000ton, color = Group)) + geom_line() +
  labs(x = "Year",
       y = "Volume (1,000 ton)")

# Modified plot
ggplot(CatchData, aes(x=Year, y=Amount_1000ton, group=Group)) +
  geom_line(aes(linetype=Group, color=Group, size=Group))+
  geom_point(size=1, color="black")+
  scale_linetype_manual(values=c("solid", "solid", "solid"))+
  scale_size_manual(values=c(1.5, 1.5, 2.5))


