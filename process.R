df <- read.csv("C:/Users/JSpector/Documents/Source Water Time Machine/20190619 DDW Source Points.csv")
df <- na.omit(df)

saveRDS(df, "C:/Users/JSpector/Documents/Source Water Time Machine/data.rds")



