library(ggplot2)

df = read.csv("G:\\STA313\\project_age_sex_prov_phc.csv")

# delete rows with non-applicable responses

# age
df1<-subset(df, dhhgage!=96 & dhhgage!=97 & dhhgage!=98 & dhhgage!=99)
# province of respondent
df2<-subset(df1, geo_prv!=96 & geo_prv!=97 & geo_prv!=98 & geo_prv!=99)
# sex
df3<-subset(df2, dhh_sex!=6 & dhh_sex!=7 & dhh_sex!=8 & dhh_sex!=9)
# remove column
df4<-subset(df3, select=-CASEID)

plot.by.province <- function(df, prov, prov_name) {
  df1 <- subset(df, geo_prv == prov)
  df2 <- subset(df1, select=-geo_prv)
  df_has_phc <- subset(df2, phc_020 == 1)
  
  phc_age <- c()
  for (i in 1:16) {
    count <- sum(df_has_phc$dhhgage == i & df_has_phc$dhh_sex == 1)
    phc_age <- append(phc_age, count)
    count <- sum(df_has_phc$dhhgage == i & df_has_phc$dhh_sex == 2)
    phc_age <- append(phc_age, count)
  }
  # make df to plot
  data <- data.frame(values = phc_age,
                     group = rep(c("12-14",
                                   "15-17",
                                   "18-19",
                                   "20-24",
                                   "25-29",
                                   "30-34",
                                   "35-39",
                                   "40-44",
                                   "45-49",
                                   "50-54",
                                   "55-59",
                                   "60-64",
                                   "65-69",
                                   "70-74",
                                   "75-79",
                                   "80+"), 
                                 each = 2),
                     subgroup = c("Male", "Female"))
  
  
  ggplot(data,aes(x=values,fill=subgroup)) + 
    geom_bar(subset=(data$subgroup=="Female")) + 
    geom_bar(subset=(data$subgroup=="Male"),aes(y=..count..*(-1))) + 
    scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
    coord_flip()
}

plot.by.province(df4, 10, "Newfoundland and Labrador")
plot.by.province(df4, 11, "Prince Edward Island")
plot.by.province(df4, 12, "Nova Scotia")
plot.by.province(df4, 13, "New Brunswick")
plot.by.province(df4, 24, "Quebec")
plot.by.province(df4, 35, "Ontario")
plot.by.province(df4, 46, "Manitoba")
plot.by.province(df4, 47, "Saskatchewan")
plot.by.province(df4, 48, "Alberta")
plot.by.province(df4, 59, "British Columbia")
plot.by.province(df4, 60, "Yukon")
plot.by.province(df4, 61, "Northwest Territories")
plot.by.province(df4, 62, "Nunavut")
