library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)


pop_2011=read_csv("D:/sem4/data science for developers/assingments/population/Population2011.csv")

pop_2020=pop_2011
pop_2020$Population=as.integer(pop_2020$Population*1.00561255390388033)


pop_2021=pop_2020
pop_2021$Population=as.integer(pop_2021$Population*1.00561255390388033)

pop_2022=pop_2021
pop_2022$Population=as.integer(pop_2022$Population*1.00561255390388033)

pop_2023=pop_2022
pop_2023$Population=as.integer(pop_2023$Population*1.00561255390388033)

sum(pop_2011$Population)
sum(pop_2020$Population)
sum(pop_2021$Population)
sum(pop_2022$Population)
sum(pop_2023$Population)


write_csv(pop_2020,"D:/sem4/data science for developers/assingments/pop2020.csv",append = FALSE)
write_csv(pop_2021,"D:/sem4/data science for developers/assingments/pop2021.csv",append = FALSE)
write_csv(pop_2022,"D:/sem4/data science for developers/assingments/pop2022.csv",append = FALSE)
write_csv(pop_2023,"D:/sem4/data science for developers/assingments/pop2023.csv",append = FALSE)


View(pop_2020)
