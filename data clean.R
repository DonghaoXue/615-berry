

library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)

berry  = read.csv("berries.csv")

ag_data <- read_csv("berries.csv", col_names = TRUE)




ag_data %>% summarize_all(n_distinct) -> aa

bb <- which(aa[1,]==1)
cn <- colnames(ag_data)[bb]

ag_data %<>% select(-all_of(bb))

aa %<>% select(-all_of(bb)) 


ag_data %<>% select(-4)
aa %<>% select(-4)

kable(head(ag_data)) %>%
  kable_styling(font_size=12)



# STRAWBERRIES
sberry <- ag_data %>% filter((Commodity=="STRAWBERRIES") & (Period=="YEAR"))
sberry %<>% select(-c(Period, Commodity)) 

sum(str_detect(sberry$`Data Item`, "^STRAWBERRIES, ")) == length(sberry$`Data Item`)

di <- sberry$`Data Item`
di_m <- str_split(di, ",", simplify=TRUE)
dim(di_m)
unique(di_m[,1])
di_m <- di_m[,2:4]


sberry %<>% separate(`Data Item`, c("B","type", "meas", "what"), sep = ",") 
sberry %<>% select(-B)

sberry %<>% separate(type,c("b1", "type", "b2", "lab1", "lab2"), " ")

sberry %<>% select(-c(b1,b2)) 

sberry[is.na(sberry)] <- " " 


sberry %<>% separate(Domain, c("D_left", "D_right"), sep = ", ")


sberry[is.na(sberry)] <- " "

sberry %<>% separate(`Domain Category`, c("DC_left", "DC_right"), sep = ", ")

head(sberry$DC_left %>% unique(),n=20)
head(sberry$DC_right %>% unique(), n=20)

sberry %<>% separate(DC_left, c("DC_left_l", "DC_left_r"), sep = ": ")

head(sberry$DC_right %>% unique(), n=20)


sberry %<>% separate(DC_right, c("DC_right_l", "DC_right_r"), sep = ": ") 
sberry[is.na(sberry)] <- " "


sberry %<>%  select(-DC_left_l) 
sberry %<>%  select(-DC_right_l) 

sberry %<>% mutate(label = paste(lab1,lab2)) 

sberry %<>% mutate(D_left = "CHEMICAL", D_left = "") 



sberry %<>% mutate(Chemical=paste(D_left, D_right)) 

sberry %<>% select(-c(D_left, D_right)) 


sberry %<>% select(Year, State, type, what, meas, label, DC_left_r, DC_right_r, Chemical, Value )



##
cnt_1 <- str_detect(sberry$what, "MEASURED IN")
cnt_2 <- str_detect(sberry$meas, "MEASURED IN")

f1 <- function(a,b){
  if(a){
    return(b)
  }else{
    return("")
  }
}
f1_log <- c(FALSE, TRUE, TRUE)
f1_str <- c("one", "two", "three")

map2(f1_log, f1_str, f1)

index_meas <- str_detect(sberry$meas, "MEASURED IN")

head(index_meas)
new <- map2(index_meas, sberry$meas, f1)
new <- unlist(new)
head(new, n=20)


sberry %<>% mutate(m_in_1 = unlist(map2(index_meas, sberry$meas, f1))) 

sberry %<>% mutate(meas = str_replace(sberry$meas, "MEASURED IN.*$", ""))

cnt_3 <- str_detect(sberry$meas, "MEASURED IN")
sum(cnt_3)

index_what <- str_detect(sberry$what, "MEASURED IN")
sum(index_what)

sberry %<>% mutate(m_in_2 = unlist(map2(index_what, sberry$what, f1))) 

sberry %<>% mutate(what = str_replace(sberry$what, "MEASURED IN.*$", ""))

cnt_what <- str_detect(sberry$what, "MEASURED IN")
sum(cnt_what)

sberry %<>% mutate(units = str_trim(paste(m_in_1, m_in_2))) 

sberry$units %>% unique()



#clean
sberry$what %>% unique()  ## rename Avg

sberry$meas %>% unique()  ## rename marketing

sberry$label %>% unique() ## rename harvest 

sberry$DC_left_r %>% unique() # rename chemical_family

tmp <- sberry$DC_right_r %>% unique() # rename materials --213

tmp <- sberry$Value %>% unique() # values

tmp <- sberry$units %>% unique() # Measures


sberry %<>% rename(Avg = what)
sberry %<>% rename(Marketing = meas, Harvest = label, Chem_family = DC_left_r, Materials = DC_right_r, Measures = units)

colnames(sberry)

sberry %<>% select(Year, State, type, Marketing, 
                   Measures, Avg, Harvest, Chem_family,
                   Materials, Chemical, Value )

str_trim(paste(sberry$Marketing, sberry$Harvest)) %>% unique

sberry %<>% mutate(production = str_trim(paste(Marketing, Harvest)))

sberry %<>% select(Year, State, type, production, Measures,
                   Avg, Chem_family, Materials, Chemical, Value)

sberry %<>% mutate(Chemical = str_trim(paste(Chem_family, Chemical)))

sberry %<>% select(Year, State, type, production, Avg, Measures, Materials, Chemical, Value)


unfood <- sberry %<>% filter(production=="APPLICATIONS") 


unfood %<>% filter(Value != "(D)")

unfood %<>% filter(Value !=  "(NA)")

unfood %<>% filter(Measures == "MEASURED IN LB / ACRE / APPLICATION")


unfood$type %>% unique()                 
unfood$production %>% unique()            
unfood$Avg %>% unique()                   
unfood$Measures %>% unique()              
unfood$Materials %>% unique()      
unfood$Chemical %>% unique()

unfood_1 <- unfood %>%  select(Year, State, Chemical, Value)

unfood_1 %<>% pivot_wider(names_from = Chemical, values_from = Value)

a <- unfood_1$FUNGICIDE


sum((sberry$Value == "(D)") | (sberry$Value == "(NA)"))

write.csv(sberry, file="D:/sberry.csv")



