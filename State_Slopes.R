library(data.table);library(dplyr);library(ggplot2);library(lme4)
dat=fread('https://raw.githubusercontent.com/kylekocak/USDA-NASS/master/SoyNASS19702018.csv')

these=dat[Acres_Planted<=5000& Year==1974,FIPS]


total_acres=lmList(Yield ~ Year | State, data=dat)
new_acres=lmList(Yield ~ Year | State, data=dat[(FIPS %in% these)])
old_acres=lmList(Yield ~ Year | State, data=dat[!(FIPS %in% these)])

total_a=data.table(State=rownames(coef(total_acres)),coef(total_acres))
old_a=data.table(State=rownames(coef(old_acres)),coef(old_acres))
new_a=data.table(State=rownames(coef(new_acres)),coef(new_acres))

full_join(old_a,new_a,by='State') %>% select(State,Old_Acres_GG = Year.x,New_Acres_GG = Year.y)
