library(data.table);library(dplyr)
dat=fread('../Downloads/County_SqMileage.csv',skip = 1,select = c('Geographic area','Target Geo Id2','Area in square miles - Land area'))
yield1=fread('../Downloads/Soy_Yield_1990_2018.csv')
yield2=fread('../Downloads/Soy_Yield_1970_1989.csv')
planted1=fread('../Downloads/Soy_AcresPlanted_1990_2018.csv',stringsAsFactors = F)
planted2=fread('../Downloads/Soy_AcresPlanted_1970_1989.csv',stringsAsFactors = F)
yield=rbind(yield1,yield2)
planted=rbind(planted1,planted2)
yield$ST=stringr::str_pad(yield$`State ANSI`,2,pad = 0)
yield$CT=stringr::str_pad(yield$`County ANSI`,3,pad = 0)
planted$ST=stringr::str_pad(planted$`State ANSI`,2,pad = 0)
planted$CT=stringr::str_pad(planted$`County ANSI`,3,pad = 0)
planted$Value=as.numeric(gsub(",", "", planted$Value))
dat$FIPS=stringr::str_pad(dat$`Target Geo Id2`,5,pad = 0)

yield_final=yield %>% filter(!is.na(CT),!is.na(ST)) %>%
  tidyr::unite(.,'FIPS',c('ST','CT'),sep='') %>%
  select(FIPS,Yield := Value,Year,CT = County,ST =State) %>% as.data.table
yield_final$State=sapply(yield_final$ST,capFirst)
yield_final$County=sapply(yield_final$CT,capFirst)
dat2=dat %>% select(FIPS,Land_Area := `Area in square miles - Land area`) %>%
  filter(.,!is.na(FIPS))

planted_final=planted %>% filter(!is.na(CT),!is.na(ST)) %>%
  tidyr::unite(.,'FIPS',c('ST','CT'),sep='') %>%
  select(FIPS,Acres_Planted := Value,Year)

yield_FIPS=left_join(yield_final,dat2) %>% select(-ST,-CT) %>% as.data.table
final_FIPS=data.table(left_join(yield_FIPS,planted_final))
final_FIPS %>%
  filter(!is.na(Acres_Planted)) %>%
  group_by(Year) %>%
  arrange(-Acres_Planted) %>%
  mutate(perc_of_usa=round(cumsum(Acres_Planted)/sum(Acres_Planted) * 100,2),
         size_adjusted=round(Acres_Planted/(Land_Area*640),2),
         yield_over_usa=round(Yield/mean(Yield),2)) %>%
  filter(Year==1974) %>% data.table %>%
  head(n=100)

#fwrite(yield_acreage_FIPS,'SoyNASS19702018.csv')
capFirst =function(x){
  s=strsplit(x,' ')[[1]]
  paste0(toupper(substring(s, 1,1)), tolower(substring(s, 2)),collapse = ' ')
  #return(z)
  }

