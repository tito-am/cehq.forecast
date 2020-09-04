require(CSHShydRology)
require(tidyhydat)
require(dplyr)
require(ggplot2)
require(lubridate)

options(scipen = 999)

#download_hydat()

mfile <- system.file("extdata", "04JD005_Daily_Flow_ts.csv", package = "CSHShydRology")
mdata <- ch_read_ECDE_flows(mfile)

stations_total<-hy_stations()

QC_stns <-stations_total %>%
  filter(HYD_STATUS == "ACTIVE") %>%
  filter(PROV_TERR_STATE_LOC == "QC") %>%
  pull_station_number()

debits_journaliers<-hy_daily_flows(station_number = QC_stns)
#debit journalier moyen
Qmoyen_journalier<-debits_journaliers %>%group_by(STATION_NUMBER)%>%summarise(Qmoy = mean(Value, na.rm = TRUE))
#debit journalier median
Qmedian_journalier<-debits_journaliers %>%group_by(STATION_NUMBER)%>%summarise(Qmed = median(Value, na.rm = TRUE))
#debit journalier median

#debit moyen annuel
Qmoyen_annuel<-debits_journaliers%>%mutate(MOIS=month(Date),ANNEE=year(Date)) %>%group_by(STATION_NUMBER,MOIS)%>%summarise(Qmoy = mean(Value, na.rm = TRUE))

Qmodule<-list()

for(bv in unique(Qmoyen_annuel$STATION_NUMBER)){
  Qmoyen_annuel_par_bv<-Qmoyen_annuel%>%filter(STATION_NUMBER==bv)
  Qmodule[bv]<-(Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==1,'Qmoy']*31+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==2,'Qmoy']*28+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==3,'Qmoy']*31+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==4,'Qmoy']*30+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==5,'Qmoy']*31+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==6,'Qmoy']*30+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==7,'Qmoy']*31+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==8,'Qmoy']*31+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==9,'Qmoy']*30+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==10,'Qmoy']*31+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==11,'Qmoy']*30+
    Qmoyen_annuel_par_bv[Qmoyen_annuel_par_bv$MOIS==12,'Qmoy']*31)/365

}

Qmodule_df<-as.data.frame(do.call(rbind,Qmodule))
df2<-tibble::rownames_to_column(Qmodule_df, "VALUE")
colnames(df2)<-c('STATION_NUMBER','Qmodule')

Qmodule_df2<-df2

df_final_part1<-merge(Qmodule_df2,Qmoyen_journalier,by='STATION_NUMBER')
df_final<-merge(df_final_part1,Qmedian_journalier,by='STATION_NUMBER')

readr::write_csv2(df_final,'comparaison_stat_sommaires_hydat_qc.csv')
