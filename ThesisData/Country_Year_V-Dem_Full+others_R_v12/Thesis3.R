library(tidyverse)
library(readr)
library(ggpubr)
vdem <- readRDS("~/Documents/data/ThesisData/Country_Year_V-Dem_Full+others_R_v12/vdem.rds")
ERT <- read_csv("ERT.csv")
vdem_lite<-vdem%>%select(country_name, 
                         year,  
                         country_text_id, 
                         v2x_polyarchy, 
                         v2x_libdem, 
                         v2x_partipdem, 
                         v2x_delibdem, 
                         v2x_egaldem, 
                         v2x_api, 
                         v2x_mpi, 
                         v2xel_frefair, 
                         v2x_jucon, 
                         v2xlg_legcon, 
                         v2elfrfair, 
                         v2psplats, 
                         v2exrescon, 
                         v2exbribe, 
                         v2exembez, 
                         v2excrptps, 
                         v2exthftps, 
                         v2exremhsp,  
                         v2exdfcbhs_rec, 
                         v2exdfvths, 
                         v2exdfpphs, 
                         v2exremhog,    
                         v2exdjdshg, 
                         v2exdjcbhg, 
                         v2exdfdshg, 
                         v2exdfvthg, 
                         v2exdfpphg, 
                         v2reginfo, 
                         v2regendtype,  
                         v2regint, 
                         v2regidnr, 
                         v2regdur,  
                         v2regimpgroup, 
                         v2regsupgroupssize, 
                         v2regsuploc,  
                         v2regpower, 
                         v2lginvstp, 
                         v2lginvstp, 
                         v2lgotovst, 
                         v2lgoppart, 
                         v2lgfunds, 
                         v2ex_hosw, 
                         v2jureform, 
                         v2jupurge, 
                         v2jupoatck, 
                         v2jupack, 
                         v2juaccnt, 
                         v2jucorrdc, 
                         v2juhcind, 
                         v2juncind, 
                         v2juhccomp, 
                         v2jucomp, 
                         v2csreprss, 
                         v2mecenefm, 
                         v2mecenefi, 
                         v2meharjrn, 
                         v2cacamps, 
                         v2caviol, 
                         v2x_regime, 
                         v2x_accountability, 
                         v2x_veracc, 
                         v2x_diagacc, 
                         v2x_horacc, 
                         v2x_corr, 
                         v2x_execorr, 
                         v2x_pubcorr, 
                         v2x_rule, 
                         v2xcl_acjst, 
                         v2xcl_prpty, 
                         v2xps_party, 
                         v2x_divparctrl, 
                         v2x_feduni, 
                         e_boix_regime, 
                         e_democracy_breakdowns, 
                         e_democracy_trans, 
                         e_coups, 
                         e_legparty, 
                         v2x_ex_party, 
                         v2lgqstexp, 
                         v2cacamps_osp)
vdem_lite<-drop_na(vdem_lite, v2regimpgroup)
ERT$country_year<-paste0(ERT$country_name, "_", ERT$year)
vdem_lite$country_year<-paste0(vdem_lite$country_name, "_", vdem_lite$year)
vdem_ERT<-left_join(vdem_lite, ERT, by = "country_year")
vdem_ERT<-drop_na(vdem_ERT, reg_id)
vdem_ERT_summary<-vdem_ERT%>%group_by(reg_id)%>%summarize_all(mean)
vdem_ERT_summary$regimpgroup<-as.integer(vdem_ERT_summary$v2regimpgroup)
vdem_ERT_summary$elite_bias<-
  ifelse(
    vdem_ERT_summary$regimpgroup <= 5, "Elite", 
    ifelse((vdem_ERT_summary$regimpgroup >= 6)  & 
             (vdem_ERT_summary$regimpgroup < 13), 
           "Popular", "Foreign"))

#This dataset contains only those regimes that have experienced changes, 
#included in the ERT dataset by vdem. I removed NA for v2regimpgroup as this wil
#allow for summarize() to return no NA




weak_executive_vs_legislature<-ifelse(round(vdem_ERT_summary$v2xlg_legcon, digits = 0) == 0, 1, 0)
strong_executive_vs_legislature<-ifelse(round(vdem_ERT_summary$v2xlg_legcon, digits =0) ==1, 1, 0)
weak_executive_vs_judiciary<-ifelse(round(vdem_ERT_summary$v2x_jucon, digits = 0) == 0, 1, 0)
strong_executive_vs_judiciary<-ifelse(round(vdem_ERT_summary$v2x_jucon, digits = 0) ==1, 1, 0)
elite_biased<-ifelse(vdem_ERT_summary$elite_bias == "Elite", 1,0)
popular<-ifelse(vdem_ERT_summary$elite_bias == "Popular", 1,0)
backsliding<-as.integer(vdem_ERT_summary$aut_ep)
reg_id<-vdem_ERT_summary$reg_id
democracy<-ifelse(as.integer(vdem_ERT_summary$v2x_regime.x) >1,1,0)
autocracy<-ifelse(as.integer(vdem_ERT_summary$v2x_regime.x) <= 1,1, 0)
polarized<-ifelse(vdem_ERT_summary$v2cacamps_osp <= 2.5, 0, 1)

vdem_sets1<-data.frame(reg_id, 
                      democracy, 
                      autocracy, 
                      backsliding, 
                      popular, 
                      elite_biased, 
                      weak_executive_vs_judiciary, 
                      strong_executive_vs_judiciary, 
                      weak_executive_vs_legislature,
                      strong_executive_vs_legislature,
                      polarized)

vdem_sets2<-vdem_sets1%>%filter(democracy == 1)%>%filter(backsliding == 1)%>%subset(select = -c(democracy, backsliding))
upset(vdem_sets2, nsets = 7, 
      sets = c("elite_biased", 
               "strong_executive_vs_judiciary",
               "strong_executive_vs_legislature", 
               "popular"), 
      keep.order = T, order.by = "degree")

venn_vdem <- vdem_sets %>%
  gather(set, binary,2:8) %>% # take all binary mappings and convert to be a the set indicator
  filter(binary == 1) %>% # only include set matches
  select(reg_id, set) %>% # only include ID and set category
  mutate(set = factor(set)) # set the set column as a factor
. 
#used Laura Ellis' script: https://www.r-bloggers.com/2019/04/set-analysis-a-face-off-between-venn-diagrams-and-upset-plots/amp/

vdem_lite%>%subset(subset = country_name == "Guatemala" | country_name == "El Salvador")%>%subset(subset = year > 1980)%>%ggplot(mapping = aes(year, v2x_ex_party))+geom_line(mapping = aes(color = country_name))
# Graph shows extent of differences between El Salvador and Guatemala on reliance of president on their own party. 
# Do moe of these. 

vdem_lite%>%subset(subset = country_name == "Guatemala" | country_name == "El Salvador")%>%subset(subset = year > 1980)%>%ggplot(mapping = aes(year, v2ex_hosw))+geom_line(mapping = aes(color = country_name))


vdem_ERT_summary$ex_party_categorical<-cut(vdem_ERT_summary$v2x_ex_party, breaks = 4, labels = c("Weak", "Medium","Medium 2", "Strong"))

vdem_ERT_summary%>%subset(elite_bias != "Foreign")%>%ggplot(mapping = aes(ex_party_categorical, aut_ep))+geom_col()+facet_grid(~elite_bias)

ERT_summary<-ERT%>%group_by(reg_id)%>%summarize_if(is.numeric, mean)
view(ERT_summary)

---------------------------
  
install.packages("rJava")
install.packages("UpSetR")
install.packages("tidyverse")
install.packages("venneuler")
install.packages("grid")
  
library(rJava)
library(UpSetR)
library(tidyverse)
library(venneuler)
library(grid)


