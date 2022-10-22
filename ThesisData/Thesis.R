vdem_transition$log_libdem = log(vdem_transition$vdem_full.v2x_libdem, base = exp(5))

vdem_transition$log_polyarchy = log(vdem_transition$vdem_full.v2x_polyarchy, base = exp(5))

vdem_ERT<-full_join(ERT, vdem, by = c("country_name", "year"))

ggplot(vdem_transition, mapping = aes(vdem_full.year))+geom_smooth(aes(y = vdem_full.v2x_libdem), color = "deepskyblue3") + geom_smooth(aes(y=vdem_full.v2x_polyarchy), color = "seagreen") + geom_smooth( aes(y  = vdem_full.v2x_rule), color = "darkslategray")+facet_wrap(facets = vars(elite_bias_am2018)) + transition_reveal(rev(seq_along(vdem_full.v2x_year))) + shadow_mark()





vdem_ERT_transition_test<-drop_na(vdem_ERT_transition, elite_bias_am2018)
vdem_ERT_transition_test<-subset(vdem_ERT_transition_test, year>=transition_year)

vdem_ERT_transition_test<-subset(
  vdem_ERT_transition_test, lag(year)-lag(transition_year)<(year-transition_year))
view(vdem_ERT_transition_test)


vdem_ERT_transition_test%>%filter(aut_ep == "1")%>%ggplot(aes(vdem_full.v2x_libdem, v2x_polyarchy)) + geom_point()+geom_smooth()+facet_wrap(facets=vars(elite_bias_am2018))

*Last action: Vreated vdem_ERT_transition. Need to add transition year to all NA under transition_year

----------------------------------------------------------------------

vdem$country_year<-paste0(vdem$country_name, "_", vdem$year)
ERT$country_year<-paste0(ERT$country_name, "_", ERT$year)
vdem_ERT<-left_join(vdem, ERT, by = "country_year")
vdem_ERT_transition<-left_join(vdem_ERT, transition_mode, by = "country_year")