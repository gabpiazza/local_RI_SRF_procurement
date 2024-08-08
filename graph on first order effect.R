name<- c("E.Zanon Physics branch", "Other manufacturing firms")
employees<- c(30, 563)
type_jobs<- c("new employees", "new employees")
df<- data.frame(name, employees, type_jobs)

ggplot(df, aes(x="", y=employees,  fill=name))+geom_col(width=0.4)+labs(x="2018")+
  geom_text(aes(label=employees),position=position_stack(vjust=0.5), size=6)+
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right", 
        legend.title = element_blank())+
  theme(axis.title.y = element_text(margin = margin(r = 20)))+
  ylab("Additional Jobs")+
  coord_flip()

