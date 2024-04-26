library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(DescTools)
library(nnet)
library(gtsummary)
library(stargazer)
library(ggrepel)
library(scales)
library(crosstable)
library(flextable)
library(psych)
library(corrgram)
library(corrplot)
library(RColorBrewer)


load(file = "Imported_data_v2.RData")

innovativeness_logistics_00<-glm(`Dimension: Innovativeness` ~
                                1,
                              data=Flat_data_filtered_2,
                              family = "binomial")

innovativeness_logistics_01<-glm(`Dimension: Innovativeness` ~
                                   `Dimension: Objectives`+
                                   `Dimension: Actors`+
                                   `Dimension: Outputs`,
                                 data=Flat_data_filtered_2,
                                 family = "binomial")

innovativeness_logistics_02<-glm(`Dimension: Innovativeness` ~
                                   `Dimension: Objectives`+
                                   `Dimension: Actors`+
                                   `Dimension: Outputs`+
                                   `Includes a project location in Region: North America`+
                                   `Includes a project location in Region: EU`+
                                   `Includes a project location in Region: Non-EU Europe`+
                                   `Includes a project location in Region: RoW`+
                                   `Includes a project location in Region: Asia`+
                                   `Project location is in more than one country`,
                                 data=Flat_data_filtered_2,
                                 family = "binomial")

innovativeness_logistics_03<-glm(`Dimension: Innovativeness` ~
                                   `Dimension: Objectives`+
                                   `Dimension: Actors`+
                                   `Dimension: Outputs`+
                                   `Includes a project location in Region: North America`+
                                   `Includes a project location in Region: EU`+
                                   `Includes a project location in Region: Non-EU Europe`+
                                   `Includes a project location in Region: RoW`+
                                   `Includes a project location in Region: Asia`+
                                   `Project location is in more than one country`+
                                   `SDG: Industry, innovation and infrastructure`+
                                   `SDG: Quality education`+
                                   `SDG: Decent work and economic growth`+
                                   `SDG: Good health and well being`+
                                   `SDG: Reduced inequalities`+
                                   `SDG: No poverty`+
                                   `SDG: Sustainable cities and communities`+
                                   `SDG: Peace justice and strong institutions`+
                                   `SDG: Responsible consumption and production`+
                                   `SDG: Climate action`+
                                   `SDG: Zero hunger`+
                                   `SDG: Life on land`+
                                   `SDG: Affordable and clean energy`+
                                   `SDG: Life below water`+
                                   `SDG: Clean water and sanitation`+
                                   `SDG: On a topic not related to SDGs`,
                                 data=Flat_data_filtered_2,
                                 family = "binomial")

innovativeness_logistics_04a<-glm(`Dimension: Innovativeness` ~
                                   `Dimension: Objectives`+
                                   `Dimension: Actors`+
                                   `Dimension: Outputs`+
                                   `Includes a project location in Region: North America`+
                                   `Includes a project location in Region: EU`+
                                   `Includes a project location in Region: Non-EU Europe`+
                                   `Includes a project location in Region: RoW`+
                                   `Includes a project location in Region: Asia`+
                                   `Project location is in more than one country`+
                                   `SDG: Industry, innovation and infrastructure`+
                                   `SDG: Quality education`+
                                   `SDG: Decent work and economic growth`+
                                   `SDG: Good health and well being`+
                                   `SDG: Reduced inequalities`+
                                   `SDG: No poverty`+
                                   `SDG: Sustainable cities and communities`+
                                   `SDG: Peace justice and strong institutions`+
                                   `SDG: Responsible consumption and production`+
                                   `SDG: Climate action`+
                                   `SDG: Zero hunger`+
                                   `SDG: Life on land`+
                                   `SDG: Affordable and clean energy`+
                                   `SDG: Life below water`+
                                   `SDG: Clean water and sanitation`+
                                   `SDG: On a topic not related to SDGs`+
                                    `Total number of related SDGs (Grouped)`+
                                   `Project location includes a country with SPI Level: Low`+
                                   `Project location includes a country with SPI Level: Lower-Middle`+
                                    `Project location includes a country with SPI Level: Upper-Middle`+
                                    `Project location includes a country with SPI Level: High`,
                                 data=Flat_data_filtered_2,
                                 family = "binomial")

innovativeness_logistics_04b<-glm(`Dimension: Innovativeness` ~
                                    `Dimension: Objectives`+
                                    `Dimension: Actors`+
                                    `Dimension: Outputs`+
                                    `Includes a project location in Region: North America`+
                                    `Includes a project location in Region: EU`+
                                    `Includes a project location in Region: Non-EU Europe`+
                                    `Includes a project location in Region: RoW`+
                                    `Includes a project location in Region: Asia`+
                                    `Project location is in more than one country`+
                                    `SDG: Industry, innovation and infrastructure`+
                                    `SDG: Quality education`+
                                    `SDG: Decent work and economic growth`+
                                    `SDG: Good health and well being`+
                                    `SDG: Reduced inequalities`+
                                    `SDG: No poverty`+
                                    `SDG: Sustainable cities and communities`+
                                    `SDG: Peace justice and strong institutions`+
                                    `SDG: Responsible consumption and production`+
                                    `SDG: Climate action`+
                                    `SDG: Zero hunger`+
                                    `SDG: Life on land`+
                                    `SDG: Affordable and clean energy`+
                                    `SDG: Life below water`+
                                    `SDG: Clean water and sanitation`+
                                    `SDG: On a topic not related to SDGs`+
                                    `Total number of related SDGs (Grouped)`+
                                   `Project location includes a country with Income Level: Low`+
                                    `Project location includes a country with Income Level: Lower-Middle`+
                                    `Project location includes a country with Income Level: Upper-Middle`+
                                     `Project location includes a country with Income Level: High`,
                                  data=Flat_data_filtered_2,
                                  family = "binomial")

innovativeness_logistics_05b<-glm(`Dimension: Innovativeness` ~
                                    `Dimension: Objectives`+
                                    `Dimension: Actors`+
                                    `Dimension: Outputs`+
                                    `Includes a project location in Region: North America`+
                                    `Includes a project location in Region: EU`+
                                    `Includes a project location in Region: Non-EU Europe`+
                                    `Includes a project location in Region: RoW`+
                                    `Includes a project location in Region: Asia`+
                                    `Project location is in more than one country`+
                                    `SDG: Industry, innovation and infrastructure`+
                                    `SDG: Quality education`+
                                    `SDG: Decent work and economic growth`+
                                    `SDG: Good health and well being`+
                                    `SDG: Reduced inequalities`+
                                    `SDG: No poverty`+
                                    `SDG: Sustainable cities and communities`+
                                    `SDG: Peace justice and strong institutions`+
                                    `SDG: Responsible consumption and production`+
                                    `SDG: Climate action`+
                                    `SDG: Zero hunger`+
                                    `SDG: Life on land`+
                                    `SDG: Affordable and clean energy`+
                                    `SDG: Life below water`+
                                    `SDG: Clean water and sanitation`+
                                    #`SDG: On a topic not related to SDGs`+
                                    `Covers Multiple SDGs`+
                                    `Project location includes a country with SPI Level: Low`+
                                    `Project location includes a country with SPI Level: Lower-Middle`+
                                    `Project location includes a country with SPI Level: Upper-Middle`+
                                    `Project location includes a country with SPI Level: High`,
                                  data=Flat_data_filtered_2,
                                  family = "binomial")







innovativeness_logistics_05a<-glm(`Dimension: Innovativeness` ~
                                    `Dimension: Objectives`+
                                    `Dimension: Actors`+
                                    `Dimension: Outputs`+
                                    `Includes a project location in Region: North America`+
                                    `Includes a project location in Region: EU`+
                                    `Includes a project location in Region: Non-EU Europe`+
                                    `Includes a project location in Region: RoW`+
                                    `Includes a project location in Region: Asia`+
                                    `Project location is in more than one country`+
                                    `SDG: Industry, innovation and infrastructure`+
                                    `SDG: Quality education`+
                                    `SDG: Decent work and economic growth`+
                                    `SDG: Good health and well being`+
                                    `SDG: Reduced inequalities`+
                                    `SDG: No poverty`+
                                    `SDG: Sustainable cities and communities`+
                                    `SDG: Peace justice and strong institutions`+
                                    `SDG: Responsible consumption and production`+
                                    `SDG: Climate action`+
                                    `SDG: Zero hunger`+
                                    `SDG: Life on land`+
                                    `SDG: Affordable and clean energy`+
                                    `SDG: Life below water`+
                                    `SDG: Clean water and sanitation`+
                                    #`SDG: On a topic not related to SDGs`+
                                    `Covers Multiple SDGs`+
                                    `Project location includes a country with Income Level: Low`+
                                    `Project location includes a country with Income Level: Lower-Middle`+
                                    `Project location includes a country with Income Level: Upper-Middle`+
                                    `Project location includes a country with Income Level: High`,
                                  data=Flat_data_filtered_2,
                                  family = "binomial")




AIC(innovativeness_logistics_00,
    innovativeness_logistics_01,
    innovativeness_logistics_02,
    innovativeness_logistics_03,
    innovativeness_logistics_04a,
    innovativeness_logistics_04b,
    innovativeness_logistics_05a,
    innovativeness_logistics_05b,
    innovativeness_logistics_06b
)


PseudoR2(innovativeness_logistics_05a, which = "all")
PseudoR2(innovativeness_logistics_05b, which = "all")
PseudoR2(innovativeness_logistics_06b, which = "all")














innovativeness_logistics_2_02b<-glm(`Dimension: Innovativeness` ~
                                      `Dimension: Objectives`+
                                      `Dimension: Actors`+
                                      `Dimension: Outputs`+
                                      `Includes a project location in Region: North America`+
                                      `Includes a project location in Region: EU`+
                                      `Includes a project location in Region: Non-EU Europe`+
                                      `Includes a project location in Region: RoW`+
                                      `Includes a project location in Region: Asia`+
                                      `Project location is in more than one country`+
                                      `SDG: Industry, innovation and infrastructure`+
                                      `SDG: Quality education`+
                                      `SDG: Decent work and economic growth`+
                                      `SDG: Good health and well being`+
                                      `SDG: Reduced inequalities`+
                                      `SDG: No poverty`+
                                      `SDG: Sustainable cities and communities`+
                                      `SDG: Peace justice and strong institutions`+
                                      `SDG: Responsible consumption and production`+
                                      `SDG: Climate action`+
                                      `SDG: Zero hunger`+
                                      `SDG: Life on land`+
                                      `SDG: Affordable and clean energy`+
                                      `SDG: Life below water`+
                                      `SDG: Clean water and sanitation`+
                                      #`Total number of related SDGs`+
                                      #`SDG: On a topic not related to SDGs`+
                                      `Covers Multiple SDGs`+
                                      `Project location includes a country with SPI Level: Low`+
                                      `Project location includes a country with SPI Level: Lower-Middle`+
                                      `Project location includes a country with SPI Level: Upper-Middle`+
                                      `Project location includes a country with SPI Level: High`,
                                    data=Flat_data_2%>%mutate(`Dimension: Innovativeness`=as.factor(case_when(`Dimension: Innovativeness`==0~"0",
                                                                                                              TRUE~"1 or 2"))),
                                    family = "binomial")





PseudoR2(innovativeness_logistics_2_02b, which = "all")







innovativeness_consolidated_b <- 
    innovativeness_logistics_05b%>%broom::tidy(conf.int=TRUE)%>%
      mutate(across(
        starts_with("term"),
        ~ map_chr(.x, ~ gsub("\`1", " = 1", .x))
      ))%>%
      mutate(across(
        starts_with("term"),
        ~ map_chr(.x, ~ gsub("\`2", " = 2", .x))
      ))%>%
      mutate(across(
        starts_with("term"),
        ~ map_chr(.x, ~ gsub("\`Yes|\`", "", .x))
      ))%>%
      filter(term!="(Intercept)")%>%
      mutate(RRR=exp(estimate),
             Significance=case_when(p.value<=0.01~"99%",
                                    p.value>0.01&p.value<=0.05~"95%",
                                    p.value>0.05&p.value<=0.1~"90%",
                                    TRUE~"Not Significant"),
             Association=case_when(RRR==1~"Zero",
                                   RRR<1~"Negative",
                                   RRR>1~"Positive",
                                   TRUE~"NA"),
             conf.low.RRR=exp(conf.low),
             conf.high.RRR=exp(conf.high),
             Model="b. 1 vs 2")%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))%>%
  mutate(`90% CI`=paste(conf.low, conf.high, sep=", "))%>%
  mutate(term =ordered(term, levels = unique(term)))%>%
  bind_rows(
    innovativeness_logistics_2_02b%>%broom::tidy(conf.int=TRUE)%>%
      mutate(across(
        starts_with("term"),
        ~ map_chr(.x, ~ gsub("\`1", " = 1", .x))
      ))%>%
      mutate(across(
        starts_with("term"),
        ~ map_chr(.x, ~ gsub("\`2", " = 2", .x))
      ))%>%
      mutate(across(
        starts_with("term"),
        ~ map_chr(.x, ~ gsub("\`Yes|\`", "", .x))
      ))%>%
      filter(term!="(Intercept)")%>%
      mutate(RRR=exp(estimate),
             Significance=case_when(p.value<=0.01~"99%",
                                    p.value>0.01&p.value<=0.05~"95%",
                                    p.value>0.05&p.value<=0.1~"90%",
                                    TRUE~"Not Significant"),
             Association=case_when(RRR==1~"Zero",
                                   RRR<1~"Negative",
                                   RRR>1~"Positive",
                                   TRUE~"NA"),
             conf.low.RRR=exp(conf.low),
             conf.high.RRR=exp(conf.high),
             Model="a. 0 vs 1 or 2")
  )%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))%>%
  mutate(`90% CI`=paste(conf.low, conf.high, sep=", "))%>%
  mutate(term =ordered(term, levels = unique(term)))%>%
  mutate(term=str_replace(term, "Dimension", "Orientation"))%>%
  select(-`90% CI`)


write_csv(innovativeness_consolidated_b%>%filter(Model %in% c("a. 0 vs 1 or 2", "b. 1 vs 2")), "innovativeness_model_table.csv")




####graphs####

level_order <-c(    "Orientation: Objectives = 1",
                    "Orientation: Objectives = 2",
                  "Orientation: Actors = 1",
                  "Orientation: Actors = 2",
                  "Orientation: Outputs = 1",
                  "Orientation: Outputs = 2",
                  "Includes a project location in Region: North America",
                  "Includes a project location in Region: EU",
                  "Includes a project location in Region: Non-EU Europe",
                  "Includes a project location in Region: RoW",
                  "Includes a project location in Region: Asia",
                  "Project location is in more than one country",
                  "SDG: Industry, innovation and infrastructure",
                  "SDG: Quality education",
                  "SDG: Decent work and economic growth",
                  "SDG: Good health and well being",
                  "SDG: Reduced inequalities",
                  "SDG: No poverty",
                  "SDG: Sustainable cities and communities",
                  "SDG: Peace justice and strong institutions",
                  "SDG: Responsible consumption and production",
                  "SDG: Climate action",
                  "SDG: Zero hunger",
                  "SDG: Life on land",
                  "SDG: Affordable and clean energy",
                  "SDG: Life below water",
                  "SDG: Clean water and sanitation",
                  "Covers Multiple SDGs",
                  "Project location includes a country with SPI Level: Low",
                  "Project location includes a country with SPI Level: Lower-Middle",
                  "Project location includes a country with SPI Level: Upper-Middle",
                  "Project location includes a country with SPI Level: High")




Figure_2 <- innovativeness_consolidated_b%>%
  filter(Model %in% c("a. 0 vs 1 or 2", "b. 1 vs 2"))%>%
  ggplot(aes(x=RRR,y=factor(term, level=level_order), colour=factor(Significance, levels=c("99%", "95%", "90%", "Not Significant"))))+
  geom_point(size=3)+
  #geom_point(aes(shape=Association), size=3)+
  #scale_x_continuous(trans = 'log10')+
  #scale_shape_manual(values=c(4, 3))+
  #xlim(-15,25)+
  xlab("Odds Ratio")+
  ylab("Independent Variable")+
  #geom_line(aes(group=term)) + 
  geom_linerange(aes(xmin = conf.low.RRR, xmax=  conf.high.RRR)) +
  geom_vline(xintercept=1, linetype="dotted", linewidth=0.7, alpha=0.7, color="green")+
  scale_y_discrete(limits=rev,
                   #labels = function(x) str_wrap(x, width = 30)
  )+
  facet_wrap(~Model, 
             #ncol=6, 
             scales="free_x", 
             #labeller = label_wrap_gen(width=13)
  )+
  theme(legend.position = "bottom",
        legend.box="vertical",
        legend.box.just = "left",
        #legend.margin=margin(),
        legend.box.spacing=unit(0.1, "cm"),
        legend.spacing = unit(0.1, "cm"),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 7, face="bold"),
        axis.title.y = element_text(size = 7, face="bold"),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7, face="bold"),
        strip.text=element_text(size = 9, face="bold", colour = "white"),
        strip.background =element_rect(fill="black"),
        legend.box.margin=margin(20,20,20,20),
        legend.margin=margin(0,0,0,0)
        )+
  geom_text_repel(aes(label=round(RRR, 2)), 
                  size=3,
                  #box.padding = unit(0.5, "mm")
                  #min.segment.length = 0,
                  #seed = 42, 
                  #box.padding = 0.5,
                  #nudge_x = .5,
                  # nudge_y = -.25,
                  #arrow = arrow(length = unit(0.5, 'cm'), type = 'open')
  )+
  #geom_text(aes(label=round(RRR, 2)), size=3, check_overlap = TRUE, vjust=-1, hjust="outward")+
  scale_color_manual(values=c( "99%"="red", "95%"="magenta", "90%"="blue", "Not Significant"="grey"))+
  labs(colour="Significance")

ggsave(Figure_2, filename= "Figure_2.png", width=157.5, height = 210, unit="mm", dpi=900)





Figure_3<-Flat_data_2%>%select(starts_with("Dimension"))%>%
  pivot_longer(cols=c(`Dimension: Objectives`, `Dimension: Actors`, `Dimension: Outputs`))%>%
  group_by(`Dimension: Innovativeness`, name, value)%>%
  tally()%>%
  ungroup()%>%
  group_by(`Dimension: Innovativeness`, name)%>%
  reframe(n, value, pct=n/sum(n))%>%
  mutate(`Dimension: Innovativeness`=as.factor(case_when(`Dimension: Innovativeness`==0~"Innovativeness: Score 0", 
                                               `Dimension: Innovativeness`==1~"Innovativeness: Score 1", 
                                               `Dimension: Innovativeness`==2~"Innovativeness: Score 2")))%>%
  mutate(across(
  starts_with("name"),
  ~ map_chr(.x, ~ gsub("Dimension: ", "", .x))
))%>%
  ggplot(aes(x=factor(name, levels=c("Objectives", "Actors", "Outputs")), fill=factor(value, levels=c("2", "1", "0")), y=n))+
  geom_bar(position="fill",  stat="identity")+
  geom_text(aes(label = percent(round(pct, 3))),
            position = position_fill(0.5),
            size=2.5)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~`Dimension: Innovativeness`)+
  labs(x="Orientation",
       y="Percentage of All Projects",
       fill="Score")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 15)),
        strip.text=element_text(face="bold", colour = 'white'),
        strip.background =element_rect(fill="black")
        )


ggsave(Figure_3, filename= "Figure_3.png", width=157.5, height = 120, unit="mm", dpi=900)


####descriptives####
####Descriptives Table####

crosstable_options(crosstable_fishertest_B = 1e+05)
crosstable(
  Flat_data_2,
  cols = c(
    -everything(),
    -Project_id,
      "Dimension: Objectives",
      "Dimension: Actors",
      "Dimension: Outputs",
      "Includes a project location in Region: North America",
      "Includes a project location in Region: EU",
      "Includes a project location in Region: Non-EU Europe",
      "Includes a project location in Region: RoW",
      "Includes a project location in Region: Asia",
      "Project location is in more than one country",
      "SDG: Industry, innovation and infrastructure",
      "SDG: Quality education",
      "SDG: Decent work and economic growth",
      "SDG: Good health and well being",
      "SDG: Reduced inequalities",
      "SDG: No poverty",
      "SDG: Sustainable cities and communities",
      "SDG: Peace justice and strong institutions",
      "SDG: Responsible consumption and production",
      "SDG: Climate action",
      "SDG: Zero hunger",
      "SDG: Life on land",
      "SDG: Affordable and clean energy",
      "SDG: Life below water",
      "SDG: Clean water and sanitation",
      #"SDG: On a topic not related to SDGs",
      "Covers Multiple SDGs",
      "Project location includes a country with SPI Level: Low",
      "Project location includes a country with SPI Level: Lower-Middle",
      "Project location includes a country with SPI Level: Upper-Middle",
      "Project location includes a country with SPI Level: High",
    "Project location includes a country with Income Level: Low",
      "Project location includes a country with Income Level: Lower-Middle",
      "Project location includes a country with Income Level: Upper-Middle",
      "Project location includes a country with Income Level: High"
   ),
  by = "Dimension: Innovativeness",
  total = "both",
  percent_pattern = "{n} ({p_row})",
  showNA = "ifany",
  test = T
) %>%
  flextable::as_flextable() %>%
  flextable::autofit(add_w = 0, add_h = 0)%>%
  font(fontname = "Arial")%>%
  fontsize(size=9)%>%
  line_spacing(space = 1, part = "all")%>%
  set_table_properties(layout = "autofit")%>%
  flextable::save_as_docx(path = "descriptives_crosstable_innovativeness_prow.docx")



####correlogram####
M <-cor(Flat_data_2%>%select(`Dimension: Innovativeness`,
                             `Dimension: Objectives`,
                             `Dimension: Actors`,
                             `Dimension: Outputs`,
                             `Includes a project location in Region: North America`,
                             `Includes a project location in Region: EU`,
                             `Includes a project location in Region: Non-EU Europe`,
                             `Includes a project location in Region: RoW`,
                             `Includes a project location in Region: Asia`,
                             `Project location is in more than one country`,
                             `SDG: Industry, innovation and infrastructure`,
                             `SDG: Quality education`,
                             `SDG: Decent work and economic growth`,
                             `SDG: Good health and well being`,
                             `SDG: Reduced inequalities`,
                             `SDG: No poverty`,
                             `SDG: Sustainable cities and communities`,
                             `SDG: Peace justice and strong institutions`,
                             `SDG: Responsible consumption and production`,
                             `SDG: Climate action`,
                             `SDG: Zero hunger`,
                             `SDG: Life on land`,
                             `SDG: Affordable and clean energy`,
                             `SDG: Life below water`,
                             `SDG: Clean water and sanitation`,
                             #`SDG: On a topic not related to SDGs`,
                             `Covers Multiple SDGs`,
                             `Project location includes a country with SPI Level: Low`,
                             `Project location includes a country with SPI Level: Lower-Middle`,
                             `Project location includes a country with SPI Level: Upper-Middle`,
                             `Project location includes a country with SPI Level: High`)%>%
          mutate(across(everything(), ~as.numeric(.x))))
correlation_innovativeness <-
  corrplot::corrplot(M, 
         type="lower", 
         method="number",
         #order="hclust",
         #addCoefasPercent=T,
         diag=F,
         number.cex=0.75,
         tl.cex=0.8,
         insig = "pch",
         sig.level=0.05,
         #tl.pos="ln",
         #win.asp=1.57,
         col=brewer.pal(n=8, name="RdYlBu"))



