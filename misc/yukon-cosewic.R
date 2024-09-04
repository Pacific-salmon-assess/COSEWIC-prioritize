# yukon plot for cosewic memo/meetings
# run code chunks in Rmd up to plots then this script

i <- "Yukon"
j <- "Chinook"

sp_data_region <- filter(sp_data, Region==i)


sub_data <- filter(sp_data_region, Species==j) |>
  complete(Year, Species, Region, du_cu) #explicitly include NA so line draws right

p <- ggplot(data=sub_data, aes(x=Year, y=Spawner.Abundance/1000)) +
  geom_line(color="grey") +
  geom_point() +
  stat_smooth(formula=y~x, method="glm", color="black", lty="dashed", se=TRUE, 
              method.args = list(family = gaussian(link = 'log')), alpha=0.3) +
  stat_smooth(data=sub_last3, formula=y~x, method="glm", color="red", 
              method.args = list(family = gaussian(link = 'log')),
              se=TRUE, na.rm=TRUE, fill = "red", alpha=0.1) +
  facet_wrap(~du_cu, scales = "free_y") +
  labs(x="Year", y="Mature individuals (thousands)") +
  theme_sleek() +
  theme(strip.background=element_blank(), strip.text = element_text(size = 10))
print(p)

ggsave("output/figures/yukon-ck-cosewic.jpeg", width = 8, height = 5, units = "in")
