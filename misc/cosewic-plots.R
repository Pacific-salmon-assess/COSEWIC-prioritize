#  plot for cosewic memo/meetings
# run code chunks in Rmd up to plots then this script

raw_sp_data <- raw_sp_data |>
  mutate(logSp=log(Spawner.Abundance))


sp_data <- left_join(raw_sp_data, select(cu_metadata, CUID, du_number, DU_name_short,gen_length), 
                     by="CUID") |> 
  drop_na(du_number, Spawner.Abundance) |>
  mutate(du_cu=paste(du_number, DU_name_short)) |> ##ERROR HERE - NOT TRULY DISTINCT! See "CK21 ECVI-Ocean-Fall"
  relocate(du_cu, 1) |>
  arrange(du_cu, Year) |>
  mutate(Region = case_when(Region == "HG" ~ "Haida Gwaii",
                            Region == "CC" ~ "Central Coast",
                            Region == "VIMI" ~ "Van. Isl. Main. Inl.", 
                            TRUE ~ Region))

sp_data2 <- sp_data |>
  group_by(du_number, Year) |>
  summarize(mat_ind = sum(Spawner.Abundance))|> ##DOUBLE CHEK
  arrange(du_number, Year)

sp_data <- left_join(sp_data2, select(sp_data, du_number, DU_name_short, Species, Region, gen_length,du_cu), 
                     by="du_number",
                     multiple = "first") |>
  mutate(Spawner.Abundance = mat_ind)

regions_ordered <- c("Yukon", "Nass", "Skeena", "Haida Gwaii", "Central Coast", 
                     "Van. Isl. Main. Inl.", "Fraser", "Columbia")

last3_gens <- sp_data |>
  group_by(du_cu) |>
  mutate(three_gens = gen_length*3) |>
  filter(Year > (last(Year) - three_gens), Year <= last(Year))


# Yukon ----
i <- "Yukon"
j <- "Chinook"

sp_data_region <- filter(sp_data, Region==i)
last3_gens_region <- filter(last3_gens, Region==i)

sub_data <- filter(sp_data_region, Species==j) |>
  complete(Year, Species, Region, du_cu) #explicitly include NA so line draws right

sub_last3 <- filter(last3_gens, Region==i, Species==j)

p <- ggplot(data=sub_data, aes(x=Year, y=mat_ind/1000)) +
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

# Skeena/Nass Chinook ---

i <- c("Skeena", "Nass")
j <- "Chinook"

sp_data_region <- filter(sp_data, Region %in% i)
last3_gens_region <- filter(last3_gens, Region %in% i)

sub_data <- filter(sp_data_region, Species==j) |>
  complete(Year, Species, Region, du_cu) #explicitly include NA so line draws right

sub_last3 <- filter(last3_gens, Region %in% i, Species==j)

p <- ggplot(data=sub_data, aes(x=Year, y=mat_ind/1000)) +
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
  theme(strip.background=element_blank(), strip.text = element_text(size = 8))
print(p)

ggsave("output/figures/skeena&nass-ck-cosewic.jpeg", width = 10, height = 5, units = "in")


# Skeena and Nass sockeye ----

i <- c("Skeena", "Nass")
j <- "Sockeye"

sp_data_region <- filter(sp_data, Region %in% i)
last3_gens_region <- filter(last3_gens, Region %in% i)

sub_data <- filter(sp_data_region, Species==j,du_number %in% c("SE043", "SE160", "SE163", "SE149", "SE164", "SE041", "SE153", "SE158", "SE169", "SE173")) |>
  complete(Year, Species, Region, du_cu) #explicitly include NA so line draws right

sub_last3 <- filter(last3_gens, Region %in% i, Species==j, du_number %in% c("SE043", "SE160", "SE163", "SE149", "SE164", "SE041", "SE153", "SE158", "SE169", "SE173"))

p <- ggplot(data=sub_data, aes(x=Year, y=mat_ind/1000)) +
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
  theme(strip.background=element_blank(), strip.text = element_text(size = 8))
print(p)

ggsave("output/figures/skeena&nass-sk-cosewic.jpeg", width = 10, height = 5, units = "in")

