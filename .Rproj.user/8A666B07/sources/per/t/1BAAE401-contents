# Eelgrass: ALL data from Bight '23 and RESCQ
# QAQC 
# calculation of structural metrics


# open libs ----
library(here)
library(tidyr)
library(dplyr)
library(sf)
library(spsurvey)
library(vegan)
library(ggplot2)
library(stringr)
library(kableExtra)
library(DT)
library(ggforce)
library(plotly)
library(purrr)

# read in data ---- 
eelgrass.field <- read.csv(here("data", "raw", "eelgrassfield_FINAL.csv"))
eelgrass.lab <- read.csv(here("data", "raw", "eelgrasslab_FINAL.csv"))
sample.draw <- read.csv(here("data", "raw", "10-23 eelgrass sample draw.csv")) #not necessary
station.info <- read.csv(here("data", "raw", "station_info_CGupdate.csv"))
perim <- read.csv(here("data", "raw", "merkel_beddimensions_copy.csv")) #not necessary


# wrangle ----

field.full <- eelgrass.field %>%
  left_join(station.info, by = "site_id", relationship = "many-to-many") 
  

lab.full <- eelgrass.lab %>%
  select(-sample_date) %>%
  left_join(station.info, by = "site_id", relationship = "many-to-many") %>%
  select(-project_id.y, -Collection_agency, -Processing_agency, -shoot_identity) %>%
  rename(project_id = project_id.x)

eelgrass <- bind_rows(field.full, lab.full) %>% 
  mutate(variable = recode(variable, "Shoot Height" = "shoot height",
                           "Leaves per Shoot" = "leaves per shoot",
                           "above_ground_AFDM" = "Above Ground AFDM",
                           "below_ground_AFDM" = "Below Ground AFDM")) %>%
  mutate(result = if_else(variable == "Shoot Width" & unit == "cm", result*10, result),
         unit = if_else(variable == "Shoot Width" & unit == "cm", "mm", unit)) %>%
  mutate(variable_label = paste0(variable, " (", unit, ")")

# QAQC ----

site.list <- unique(eelgrass.all$site_id)
structural.metric.list <- unique(eelgrass$variable_label)

## look at where we have -88. Scan to see if we should remove data or turn them to zeros

ND.list <- eelgrass %>%
  filter(result == -88)

## turn them to zeros and remove any identified accidental negative values (there were 6)
## also remove whitespace
eelgrass <- eelgrass %>%
  mutate(result = if_else(result == "-88", 0, result)) %>%
  mutate(result = abs(result)) %>%
  mutate(
    sample_id = str_replace_all(sample_id, "\\s+", ""),
  sample_id = str_replace(
    sample_id,
    "^([A-Za-z]+)(\\d+)$",
    "\\1.\\2"
  )
)


#########################################################

## QAQC tables ----
#### group by stratum----

summary_stratum <- eelgrass_v2 %>%
  group_by(stratum, variable_label) %>%
  summarise(
    n = n(),
    mean = round(mean(result, na.rm = TRUE), 3),
    sd = round(sd(result, na.rm = TRUE), 3),
    min = round(min(result, na.rm = TRUE), 2),
    max = round(max(result, na.rm = TRUE), 2),
    range = max - min,
    .groups = "drop"
  )

summary_table_stratum <- datatable(summary_stratum,
          options = list(pageLength = 10),
          caption = "Eelgrass Lab Metric Summary Stats by stratum (interactive table)")

htmlwidgets::saveWidget(widget = summary_table_stratum, 
                        file = here("output", "tables", "stratum_summary.html"),
                                    selfcontained = TRUE)

### group by waterbody ----
summary_waterbody <- eelgrass %>%
  group_by(waterbody, variable_label) %>%
  summarise(
    n = n(),
    mean = round(mean(result, na.rm = TRUE), 3),
    sd = round(sd(result, na.rm = TRUE), 3),
    min = round(min(result, na.rm = TRUE), 2),
    max = round(max(result, na.rm = TRUE), 2),
    range = max - min,
    .groups = "drop"
  )

summary_table_waterbody <- datatable(summary_waterbody,
                                   options = list(pageLength = 10),
                                   caption = "Eelgrass Lab Metric Summary Stats by waterbody (interactive table)")

htmlwidgets::saveWidget(widget = summary_table_waterbody, 
                        file = here("output", "tables", "waterbody_summary.html"),
                        selfcontained = TRUE)
######################################################
## QAQC visuals ----
### visualize by stratum ----

# remove some irrelevant vars
eelgrass_minimal <- eelgrass %>%
  filter(!variable %in% c("n_blades_w_disease", "n_leaves", "below_ground_TOC_TN_dry_mass", "above_ground_TOC_TN_dry_mass",
                       "Above Ground wet mass", "Above Ground dry mass", "Below Ground wet mass",
                       "Below Ground dry mass", "Epiphyte wet mass", "Epiphyte dry mass"))

eelgrass_structural <- c("flowering shoot density", "shoot density", "leaves per shoot",
                         "shoot height", "Shoot Width", "Shoot Count", "Leaf Area", "percent cover")
eelgrass_labmeas <- c("AG_TN", "AG_TOC", "Above Ground AFDM", "Below Ground AFDM", "Epiphyte AFDM", 
                  "BG_TOC", "AB_TN")
col_strata <- c(estuary = "#330066", small_embayment = "#cc0066",
                large_embayment ="#ffcccc", "NA" = "cornsilk3")

#dataset ONLY for plotting - trim outliers
eelgrass_minimal_trim <- eelgrass_minimal %>%
  group_by(variable_label) %>%
  filter(result <= quantile(result, 0.97, na.rm =T)) %>%
  ungroup()

# PLOTS
#overall
structural.overall <- eelgrass_minimal_trim %>%
  filter(project_id == "B23-SAV") %>%
  ggplot(aes(x = "", y = result)) +
  geom_boxplot(fill = "cornflowerblue") + 
  scale_y_continuous(oob = scales::squish)+
  ggh4x::facet_wrap2(~ variable_label, scales = "free_y", ncol = 4, axes = "margins", remove_labels = "x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  theme(legend.position = "none")
  
  structural.overall
  
  ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/QAQC/structural.overall.jpg", 
        structural.overall,
         height = 10, width = 15,
         dpi = 400)
  
library(ggh4x)
plot_structural <- eelgrass_minimal_trim %>%
  filter(variable %in% eelgrass_structural) %>%
  filter(project_id == "B23-SAV") %>%
  ggplot(aes(x = stratum, y = result)) +
  #geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.6, size = 3) +
  geom_boxplot(aes(fill = stratum)) +
  scale_y_continuous(oob = scales::squish)+
  scale_fill_manual(values = col_strata) +
  ggh4x::facet_wrap2(~ variable_label, scales = "free_y", ncol = 2, axes = "margins", remove_labels = "x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  theme(legend.position = "none") 

plot_structural 

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/QAQC/structural.metric.boxplot.jpg", 
       plot_structural,
       height = 14, width = 12,
       dpi = 400)


plot_lab <- eelgrass_minimal_trim %>%
  filter(variable %in% eelgrass_labmeas) %>%
  filter(project_id == "B23-SAV") %>%
  ggplot(aes(x = stratum, y = result)) +
  geom_boxplot(aes(fill = stratum)) +
  #geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.5, size = 3) +
  scale_fill_manual(values = col_strata) +
  facet_wrap2(~ variable_label, scales = "free_y", ncol = 2, axes = "margins", remove_labels = "x") +
  scale_y_continuous(oob = scales::squish) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  theme(legend.position = "none")
  
  plot_lab
  ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/QAQC/structural.metric.boxplot2.jpg", 
         plot_lab,
         height = 12, width = 12,
         dpi = 400)

## Interesting relationships
  
  #structural metrics with distance to inlet
 
inlet_plot1 <- eelgrass_minimal %>%
  filter(variable %in% eelgrass_labmeas) %>%
  filter(project_id == "B23-SAV") %>%
  ggplot( aes(x = distance_to_inlet, y = result)) +
  geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.5, size = 3) +
  scale_color_manual(values = col_strata) +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  xlab("Distance to inlet (km)")
  theme(legend.position = "none") 

inlet_plot2 <- eelgrass_minimal %>%
  filter(variable %in% eelgrass_structural) %>%
  filter(project_id == "B23-SAV") %>%
  ggplot( aes(x = distance_to_inlet, y = result)) +
  geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.5, size = 3) +
  scale_color_manual(values = col_strata) +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  xlab("Distance to inlet (km)")
theme(legend.position = "none") 

candidate_inlet_metrics <- c("leaves per shoot", "Shoot Count", "shoot density",
                             "flowering shoot density", "Above Ground AFDM")

inlet_plot_final <- eelgrass_minimal %>%
  filter(variable %in% candidate_inlet_metrics) %>%
  filter(project_id == "B23-SAV") %>%
  filter(species != "Zostera pacifica") %>%
  ggplot( aes(x = distance_to_inlet, y = result)) +
  geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.5, size = 3) +
  scale_color_manual(values = col_strata) +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  xlab("Distance to inlet (km)") +
theme(legend.position = "none") 

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/QAQC/DTI_metric_plot.jpg", 
       inlet_plot_final,
       height = 12, width = 12,
       dpi = 400)

#run some linear mods to check for relationships


#structural metrics vs site protection
siteprotection_plot1 <- eelgrass_minimal %>%
  filter(variable %in% eelgrass_labmeas) %>%
  filter(project_id == "B23-SAV") %>%
  filter(species != "Zostera pacifica") %>%
  ggplot( aes(x = site_protection_granular, y = result)) +
  geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.5, size = 3) +
  scale_color_manual(values = col_strata) +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  xlab("Site protection status") 
theme(legend.position = "none") 

siteprotection_plot2 <- eelgrass_minimal %>%
  filter(variable %in% eelgrass_structural) %>%
  filter(project_id == "B23-SAV") %>%
  filter(species != "Zostera pacifica") %>%
  ggplot( aes(x = site_protection_granular, y = result)) +
  geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.5, size = 3) +
  scale_color_manual(values = col_strata) +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  xlab("Site protection status") 
theme(legend.position = "none") 

candidate_siteprotection_metrics <- c("Below Ground AFDM", "Epiphyte AFDM",
                                      "flowering shoot density")

siteprotection_plot_final <- eelgrass_minimal %>%
  filter(variable %in% candidate_siteprotection_metrics) %>%
  filter(project_id == "B23-SAV") %>%
  filter(species != "Zostera pacifica") %>%
  ggplot( aes(x = site_protection_granular, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.5, size = 4) +
  scale_color_manual(values = col_strata) +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18)) +
  xlab("Site protection status") 
#theme(legend.position = "none") 

siteprotection_plot_final

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/QAQC/siteprotection_metric_plot.jpg", 
       siteprotection_plot_final,
       height = 10, width = 12,
       dpi = 400)


### visualize by waterbody ----
# King Harbor
king_harbor_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "King Harbor")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - King Harbor")

ggplotly(king_harbor_plot)

htmlwidgets::saveWidget(ggplotly(king_harbor_plot), 
                        here("figures", "exploratory", "king_harbor_boxplot.html"),
                        selfcontained = TRUE)

# Alamitos Bay 
alamitos_plot <-ggplot(eelgrass %>% filter(str_detect(waterbody, "Alamitos")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Alamitos Bay")

ggplotly(alamitos_plot)

htmlwidgets::saveWidget(ggplotly(alamitos_plot), 
                        here("figures", "exploratory", "alamitos_boxplot.html"),
                        selfcontained = TRUE)


# Port of Los Angeles
portofLA_plot <-ggplot(eelgrass %>% filter(str_detect(waterbody, "Port of Los Angeles")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Port of Los Angeles")

ggplotly(portofLA_plot)

htmlwidgets::saveWidget(ggplotly(portofLA_plot), 
                        here("figures", "exploratory", "portofLA_boxplot.html"),
                        selfcontained = TRUE)

# Seal Beach NWR
sealbeach_plot <-ggplot(eelgrass %>% filter(str_detect(waterbody, "Seal Beach")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Seal Beach NWR")

ggplotly(sealbeach_plot)

htmlwidgets::saveWidget(ggplotly(sealbeach_plot), 
                        here("figures", "exploratory", "sealbeach_boxplot.html"),
                        selfcontained = TRUE)



# Bolsa Chica
bolsachica_plot <-ggplot(eelgrass %>% filter(str_detect(waterbody, "Bolsa")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Bolsa Chica")

ggplotly(bolsachica_plot)

htmlwidgets::saveWidget(ggplotly(bolsachica_plot), 
                        here("figures", "exploratory", "bolsachica_boxplot.html"),
                        selfcontained = TRUE)

# Magnolia Marsh
magnoliamarsh_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Magnolia Marsh")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Magnolia Marsh")

ggplotly(magnoliamarsh_plot)

htmlwidgets::saveWidget(ggplotly(magnoliamarsh_plot), 
                        here("figures", "exploratory", "magnoliamarsh_boxplot.html"),
                        selfcontained = TRUE)


# Santa Ana River
santaanariver_plot <-ggplot(eelgrass %>% filter(waterbody == "Santa Ana River"), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Santa Ana River")

ggplotly(santaanariver_plot)

htmlwidgets::saveWidget(ggplotly(santaanariver_plot), 
                        here("figures", "exploratory", "santaanariver_boxplot.html"),
                        selfcontained = TRUE)

# San Dieguito Lagoon
sandieguito_lagoon_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Dieguito")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - San Dieguito Lagoon")

ggplotly(sandieguito_lagoon_plot)

htmlwidgets::saveWidget(ggplotly(sandieguito_lagoon_plot), 
                        here("figures", "exploratory", "sandieguito_lagoon_boxplot.html"),
                        selfcontained = TRUE)

# Newport
newport_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Newport")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Newport Bay")

ggplotly(newport_plot)

htmlwidgets::saveWidget(ggplotly(newport_plot), 
                        here("figures", "exploratory", "newport_boxplot.html"),
                        selfcontained = TRUE)


# Dana Point Harbor
danapoint_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Dana Point")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Dana Point Harbor")

ggplotly(danapoint_plot)

htmlwidgets::saveWidget(ggplotly(danapoint_plot), 
                        here("figures", "exploratory", "danapoint_boxplot.html"),
                        selfcontained = TRUE)


# Oceanside Harbor
oceanside_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Oceanside")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Oceanside Harbor")

ggplotly(oceanside_plot)

htmlwidgets::saveWidget(ggplotly(oceanside_plot), 
                        here("figures", "exploratory", "oceanside_boxplot.html"),
                        selfcontained = TRUE)


# Batiquitos Lagoon
batiquitos_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Batiquitos")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Batiquitos Lagoon")

ggplotly(batiquitos_plot)

htmlwidgets::saveWidget(ggplotly(batiquitos_plot), 
                        here("figures", "exploratory", "batiquitos_boxplot.html"),
                        selfcontained = TRUE)


# Agua Hedionda 
aguahedionda_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Agua Hedionda")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Agua Hedionda")

ggplotly(aguahedionda_plot)

htmlwidgets::saveWidget(ggplotly(aguahedionda_plot), 
                        here("figures", "exploratory", "aguahedionda_boxplot.html"),
                        selfcontained = TRUE)


# San Elijo
sanelijo_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "San Elijo")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - San Elijo")

ggplotly(sanelijo_plot)

htmlwidgets::saveWidget(ggplotly(sanelijo_plot), 
                        here("figures", "exploratory", "sanelijo_boxplot.html"),
                        selfcontained = TRUE)


#ggplot(eelgrassfield_bightonly %>% filter(str_detect(waterbody, "San Elijo")), aes(x = site_id, y = result)) +
# geom_boxplot(outlier.shape = NA, color = "grey30") +
# geom_jitter(width = 0.2, alpha = 0.7, color = "darkseagreen4") +
#  facet_wrap(~ variable_label, scale = "free") +
#  theme_bw() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  theme(legend.position = "none") +
#  ggtitle("B23 SAV field data - San Elijo")

# La Jolla Cove
lajolla_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "La Jolla")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - La Jolla Cove")

ggplotly(lajolla_plot)

htmlwidgets::saveWidget(ggplotly(lajolla_plot), 
                        here("figures", "exploratory", "lajolla_boxplot.html"),
                        selfcontained = TRUE)


# Mission Bay
missionbay_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Mission Bay")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Mission Bay")

ggplotly(missionbay_plot)

htmlwidgets::saveWidget(ggplotly(missionbay_plot), 
                        here("figures", "exploratory", "missionbay_boxplot.html"),
                        selfcontained = TRUE)


# San Diego Bay
sandiego_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Diego")) %>% filter(str_detect(site_id, "B23-SAV")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - San Diego Bay")

ggplotly(sandiego_plot)

htmlwidgets::saveWidget(ggplotly(sandiego_plot), 
                        here("figures", "exploratory", "sandiego_boxplot.html"),
                        selfcontained = TRUE)


# San Diego Bay Targeted sites
sandiego_target_plot <- ggplot(eelgrass %>% filter(str_detect(waterbody, "Diego")) %>% filter(!str_detect(site_id, "B23-SAV")), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - San Diego Bay")

ggplotly(sandiego_target_plot)

htmlwidgets::saveWidget(ggplotly(sandiego_target_plot), 
                        here("figures", "exploratory", "sandiego_target_boxplot.html"),
                        selfcontained = TRUE)


#Santa Cruz Island
santacruz_plot <- ggplot(eelgrass %>% filter(waterbody == "Santa Cruz Island"), aes(x = site_id, y = result)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "midnightblue") +
  facet_wrap(~ variable_label, scale = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  ggtitle("2023 eelgrass data - Santa Cruz Island")

ggplotly(santacruz_plot)

htmlwidgets::saveWidget(ggplotly(santacruz_plot), 
                        here("figures", "exploratory", "santacruz_boxplot.html"),
                        selfcontained = TRUE)


# output data into intermediate folder
write.csv(eelgrass, here("data", "intermediate", "eelgrass_ALL_clean.csv"))

#################################################################
# Score ALL 2023 Structural Metrics ----

#delineate probabilistic -sampled sites

#We'll also look at the distribution of scores and compare to another metric (anything from Martha??)
## wrangle ----

shootheight_all <-eelgrass %>% filter(variable == "shoot height") %>% filter(variable > 0)
shootdensity_all <-eelgrass %>% filter(variable == "shoot density") %>% filter(variable >= 0)
percentcover_all <-eelgrass %>% filter(variable == "percent cover") %>% filter(variable >= 0)
percentmacroalgae_all <-eelgrass %>% filter(variable == "percent macroalgae") %>% filter(variable >= 0)
floweringshoot_all <-eelgrass %>% filter(variable == "flowering shoot density") %>% filter(variable >= 0)
shootcount_all <- eelgrass %>% filter(variable == "Shoot Count")
leafarea_all <- eelgrass %>% filter(variable == "Leaf Area")
epiphyteAFDM_all <-eelgrass %>% filter(variable == "Epiphyte AFDM")
shootwidth_all <- eelgrass %>% filter(variable == "Shoot Width")
AG_AFDM_all <- eelgrass %>% filter(variable == "Above Ground AFDM")
BG_AFDM_all <- eelgrass %>% filter(variable == "Below Ground AFDM")
LPS_all <- eelgrass %>% filter(variable == "Leaves per Shoot")
AG_TOC_all <- eelgrass %>% filter(variable == "AG_TOC")
AG_TN_all <- eelgrass %>% filter(variable == "AG_TN")
BG_TOC_all <- eelgrass %>% filter(variable == "BG_TOC")
BG_TN_all <- eelgrass %>% filter(variable == "BG_TN")

# Intermediate Structural Metric calculations ----
# TOC:TN ratio
TOC.TN.calc <- eelgrass %>%
  filter(variable %in% c("AG_TOC", "AG_TN", "BG_TOC", "BG_TN")) %>%
  group_by(site_id, transect, sample_id, variable) %>%
  summarise(
    result = mean(result, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable,
              values_from = result) %>%
  mutate(AG_TOC_TN = if_else(AG_TN > 0,
                                 AG_TOC/AG_TN,
                                 NA_real_),
         BG_TOC_TN = if_else(BG_TN > 0,
                                 BG_TOC/BG_TN,
                                 NA_real_)) %>%
 select(site_id, transect, sample_id, AG_TOC_TN, BG_TOC_TN) 

TOC.TN.calc

#re-pivot and join with metadata
TOC.TN.long <-TOC.TN.calc %>%
  pivot_longer(cols = c(AG_TOC_TN, BG_TOC_TN),
               names_to = "variable",
               values_to = "result")

metadata <- eelgrass %>%
  distinct(site_id, transect, sample_id, .keep_all = TRUE) %>%
  select(-variable, -result)

eelgrass.calc <- TOC.TN.long %>%
  left_join(metadata, by = c("site_id", "transect", "sample_id"))

eelgrass.calc.rename <- eelgrass.calc %>%
  mutate(variable_label = case_when(
    variable == "AG_TOC_TN" ~ "Above Ground TOC:TN",
    variable == "BG_TOC_TN" ~ "Below Ground TOC:TN",
    TRUE ~ variable_label),
    unit = NA_character_)

#New eelgrass dataset ready for scoring
eelgrass_v2 <-bind_rows(eelgrass, eelgrass.calc.rename)

# Useful subsets from new data
AG_TOC_TN_all <- eelgrass_v2 %>% filter(variable == "AG_TOC_TN")
BG_TOC_TN_all <- eelgrass_v2 %>% filter(variable == "BG_TOC_TN")

# ## Examine leaf area: AG biomass, leaf area vs avg shoot height, and AG biomass vs shoot density
# Since there are multiple Leaf Area measurements per sample_id but only one AFDM per sample_id, 
# let's sum each leaf area measurement in each sample, and compare the total area to the AFDM
total.leaf.area <- leafarea_all %>% 
  group_by(site_id, transect, sample_id) %>%
  summarise(total.leafarea = sum(result, na.rm = TRUE), .groups = "drop")

avg.shoot.height <- shootheight_all %>%
  group_by(site_id, transect, sample_id) %>%
  summarise(avg.shootheight = mean(result, na.rm = TRUE), .groups = "drop")

total.shoot.density <- shootdensity_all %>%
  group_by(site_id, transect, sample_id) %>%
  summarise(total.shootdensity = sum(result, na.rm = TRUE), .groups = "drop")

total.ag.biomass <- AG_AFDM_all %>%
  group_by(site_id, transect, sample_id) %>%
  summarise(total.agbiomass = sum(result, na.rm = TRUE), .groups = "drop")

area.biomass.df <- total.leaf.area %>%
  left_join(total.ag.biomass, by = c("site_id", "transect", "sample_id")) %>%
  left_join(avg.shoot.height, by = c("site_id", "transect", "sample_id")) %>%
  left_join(total.shoot.density, by = c("site_id", "transect", "sample_id"))

#screen for inappropriate negatives - CHECKPOINT
negatives <- area.biomass.df %>%
  filter(if_any(where(is.numeric), ~. < 0))

#screen for missing sample/transect info - CHECKPOINT
no.transect <- area.biomass.df %>%
  filter(if_any(where(is.character), ~. == ""))

#screen for mismatches in sample/transect info
sample.mismatch <- eelgrass %>%
  filter(!str_starts(sample_id, paste0(recode(transect, 
                                              inner = "in",
                                              mid = "mid",
                                              outer = "out",
                                              cross = "cross",
                                              patch = "patch"),
                                              ".")
                     )
         )


# Leaf area to Above Ground Biomass plot - VISUAL CHECKPOINT

LA_biomass_plot <- area.biomass.df %>%
  ggplot(aes(x = total.leafarea, y = total.agbiomass, color = sample_id)) +
  geom_point(size = 4) +
  theme_bw() + 
  facet_wrap(~transect, scales = "free") +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18))+
  theme(strip.text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_color_viridis_d(alpha = 0.6) +
  ylab("Above Ground Biomass (mg)") + xlab("Total Leaf Area per Sample (cm_sq)")

ggplotly(LA_biomass_plot)

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/QAQC/leafarea_vs_AGbiomass.jpg", 
              LA_biomass_plot,
              height = 10, width = 12,
              dpi = 400)

# Average shoot height vs total leaf area - VISUAL CHECKPOINT
LA_shootheight_plot <- area.biomass.df %>%
  ggplot(aes(x = total.leafarea, y = avg.shootheight, color = sample_id)) +
  geom_point(size = 4) +
  theme_bw() + 
  facet_wrap(~transect, scales = "free") +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18))+
  theme(strip.text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_color_viridis_d(alpha = 0.6) +
  xlab("Total Leaf Area per Sample (cm_sq)") + ylab("Average Shoot Height per Sample (cm)")

ggplotly(LA_shootheight_plot) 

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/QAQC/shootheight_vs_leafarea.jpg", 
       LA_shootheight_plot,
       height = 10, width = 12,
       dpi = 400)

#Shoot density vs AG biomass - VISUAL CHECKPOINT
biomass_shootdensity_plot <- area.biomass.df %>%
  ggplot(aes(x = total.shootdensity, y = total.agbiomass, color = sample_id)) +
  geom_point(size = 4) +
  theme_bw() +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18))+
  theme(strip.text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  facet_wrap(~transect, scales = "free") +
  scale_color_viridis_d(alpha = 0.6) +
  ylab("Above Ground Biomass (mg)") + xlab("Total Shoot Density per Quadrat (count)")

ggplotly(biomass_shootdensity_plot)
  
ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/QAQC/shootdensity_vs_AGbiomass.jpg", 
       biomass_shootdensity_plot,
       height = 10, width = 12,
       dpi = 400)

#Above Ground vs Belowground


# Additional structural metrics


## score each structural metric by site ----

### bin values into quartiles based on rank-order
AG_TOCTN_score4 <- AG_TOC_TN_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(AG_TOCTN_score = ntile(mean_result, 4))

BG_TOCTN_score4 <- BG_TOC_TN_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(BG_TOCTN_score = ntile(mean_result, 4))

shootheight_score4 <- shootheight_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(shootheight_score = ntile(mean_result, 4))

shootdensity_score4 <- shootdensity_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(shootdensity_score = ntile(mean_result, 4))

percentcover_score4 <- percentcover_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(percentcover_score = ntile(mean_result, 4)) 

percentmacroalgae_score4 <- percentmacroalgae_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(percentmacroalgae_score = ntile(mean_result, 4))

floweringshoot_score4 <- floweringshoot_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(floweringshoot_score = ntile(mean_result, 4))

shootcount_score4 <- shootcount_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(shootcount_score = ntile(mean_result, 4))

leafarea_score4 <- leafarea_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(leafarea_score = ntile(mean_result, 4))

epiphyteAFDM_score4 <- epiphyteAFDM_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(epiphyteAFDM_score = ntile(mean_result, 4))

shootwidth_score4 <- shootwidth_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(shootwidth_score = ntile(mean_result, 4))

AG_AFDM_score4 <- AG_AFDM_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(AG_AFDM_score = ntile(mean_result, 4))

BG_AFDM_score4 <- BG_AFDM_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(BG_AFDM_score = ntile(mean_result, 4))

LPS_score4 <- LPS_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(LPS_score = ntile(mean_result, 4))

AG_TOC_score4 <- AG_TOC_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(AG_TOC_score = ntile(mean_result, 4))

AG_TN_score4 <- AG_TN_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(AG_TN_score = ntile(mean_result, 4))

BG_TOC_score4 <- BG_TOC_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(BG_TOC_score = ntile(mean_result, 4))

BG_TN_score4 <- BG_TN_all %>%
  group_by(site_id) %>%
  summarize(mean_result = mean(result, na.rm = T)) %>%
  mutate(BG_TN_score = ntile(mean_result, 4))

## attach scores to metadata
#trim metadata
eelgrass_meta <- eelgrass %>%
  select(project_id, site_id, waterbody, distance_to_inlet, 
         inlet_status, site_protection, site_protection_granular, conservation_area,
         natural_or_restored,
         species, central_depth_mhw, nominal_lat, nominal_lon, 
         bed_type, stratum, sample_n, sample_date, bed_type,
         bed_perimeter, field_org, collection_method, wasting_disease,
         stratum)

#list all score dfs
score_dfs_4 <-list(shootheight_score4,
                       shootdensity_score4,
                       percentcover_score4,
                       percentmacroalgae_score4,
                       floweringshoot_score4,
                       shootcount_score4,
                       shootwidth_score4,
                       leafarea_score4,
                       LPS_score4,
                       epiphyteAFDM_score4,
                       AG_AFDM_score4,
                       BG_AFDM_score4,
                   AG_TOC_score4,
                   AG_TN_score4, 
                   BG_TOC_score4,
                   BG_TN_score4,
                   AG_TOCTN_score4,
                   BG_TOCTN_score4)

# create df pf all structural metric scores
metric_scores4 <- reduce(score_dfs_4, left_join, by = "site_id")
metric_scores4_meta <- metric_scores4 %>%
  left_join(eelgrass_meta, by = "site_id")

## output data into intermediate folder
write.csv(metric_scores4_meta, here("data", "intermediate", "eelgrass_ALL_structural_metric_scores.csv"))

# manually adjust headings in excel - drop edited spreadsheet into 'final' data folder

# read back in 'final' structural metric data 
eelgrass_structural_metrics <- read.csv(here("data", "final", "eelgrass_ALL_structural_metric_scores_FINAL.csv"))

#recall other data from above
head(perim)
head(area.wts)
  
#clean bed perimeter - some NAs and NRs
eelgrass_cleanbed <- eelgrass %>%
  mutate(
    bed_perimeter = as.numeric(bed_perimeter)
  )

## scoring tool ----

perim.scores<-eelgrass_cleanbed %>% 
  mutate(bench.4=quantile(bed_perimeter, p=0.75, na.rm = TRUE),
         bench.3=quantile(bed_perimeter, p=0.5, na.rm = TRUE),
         bench.2=quantile(bed_perimeter, p=0.25, na.rm = TRUE),
         perimeter_score=case_when(bed_perimeter<bench.2~1,
                                   bed_perimeter>=bench.2 & bed_perimeter<bench.3~2,
                                   bed_perimeter>=bench.3 & bed_perimeter<bench.4~3,
                                   bed_perimeter>=bench.4 ~4))

perim.scores1<-perim.scores %>% 
  select(site_id, perimeter_score)

scores.2<- eelgrass_structural_metrics %>% 
  select(site_id, shootheight_score, shootdensity_score, percentcover_score, shootwidth_score, leafarea_score, LPS_score, 
         BG_AFDM_score, epiphyteAFDM_score,AG_AFDM_score, floweringshoot_score, shootcount_score, AG_TOC_score, AG_TN_score,
         BG_TOC_score, BG_TN_score, AG_TOCTN_score, BG_TOCTN_score) %>% 
  left_join(., perim.scores1, by="site_id", relationship = "many-to-many") %>% 
  mutate(masterid=site_id, site_id=str_sub(masterid, 7) )

#### CLUNKY WAY - DON'T use this one
index.scores<-scores.2 %>% 
  mutate(substrate.stabilization=(BG_AFDM_score+shootdensity_score+ shootheight_score+leafarea_score)/4,
         carbon.sequestration=(AG_AFDM_score+BG_AFDM_score)/2,
         primary.production=(AG_AFDM_score+ BG_AFDM_score+shootdensity_score+LPS_score+floweringshoot_score+leafarea_score+epiphyteAFDM_score+(AG_TOC_score+AG_TN_score)/2+(BG_TOC_score+BG_TN_score)/2)/9,
         secondary.production=(AG_AFDM_score+ BG_AFDM_score+shootdensity_score+LPS_score+shootheight_score+ epiphyteAFDM_score + (AG_TOC_score+AG_TN_score)/2)/7,
         wq=(AG_AFDM_score+percentcover_score+shootdensity_score+leafarea_score)/4,
         nekton.habitat=(percentcover_score+shootdensity_score+shootheight_score+leafarea_score)/4,
         waterfowl.habitat=(shootheight_score+shootdensity_score)/2, na.rm = TRUE) %>% 
  select(site_id, masterid, substrate.stabilization, carbon.sequestration, primary.production, secondary.production, wq, nekton.habitat, waterfowl.habitat) %>% 
  pivot_longer(cols=c(-site_id, -masterid), names_to = "function_", values_to = "fun_score") %>% 
  group_by(site_id) %>% 
  mutate(overall_score=mean(fun_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(fun_cond=case_when(fun_score>3 ~"good condition",
                            fun_score<2 ~"poor condition",
                            TRUE~"moderate condition"),
         overall_cond=case_when(overall_score>3~"good condition",
                                overall_score<2~"poor condition",
                                TRUE~"moderate condition")) %>%
  distinct()
#### CALCULATE INDEX SCORES THIS WAY INSTEAD - 3 categories
index.scores <- scores.2 %>%
  mutate(
    substrate.stabilization = rowMeans(
      cbind(BG_AFDM_score, shootdensity_score, shootheight_score, leafarea_score),
      na.rm = TRUE
    ),
    
    carbon.sequestration = rowMeans(
      cbind(AG_AFDM_score, BG_AFDM_score),
      na.rm = TRUE
    ),
    
    primary.production = rowMeans(
      cbind(
        AG_AFDM_score,
        BG_AFDM_score,
        shootdensity_score,
        LPS_score,
        floweringshoot_score,
        leafarea_score,
        epiphyteAFDM_score,
        rowMeans(cbind(AG_TOC_score, AG_TN_score), na.rm = TRUE),
        rowMeans(cbind(BG_TOC_score, BG_TN_score), na.rm = TRUE)
      ),
      na.rm = TRUE
    ),
    
    secondary.production = rowMeans(
      cbind(
        AG_AFDM_score,
        BG_AFDM_score,
        shootdensity_score,
        LPS_score,
        shootheight_score,
        epiphyteAFDM_score,
        rowMeans(cbind(AG_TOC_score, AG_TN_score), na.rm = TRUE)
      ),
      na.rm = TRUE
    ),
    
    wq = rowMeans(
      cbind(AG_AFDM_score, percentcover_score, shootdensity_score, leafarea_score),
      na.rm = TRUE
    ),
    
    nekton.habitat = rowMeans(
      cbind(percentcover_score, shootdensity_score, shootheight_score, leafarea_score),
      na.rm = TRUE
    ),
    
    waterfowl.habitat = rowMeans(
      cbind(shootheight_score, shootdensity_score),
      na.rm = TRUE
    )
  ) %>%
  select(
    site_id, masterid,
    substrate.stabilization, carbon.sequestration,
    primary.production, secondary.production,
    wq, nekton.habitat, waterfowl.habitat
  ) %>%
  pivot_longer(
    cols = -c(site_id, masterid),
    names_to = "function_",
    values_to = "fun_score"
  ) %>%
  group_by(site_id) %>%
  mutate(overall_score = mean(fun_score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    fun_cond = case_when(
      fun_score > 3 ~ "good condition",
      fun_score < 2 ~ "poor condition",
      TRUE ~ "moderate condition"
    ),
    overall_cond = case_when(
      overall_score > 3 ~ "good condition",
      overall_score < 2 ~ "poor condition",
      TRUE ~ "moderate condition"
    )
  ) %>%
  distinct()

# area-weighted index scores
wt.index.scores<-index.scores %>% 
  left_join(., select(station.info, project_id, site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody,
                      bed_type, sample_date, central_depth_mhw, distance_to_inlet, inlet_status,
                      site_protection, natural_or_restored, species),
            by=c("masterid" = "site_id"), relationship = "many-to-many") %>%
  mutate(overall_cond = factor(overall_cond),
         #wgt = as.numeric(wgt),
         fun_cond = factor(fun_cond)) %>%
  distinct()

#Remember to specify the horvitz thompson variance estimator in this new version of spsurvey
wt.overall.cond<-wt.index.scores %>% 
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond), !is.na(nominal_lat), !is.na(nominal_lon)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, overall_score, overall_cond) %>% 
  cat_analysis(., siteID = "site_id", vars="overall_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", vartype = "HT")

wt.overall.cond.2<-wt.index.scores %>% 
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond), !is.na(nominal_lat), !is.na(nominal_lon)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, overall_score, overall_cond) %>% 
  cat_analysis(., siteID = "site_id", vars="overall_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", subpops="stratum", vartype = "HT") %>% 
  bind_rows(wt.overall.cond)

#jill's version
wt.index.scores.f <- wt.index.scores %>%
  mutate(
    across(where(is.factor), as.character),
    across(where(is.character), ~ na_if(.x, "NR"))
  ) %>%
  mutate(across(where(is.character), as.factor))
 
# this one is correct for overall
wt.overall.cond.bight <- wt.index.scores.f %>% 
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id),
         !is.na(wgt),
         !is.na(overall_score),
         !is.na(nominal_lat),
         !is.na(nominal_lon)) %>%
  mutate(wgt = as.numeric(wgt)) %>%
  mutate(site_id = as.character(site_id)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt,
           waterbody, overall_score, overall_cond) %>% 
  mutate(overall_cond = as.character(overall_cond)) %>%
  cat_analysis(siteID = "site_id",
               vars = "overall_cond",
               weight = "wgt",
               xcoord = "nominal_lon",
               ycoord = "nominal_lat",
               vartype = "HT")
wt.overall.cond.bight

wt.overall.cond.bight.2<-wt.index.scores.f %>% 
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), 
         !is.na(overall_cond), 
         !is.na(nominal_lat), 
         !is.na(nominal_lon)) %>%
  mutate(wgt = as.numeric(wgt)) %>%
  mutate(site_id = as.character(site_id)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, overall_score, overall_cond) %>% 
  cat_analysis(., siteID = "site_id", vars="overall_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", subpops="stratum", vartype = "HT") %>% 
  bind_rows(wt.overall.cond.bight)

write.csv(wt.overall.cond.bight.2, here("data", "final", "BIGHT ONLY Overall Condition Scores by stratum - draft.csv"))

# Calculate index scores using 4 categories - poor, moderate, good, excellent
#step 1: combine and score functions
index.scores.4 <- scores.2 %>%
  mutate(
    substrate.stabilization = rowMeans(
      cbind(BG_AFDM_score, shootdensity_score, shootheight_score, leafarea_score),
      na.rm = TRUE
    ),
    
    carbon.sequestration = rowMeans(
      cbind(AG_AFDM_score, BG_AFDM_score),
      na.rm = TRUE
    ),
    
    primary.production = rowMeans(
      cbind(
        AG_AFDM_score,
        BG_AFDM_score,
        shootdensity_score,
        LPS_score,
        floweringshoot_score,
        leafarea_score,
        epiphyteAFDM_score,
        rowMeans(cbind(AG_TOC_score, AG_TN_score), na.rm = TRUE),
        rowMeans(cbind(BG_TOC_score, BG_TN_score), na.rm = TRUE)
      ),
      na.rm = TRUE
    ),
    
    secondary.production = rowMeans(
      cbind(
        AG_AFDM_score,
        BG_AFDM_score,
        shootdensity_score,
        LPS_score,
        shootheight_score,
        epiphyteAFDM_score,
        rowMeans(cbind(AG_TOC_score, AG_TN_score), na.rm = TRUE)
      ),
      na.rm = TRUE
    ),
    
    wq = rowMeans(
      cbind(AG_AFDM_score, percentcover_score, shootdensity_score, leafarea_score),
      na.rm = TRUE
    ),
    
    nekton.habitat = rowMeans(
      cbind(percentcover_score, shootdensity_score, shootheight_score, leafarea_score),
      na.rm = TRUE
    ),
    
    waterfowl.habitat = rowMeans(
      cbind(shootheight_score, shootdensity_score),
      na.rm = TRUE
    )
  ) %>%
  select(
    site_id, masterid,
    substrate.stabilization, carbon.sequestration,
    primary.production, secondary.production,
    wq, nekton.habitat, waterfowl.habitat
  ) %>%
  pivot_longer(
    cols = -c(site_id, masterid),
    names_to = "function_",
    values_to = "fun_score"
  ) %>%
  group_by(site_id) %>%
  mutate(overall_score = mean(fun_score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    fun_cond = cut(fun_score,
                   breaks = c(1, 1.75, 2.5, 3.25, 4),
                   labels = c("poor condition",
                              "moderate condition",
                              "good condition",
                              "excellent condition"),
                   include.lowest = TRUE),
    overall_cond = cut(overall_score,
                       breaks = c(1,1.75, 2.5, 3.25, 4),
                       labels = c("poor condition",
                                  "moderate condition",
                                  "good condition",
                                  "excellent condition"),
                       include.lowest = TRUE)
  ) %>%
  distinct()

#assign target vs random sampling
selection.df <- nmds.scores %>% select(masterid, selection)

index.scores.4.sl <-index.scores.4 %>%
  left_join(selection.df, by = "masterid", relationship = "many-to-many")

# step 2: calculate area-weighted function and condition scores
# area-weighted index scores
wt.index.scores.4 <-index.scores.4 %>% 
  left_join(., select(station.info, project_id, site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody,
                      bed_type, sample_date, central_depth_mhw, distance_to_inlet, inlet_status,
                      site_protection, site_protection_granular, natural_or_restored, species),
            by=c("masterid" = "site_id"), relationship = "many-to-many") %>%
  mutate(overall_cond = factor(overall_cond),
         #wgt = as.numeric(wgt),
         fun_cond = factor(fun_cond)) %>%
  distinct()

# class check
wt.index.scores.four <- wt.index.scores.4 %>%
  mutate(
    across(where(is.factor), as.character),
    across(where(is.character), ~ na_if(.x, "NR"))
  ) %>%
  mutate(across(where(is.character), as.factor))

# calculate weighted overall condition for bight
wt.overall.cond.bight.four <- wt.index.scores.four %>% 
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id),
         !is.na(wgt),
         !is.na(overall_score),
         !is.na(nominal_lat),
         !is.na(nominal_lon)) %>%
  mutate(wgt = as.numeric(wgt)) %>%
  mutate(site_id = as.character(site_id)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt,
           waterbody, overall_score, overall_cond) %>% 
  mutate(overall_cond = as.character(overall_cond)) %>%
  cat_analysis(siteID = "site_id",
               vars = "overall_cond",
               weight = "wgt",
               xcoord = "nominal_lon",
               ycoord = "nominal_lat",
               vartype = "HT")
wt.overall.cond.bight.four

#part 2 of calculation
wt.overall.cond.bight.2.four <-wt.index.scores.four %>% 
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), 
         !is.na(overall_cond), 
         !is.na(nominal_lat), 
         !is.na(nominal_lon)) %>%
  mutate(wgt = as.numeric(wgt)) %>%
  mutate(site_id = as.character(site_id)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, overall_score, overall_cond) %>% 
  cat_analysis(., siteID = "site_id", vars="overall_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", subpops="stratum", vartype = "HT") %>% 
  bind_rows(wt.overall.cond.bight.four)

#write out dataset for 4 category overall condition by stratum
write.csv(wt.overall.cond.bight.2.four, here("data", "final", "BIGHT ONLY Overall Condition Scores - 4 cateogry - by stratum - draft.csv"))



#### David's version - do not use

wt.overall.cond.bight<-wt.index.scores.f %>% 
  filter(project_id =="B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond), !is.na(nominal_lat), !is.na(nominal_lon)) %>%
  distinct(masterid, nominal_lon, nominal_lat, stratum, wgt, waterbody, overall_score, overall_cond) %>% 
  cat_analysis(., siteID = "masterid", vars="overall_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", vartype = "HT")

###
wt.overall.cond.bight.2<-wt.index.scores.f %>% 
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond), !is.na(nominal_lat), !is.na(nominal_lon)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, overall_score, overall_cond) %>% 
  cat_analysis(., siteID = "site_id", vars="overall_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", subpops="stratum", vartype = "HT") %>% 
  bind_rows(wt.overall.cond)
#####################
#write.csv(wt.overall.cond.2, here("data", "final", "BIGHT ONLY Overall Condition Scores by stratum - draft.csv"))

#### target vs random boxplots
# were the targeted sites in "better" condition?

overall.boxplot.target <- ggplot(mapping = aes(x = " ", y = overall_score)) +
                              geom_boxplot(data = index.scores.4.sl %>% filter(selection == "probabilistic"), fill = "azure2") +
  geom_point(data = index.scores.4.sl %>% filter(selection == "targeted"), fill = "turquoise", alpha = 0.5, color = "grey20", shape = 21, size = 4) +
  annotate("text", x = " ", y = targets.median, label = "X", size = 12, color = "#d8006f") +
  theme_bw() + xlab(" ") + ylab("Overall Condition Score") +
  theme(axis.title.y = element_text(size = 14))
overall.boxplot.target

probs <- index.scores.4.sl %>% filter(selection == "probabilistic")
probs.median <- median(probs$overall_score)
probs.fun.median <- probs %>%
  group_by(function_) %>%
  summarize(median_score = median(fun_score, na.rm = T), .groups = "drop")

#medians for target sites
targets <- index.scores.4.sl %>% filter(selection == "targeted")
targets.median <- median(targets$overall_score)
targets.fun.median <- targets %>% 
  group_by(function_) %>%
  summarize(median_score = median(fun_score, na.rm = T), .groups = "drop")

functions.boxplot.target <- ggplot(mapping = aes(x = function_, y = fun_score)) +
  geom_boxplot(data = index.scores.4.sl %>% filter(selection == "probabilistic"), fill = "azure2") +
  geom_point(data = index.scores.4.sl %>% filter(selection == "targeted"), fill = "turquoise", alpha = 0.5, color = "grey20", shape = 21, size = 4) +
  geom_point(data = targets.fun.median, aes(x = function_, y = median_score), color = "deeppink", shape = 4, size = 12) +
  theme_bw() + xlab(" ") + ylab("Ecosystem Function Score") +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust =1),
        axis.title.y = element_text(size = 14))

functions.boxplot.target

target.boxplots <- plot_grid(overall.boxplot.target, functions.boxplot.target, ncol = 1)
target.boxplots

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/targetsite.boxplots.jpg", 
       target.boxplots,
       height = 8, width = 10,
       dpi = 300)


## tables ----
# Three category tables
#### summary table of each function by stratum
summary_stratum_function <- wt.index.scores %>%
  filter(project_id == "B23-SAV") %>%
  group_by(stratum, function_) %>%
  summarise(
    mean = round(mean(fun_score, na.rm = TRUE), 3),
    sd = round(sd(fun_score, na.rm = TRUE), 3),
    min = round(min(fun_score, na.rm = TRUE), 2),
    max = round(max(fun_score, na.rm = TRUE), 2),
    range = max - min,
    .groups = "drop") %>% 
  mutate(
    fun_condition = case_when(
      mean > 3 ~ "good condition",
      mean < 2 ~ "poor condition",
      TRUE ~ "moderate condition"))

summary_table_stratum_byfunction <- summary_stratum_function %>%
  gt() %>% tab_caption("Eelgrass Ecosystem Function Scores Summary Stats by stratum")
                                  

write.csv(
  summary_table_stratum_byfunction,
  here("output", "tables", "stratum_function_summary.csv"),
  row.names = FALSE
)

## summary table - mean overall scores by waterbody
summary_overall_waterbody <- wt.index.scores %>%
  filter(project_id == "B23-SAV") %>%
  group_by(waterbody) %>%
  summarise(
    mean = round(mean(overall_score, na.rm = TRUE), 3),
    sd = round(sd(overall_score, na.rm = TRUE), 3),
    min = round(min(overall_score, na.rm = TRUE), 2),
    max = round(max(overall_score, na.rm = TRUE), 2),
    range = max - min,
    .groups = "drop"
  ) %>%
  mutate(overall_cond=case_when(mean>3~"good condition",
                         mean<2~"poor condition",
                         TRUE~"moderate condition"))
summary_overall_waterbody

write.csv(
  summary_overall_waterbody,
  here("output", "tables", "waterbody_overall_summary.csv"),
  row.names = FALSE
)

# four category tables
#### summary table of each function by stratum
summary_stratum_function_four <- wt.index.scores.four %>%
  filter(project_id == "B23-SAV") %>%
  group_by(stratum, function_) %>%
  summarise(
    mean = round(mean(fun_score, na.rm = TRUE), 3),
    sd = round(sd(fun_score, na.rm = TRUE), 3),
    min = round(min(fun_score, na.rm = TRUE), 2),
    max = round(max(fun_score, na.rm = TRUE), 2),
    range = max - min,
    .groups = "drop") %>% 
  mutate(
    fun_condition = cut(mean,
                        breaks = c(1,1.75, 2.5, 3.25, 4),
                        labels = c( "poor condition", "moderate condition",
                                    "good condition", "excellent condition"),
                        include.lowest = TRUE))


summary_table_stratum_byfunction_four <- summary_stratum_function_four %>%
  gt() %>% tab_caption("Eelgrass Ecosystem Function Scores Summary Stats by stratum - four category")


write.csv(
  summary_table_stratum_byfunction_four,
  here("output", "tables", "stratum_function_summary_fourcat.csv"),
  row.names = FALSE
)

## summary table - mean overall scores by waterbody
summary_overall_waterbody_four <- wt.index.scores.four %>%
  filter(project_id == "B23-SAV") %>%
  group_by(waterbody) %>%
  summarise(
    mean = round(mean(overall_score, na.rm = TRUE), 3),
    sd = round(sd(overall_score, na.rm = TRUE), 3),
    min = round(min(overall_score, na.rm = TRUE), 2),
    max = round(max(overall_score, na.rm = TRUE), 2),
    range = max - min,
    .groups = "drop"
  ) %>%
  mutate(overall_cond=cut(mean,
                          breaks = c(1, 1.75, 2.5, 3.25, 4),
                          labels = c("poor condition", "moderate condition",
                                     "good condition", "excellent condition"),
                          include.lowest = TRUE))
summary_overall_waterbody_four

write.csv(
  summary_overall_waterbody_four,
  here("output", "tables", "waterbody_overall_summary_fourcat.csv"),
  row.names = FALSE
)

## summary table - overall site mastertable

write.csv(wt.index.scores.four, here("output", "tables", "site_scores_detailed_BIGHT.csv"), row.names = F)


#summary table - sites in each stratum and waterbody

summary_site_strata <- wt.index.scores.4 %>%
  distinct(waterbody, masterid, stratum)

write.csv(summary_site_strata, here("output", "tables", "site_waterbody_stratum_info.csv"), row.names = F)

# condition plots
# overall condition plot - three categories
overall.plot.1<-wt.overall.cond.bight.2 %>% 
  filter(Category!="Total") %>% 
  mutate(stratum=factor(Subpopulation, levels = c("All Sites", "large_embayment", "small_embayment", "estuary"), 
                        labels = c("Southern California\nBight", "Large Embayments", "Small Embayments", "Estuaries")),
         overall.cond=factor(Category, levels=c("poor condition", "moderate condition", "good condition"))) %>% 
  naniar::replace_with_na(., replace=list(Estimate.P=0)) %>% 
  ggplot(., aes(x = stratum, y = Estimate.P)) +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P, color = overall.cond),
                width = 0.15, linewidth = 0.8, position = position_dodge_val) +
  geom_point(aes(fill = overall.cond),
             shape = 21, size = 6, stroke = 1.2, color = "black", position = position_dodge_val) +
  scale_fill_manual(values = c("poor condition" = "#ff6600", 
                               "moderate condition" = "peachpuff3",
                               "good condition" = "cadetblue"), 
                    labels = c("Poor", "Moderate", "Good"),
                    name = "Overall\nCondition") +
  scale_color_manual(values = c("poor condition" = "#ff6600", 
                                "moderate condition" = "peachpuff3",
                                "good condition" = "cadetblue"), guide = "none") +
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face="bold", size=18),
        axis.text.x=element_text(face="bold", size=16, hjust = 1, angle = 45),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))+
  ylab("Percent of Area Bight-Wide (+/- 95% CI)") + xlab("Stratum")


overall.plot.1

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/Overall.index.scores-area.weighted.jpg", 
      overall.plot.1,
       height = 10, width = 10,
       dpi = 300)

#overall plot - 4 categories
position_dodge_val <- position_dodge(width = 0.6)

overall.plot.1.four <-wt.overall.cond.bight.2.four %>% 
  filter(Category!="Total") %>% 
  mutate(stratum=factor(Subpopulation, levels = c("All Sites", "large_embayment", "small_embayment", "estuary"), 
                        labels = c("Overall\nSouthern California\nBight", "Large\nEmbayments", "Small\nEmbayments", "Estuaries")),
         overall.cond=factor(Category, levels=c("excellent condition", "good condition", "moderate condition", "poor condition"))) %>% 
  naniar::replace_with_na(., replace=list(Estimate.P=0)) %>%
  ggplot(., aes(x = stratum, y = Estimate.P)) +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P, color = overall.cond),
                 width = 0.15, linewidth = 0.8, position = position_dodge_val) +
  geom_point(aes(fill = overall.cond),
                 shape = 21, size = 6, stroke = 1.2, color = "black", position = position_dodge_val) +
  scale_fill_manual(values = c("excellent condition" = "turquoise1",
                               "good condition" = "cadetblue",
                               "moderate condition" = "peachpuff3",
                               "poor condition" = "#ff6600"), 
                    labels = c("Best Observed", "Good", "Moderate", "Lowest Observed"),
                               name = "Overall\nCondition") +
  scale_color_manual(values = c("excellent condition" = "turquoise1", 
                                "good condition" = "cadetblue",
                                "moderate condition" = "peachpuff3",
                                "poor condition" = "#ff6600"),
                               guide = "none") +
  theme_bw()+
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey30")+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face="bold", size=18),
        axis.text.x=element_text(face="bold", size=16, hjust = 1, angle = 45),
        legend.text = element_text(size = 16),
          legend.title = element_text(size = 18))+
  ylab("Percent of Area Bight-Wide (+/- 95% CI)") + xlab("Stratum")

overall.plot.1.four

##### Overall 4 category panel plot - jill v2 ######

overall.plot.1.STRATUM <-wt.overall.cond.bight.2.four %>% 
  filter(Category!="Total") %>% 
  filter(Subpopulation != "All Sites") %>%
  mutate(stratum=factor(Subpopulation, levels = c("large_embayment", "small_embayment", "estuary"), 
                        labels = c("Large\nEmbayments", "Small\nEmbayments", "Estuaries")),
         overall.cond=factor(Category, levels=c("excellent condition", "good condition", "moderate condition", "poor condition"))) %>% 
  naniar::replace_with_na(., replace=list(Estimate.P=0)) %>%
  ggplot(., aes(x = stratum, y = Estimate.P)) +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P, color = overall.cond),
                width = 0.15, linewidth = 0.8, position = position_dodge_val) +
  geom_point(aes(fill = overall.cond),
             shape = 21, size = 6, stroke = 1.2, color = "black", position = position_dodge_val) +
  scale_fill_manual(values = c("excellent condition" = "turquoise1",
                               "good condition" = "cadetblue",
                               "moderate condition" = "peachpuff3",
                               "poor condition" = "#ff6600"), 
                    labels = c("Best Observed", "Good", "Moderate", "Lowest Observed"),
                    name = "Overall\nCondition") +
  scale_color_manual(values = c("excellent condition" = "turquoise1", 
                                "good condition" = "cadetblue",
                                "moderate condition" = "peachpuff3",
                                "poor condition" = "#ff6600"),
                     guide = "none") +
  theme_bw()+
 # geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey30")+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face="bold", size=18),
        axis.text.x=element_text(face="bold", size=16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))+
  ylab("Percent Area of Stratum (+/- 95% CI)") + xlab("") +
  theme(legend.position = "none")

overall.plot.1.STRATUM

overall.plot.1.SCB <-wt.overall.cond.bight.2.four %>% 
  filter(Category!="Total") %>% 
  filter(Subpopulation == "All Sites") %>%
  mutate(stratum=factor(Subpopulation, levels = c("All Sites"), 
                        labels = c("Overall\nSouthern California\nBight")),
         overall.cond=factor(Category, levels=c("excellent condition", "good condition", "moderate condition", "poor condition"))) %>% 
  naniar::replace_with_na(., replace=list(Estimate.P=0)) %>%
  ggplot(., aes(x = stratum, y = Estimate.P)) +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P, color = overall.cond),
                width = 0.15, linewidth = 0.8, position = position_dodge_val) +
  geom_point(aes(fill = overall.cond),
             shape = 21, size = 6, stroke = 1.2, color = "black", position = position_dodge_val) +
  scale_fill_manual(values = c("excellent condition" = "turquoise1",
                               "good condition" = "cadetblue",
                               "moderate condition" = "peachpuff3",
                               "poor condition" = "#ff6600"), 
                    labels = c("Best Observed", "Good", "Moderate", "Lowest Observed"),
                    name = "Overall\nCondition") +
  scale_color_manual(values = c("excellent condition" = "turquoise1", 
                                "good condition" = "cadetblue",
                                "moderate condition" = "peachpuff3",
                                "poor condition" = "#ff6600"),
                     guide = "none") +
  theme_bw()+
 # geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey30")+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face="bold", size=18),
        axis.text.x=element_text(face="bold", size=16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))+
  ylab("Percent Area of Region (+/- 95% CI)") + xlab("") 

overall.plot.1.SCB

overall.plot.cow <- plot_grid(overall.plot.1.SCB, overall.plot.1.STRATUM, ncol = 1)
overall.plot.cow

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/Overall.index.scores-cowplot.jpg", 
       overall.plot.cow,
       height = 10, width = 8,
       dpi = 300)
##### doing area weighting for individual functions ######

#regional-scale - not going to amend to indivual strata, as it will make plotting messier
wt.fun.cond<-wt.index.scores %>% 
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond), !is.na(nominal_lat), !is.na(nominal_lon), !is.na(fun_cond)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, function_,fun_score, fun_cond) %>% 
  nest(dat=c(-function_)) %>% 
  mutate(catty=purrr::map(.x=dat, ~cat_analysis(., siteID = "site_id", vars="fun_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", vartype = "HT"))) %>% 
  unnest(catty)
write.csv(select(wt.fun.cond, -dat), here("data", "final", "BIGHT function conditions bight-wide - draft.csv"))  

wt.fun.cond.2<-wt.index.scores %>% 
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond), !is.na(nominal_lat), !is.na(nominal_lon), !is.na(fun_cond)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, function_,fun_score, fun_cond) %>% 
  nest(dat=c(-function_)) %>% 
  mutate(catty=purrr::map(.x=dat, ~cat_analysis(., siteID = "site_id", vars="fun_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", subpops="stratum",, vartype = "HT")) )%>%
  unnest(catty)# %>% #omitting the binding, I think this is best for vizualization
#bind_rows(wt.fun.cond)


write.csv(select(wt.fun.cond, -dat), here("data", "final", "BIGHT function conditions bight-wide - draft.csv"))  

wt.fun.cond.bight<-wt.index.scores.f %>% 
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond), !is.na(nominal_lat), !is.na(nominal_lon), !is.na(fun_cond)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, function_,fun_score, fun_cond) %>% 
  mutate(wgt = as.numeric(wgt)) %>%
  mutate(site_id = as.character(site_id)) %>%
  nest(dat=c(-function_)) %>% 
  mutate(catty=purrr::map(.x=dat, ~cat_analysis(., siteID = "site_id", vars="fun_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", vartype = "HT"))) %>% 
  unnest(catty)

wt.fun.cond.2.bight<-wt.index.scores %>% 
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond), !is.na(nominal_lat), !is.na(nominal_lon), !is.na(fun_cond)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, function_,fun_score, fun_cond) %>% 
  mutate(wgt = as.numeric(wgt)) %>%
  mutate(site_id = as.character(site_id)) %>%
  nest(dat=c(-function_)) %>% 
  mutate(catty=purrr::map(.x=dat, ~cat_analysis(., siteID = "site_id", vars="fun_cond", weight="wgt", xcoord="nominal_lon", ycoord="nominal_lat", subpops="stratum",, vartype = "HT")) )%>%
  unnest(catty)# %>% #omitting the binding, I think this is best for vizualization
#bind_rows(wt.fun.cond)


#try this non-David way instead - 3 category
wt.fun.cond.bight <- wt.index.scores %>%
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond),
         !is.na(nominal_lat), !is.na(nominal_lon), !is.na(fun_cond)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, function_, fun_score, fun_cond) %>%
  nest(dat = c(-function_)) %>%
  filter(map_lgl(dat, ~ nrow(.) > 0)) %>%
  mutate(catty = map(dat, ~ {
    df <- .
    # Convert fun_cond to numeric
    df$fun_cond_num <- as.numeric(factor(df$fun_cond,
                                         levels = c("poor condition",
                                                    "moderate condition",
                                                    "good condition")))
    cat_analysis(
      df,
      siteID = "site_id",
      vars = "fun_cond_num",
      weight = "wgt",
      xcoord = "nominal_lon",
      ycoord = "nominal_lat",
      vartype = "HT"
    )
  })) %>%
  unnest(catty)

# four category area-weighted function scores
wt.fun.cond.bight.four <- wt.index.scores.four %>%
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond),
         !is.na(nominal_lat), !is.na(nominal_lon), !is.na(fun_cond)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, function_, fun_score, fun_cond) %>%
  nest(dat = c(-function_)) %>%
  filter(map_lgl(dat, ~ nrow(.) > 0)) %>%
  mutate(catty = map(dat, ~ {
    df <- .
    # Convert fun_cond to numeric
    df$fun_cond_num <- as.numeric(factor(df$fun_cond,
                                         levels = c("poor condition",
                                                    "moderate condition",
                                                    "good condition",
                                                    "excellent condition")))
    cat_analysis(
      df,
      siteID = "site_id",
      vars = "fun_cond_num",
      weight = "wgt",
      xcoord = "nominal_lon",
      ycoord = "nominal_lat",
      vartype = "HT"
    )
  })) %>%
  unnest(catty)

#strata-level summary dataset with four categories
wt.fun.cond.2.bight.four <-wt.index.scores.four %>%
  filter(project_id == "B23-SAV") %>%
  filter(!is.na(site_id), !is.na(wgt), !is.na(overall_cond),
         !is.na(nominal_lat), !is.na(nominal_lon), !is.na(fun_cond)) %>%
  distinct(site_id, nominal_lon, nominal_lat, stratum, wgt, waterbody, function_, fun_score, fun_cond) %>%
  nest(dat = c(-function_)) %>%
  filter(map_lgl(dat, ~ nrow(.) > 0)) %>%
  mutate(catty = map(dat, ~ {
    df <- .
    # Convert fun_cond to numeric
    df$fun_cond_num <- as.numeric(factor(df$fun_cond,
                                         levels = c("poor condition",
                                                    "moderate condition",
                                                    "good condition",
                                                    "excellent condition")))
    cat_analysis(
      df,
      siteID = "site_id",
      vars = "fun_cond_num",
      weight = "wgt",
      xcoord = "nominal_lon",
      ycoord = "nominal_lat",
      subpops = "stratum",
      vartype = "HT"
    )
  })) %>%
  unnest(catty)

write.csv(select(wt.fun.cond.2.bight.four, -dat), here("data", "final", "BIGHT ONLY function conditions by stratum - four category - draft.csv"))

# 3 cateory area-weighted function plot
bight.wide.fun.plots.panel <-wt.fun.cond.bight %>% 
  filter(Category!="Total") %>% 
  mutate(stratum=factor(Subpopulation, levels = c("All Sites", "large_embayment", "small_embayment", "estuary"),  
                        labels = c("Southern California\nBight", "Large Embayments", "Small Embayments", "Estuaries")),
         overall.cond=factor(Category, levels=c("poor condition", "moderate condition", "good condition")),
         eco.fun=case_when(function_=="substrate.stabilization"~ "Substrate\nStabilization",
                           function_=="carbon.sequestration"~ "Carbon\nSequestration",
                           function_=="primary.production"~ "Primary\nProduction",
                           function_=="secondary.production"~ "Secondary\nProduction",
                           function_=="wq"~ "Water\nQuality",
                           function_=="nekton.habitat"~ "Nekton\nHabitat",
                           function_=="waterfowl.habitat"~ "Waterfowl\nHabitat")) %>% 
  naniar::replace_with_na(., replace=list(Estimate.P=0)) %>% 
  ggplot(., aes(x = eco.fun, y = Estimate.P)) +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P, color = overall.cond),
                width = 0.15, linewidth = 0.8, position = position_dodge_val) +
  geom_point(aes(fill = overall.cond),
             shape = 21, size = 6, stroke = 1.2, color = "black", position = position_dodge_val) +
  scale_fill_manual(values = c("poor condition" = "#ff6600", 
                               "moderate condition" = "peachpuff3",
                               "good condition" = "cadetblue"), 
                    labels = c("Poor", "Moderate", "Good"),
                    name = "Function\nCondition") +
  scale_color_manual(values = c("poor condition" = "#ff6600", 
                                "moderate condition" = "peachpuff3",
                                "good condition" = "cadetblue"), guide = "none") +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), linetype=3)+
  labs(x="Ecosystem Function", y="Percent of Area Bight-Wide (+/- 95% CI)", fill="Function\nCondition")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face="bold", size=18),
        axis.text.x=element_text(face="bold", size=16, hjust = 1, angle = 45),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

bight.wide.fun.plots.panel

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/fun.scores.panel.jpg", 
       bight.wide.fun.plots.panel,
       height = 10, width = 10,
       dpi = 300)

write.csv(select(wt.fun.cond.bight.four, -dat), here("data", "final", "Bight-wide function scores - draft.csv"))

# ecosystem function by area - four category
bight.wide.fun.plots.panel.four <-wt.fun.cond.bight.four %>% 
  filter(Category!="Total") %>% 
  mutate(stratum=factor(Subpopulation, levels = c("All Sites", "large_embayment", "small_embayment", "estuary"),  
                        labels = c("Southern California\nBight", "Large Embayments", "Small Embayments", "Estuaries")),
        overall.cond = case_when(
          Category == "1" ~ "poor condition",
          Category == "2" ~ "moderate condition",
          Category == "3" ~ "good condition",
          Category == "4" ~ "excellent condition"),
         overall.cond=factor(overall.cond, levels=c("excellent condition", "good condition", "moderate condition", "poor condition")),
         eco.fun=case_when(function_=="substrate.stabilization"~ "Substrate\nStabilization",
                           function_=="carbon.sequestration"~ "Carbon\nSequestration",
                           function_=="primary.production"~ "Primary\nProduction",
                           function_=="secondary.production"~ "Secondary\nProduction",
                           function_=="wq"~ "Water\nQuality",
                           function_=="nekton.habitat"~ "Nekton\nHabitat",
                           function_=="waterfowl.habitat"~ "Waterfowl\nHabitat")) %>% 
  naniar::replace_with_na(., replace=list(Estimate.P=0)) %>% 
ggplot(., aes(x = eco.fun, y = Estimate.P)) +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P, color = overall.cond),
                width = 0.15, linewidth = 0.8, position = position_dodge_val) +
  geom_point(aes(fill = overall.cond),
             shape = 21, size = 6, stroke = 1.2, color = "black", position = position_dodge_val) +
  scale_fill_manual(values = c("excellent condition" = "turquoise1",
                               "good condition" = "cadetblue",
                               "moderate condition" = "peachpuff3",
                               "poor condition" = "#ff6600"), 
                    labels = c("Best Observed", "Good", "Moderate", "Lowest Observed"),
                    name = "Overall\nCondition") +
  scale_color_manual(values = c("excellent condition" = "turquoise1",
                                "good condition" = "cadetblue",
                                "moderate condition" = "peachpuff3",
                                "poor condition" = "#ff6600"), guide = "none") +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), linetype=3)+
  labs(x="Ecosystem Function", y="Percent Area of Region (+/- 95% CI)", colour="Function\nCondition")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face="bold", size=18),
        axis.text.x=element_text(face="bold", size=16, hjust = 1, angle = 45),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

bight.wide.fun.plots.panel.four

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/fun.scores.panel.fourcat.jpg", 
       bight.wide.fun.plots.panel.four,
       height = 8, width = 10,
       dpi = 300)

# area by function by stratum - four category
bight.wide.fun.plots.bystrata.four <-wt.fun.cond.2.bight.four %>% 
  filter(Category!="Total") %>% 
  mutate(stratum=factor(Subpopulation, levels = c("All Sites", "large_embayment", "small_embayment", "estuary"),  
                        labels = c("Southern California\nBight", "Large Embayments", "Small Embayments", "Estuaries")),
         overall.cond = case_when(
           Category == "1" ~ "poor condition",
           Category == "2" ~ "moderate condition",
           Category == "3" ~ "good condition",
           Category == "4" ~ "excellent condition"),
         overall.cond=factor(overall.cond, levels=c("poor condition", "moderate condition", "good condition", "excellent condition")),
         eco.fun=case_when(function_=="substrate.stabilization"~ "Substrate\nStabilization",
                           function_=="carbon.sequestration"~ "Carbon\nSequestration",
                           function_=="primary.production"~ "Primary\nProduction",
                           function_=="secondary.production"~ "Secondary\nProduction",
                           function_=="wq"~ "Water\nQuality",
                           function_=="nekton.habitat"~ "Nekton\nHabitat",
                           function_=="waterfowl.habitat"~ "Waterfowl\nHabitat")) %>% 
  naniar::replace_with_na(., replace=list(Estimate.P=0)) %>% 
  ggplot(., aes(x = eco.fun, y = Estimate.P)) +
  facet_wrap(~stratum, nrow = 3, ncol = 1) +
  geom_errorbar(aes(ymin = LCB95Pct.P, ymax = UCB95Pct.P, color = overall.cond),
                width = 0.15, linewidth = 0.8, position = position_dodge_val) +
  geom_point(aes(fill = overall.cond),
             shape = 21, size = 6, stroke = 1.2, color = "black", position = position_dodge_val) +
  scale_fill_manual(values = c("poor condition" = "#ff6600", 
                               "moderate condition" = "peachpuff3",
                               "good condition" = "cadetblue",
                               "excellent condition" = "turquoise1"), 
                    labels = c("Lowest Observed", "Moderate", "Good", "Best Observed"),
                    name = "Overall\nCondition") +
  scale_color_manual(values = c("poor condition" = "#ff6600", 
                                "moderate condition" = "peachpuff3",
                                "good condition" = "cadetblue",
                                "excellent condition" = "turquoise1"), guide = "none") +
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), linetype=3)+
  labs(x="Ecosystem Function", y="Percent of Area Bight-Wide (+/- 95% CI)", colour="Function\nCondition")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face="bold", size=18),
        axis.text.x=element_text(face="bold", size=16, hjust = 1, angle = 45),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        strip.text = element_text(size = 16))

bight.wide.fun.plots.bystrata.four

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/fun.scores.bystrata.fourcat.jpg", 
       bight.wide.fun.plots.bystrata.four,
       height = 12, width = 12,
       dpi = 400)

# area by function - panel by strata - three category
bight.wide.fun.plots.bystratum <-wt.fun.cond.bight %>% 
  filter(Category!="Total") %>% 
  mutate(stratum=factor(Subpopulation, levels = c("All Sites", "large_embayment", "small_embayment", "estuary"), 
          labels = c("Southern California\nBight", "Large Embayments", "Small Embayments", "Estuaries")),
    overall.cond=factor(Category, levels=c("good condition", "moderate condition", "poor condition")),
    eco.fun=case_when(function_=="substrate.stabilization"~ "Substrate\nStabilization",
                      function_=="carbon.sequestration"~ "Carbon\nSequestration",
                      function_=="primary.production"~ "Primary\nProduction",
                      function_=="secondary.production"~ "Secondary\nProduction",
                      function_=="wq"~ "Water\nQuality",
                      function_=="nekton.habitat"~ "Nekton\nHabitat",
                      function_=="waterfowl.habitat"~ "Waterfowl\nHabitat")) %>% 
  naniar::replace_with_na(., replace=list(Estimate.P=0)) %>% 
  ggplot(., aes(x=eco.fun, y=Estimate.P, colour = overall.cond, ymin = LCB95Pct.P, ymax=UCB95Pct.P))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title = element_text(face="bold", size=13),
        axis.text.x=element_text(face="bold", size=10, hjust = 1, angle = 45))+
  geom_pointrange(size=0.75, position = position_dodge(width=0.5))+
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), linetype=3)+
  labs(x="Ecosystem Function", y="Percent of Area Bight-Wide (+/- 95% CI)", colour="Function\nCondition")+
  scale_color_manual(values=c("plum1", "#666699", "#CC6600"),
                     labels=c("Good", "Moderate", "Poor"))+
  NULL

bight.wide.fun.plots.bystratum

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/Overall.function.scores-area.weighted.jpg", 
       bight.wide.fun.plots,
       height = 10, width = 10,
       dpi = 300)

## more plots - not area-weighted but location based
cols_condition <- c("plum1" = "good condition", "#666699" = "moderate condition", "#CC6600" = "poor condition")
cols_condition <- c("good condition" = "plum1", "moderate condition" = "#666699", "poor condition"= "#CC6600")

#bight weighted index scores
wt.index.score.b <- wt.index.scores %>%
  filter(project_id == "B23-SAV")

#overall condition vs. distance to inlet
#ecosystem function vs site protection
siteprotection.plot1 <- wt.index.scores.4 %>%
  ggplot(aes(x =site_protection_granular, y = fun_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "darkgreen", alpha = 0.5, na.rm = TRUE) + 
  facet_wrap(~ function_) +
  theme_bw() +
  ylab("Ecosystem Function Score") + xlab("Site Protection Status")
siteprotection.plot1

#run anovas to see if site protection affects any score
# Create a vector of the unique functions
functions <- unique(wt.index.scores.4$function_)

# Prepare a list to store results
anova_results <- list()

# Loop through each function
for (fn in functions) {
  df <- wt.index.scores.4 %>% filter(function_ == fn)
  
  # Check that there are at least 2 groups for ANOVA
  if(length(unique(df$site_protection_granualr)) > 1) {
    fit <- aov(fun_score ~ site_protection_granular, data = df)
    anova_results[[fn]] <- summary(fit)
  } else {
    anova_results[[fn]] <- "Not enough groups"
  }
}

# View results
anova_results

library(ggsignif)

#let's try this another way with wilcox test pairwise comparisons
sig_df <- wt.index.scores.4 %>%
  group_by(function_) %>%
  summarise(
    pvals = list(
      list(
        "U vs NWR" = wilcox.test(fun_score[site_protection_granular=="U"],
                                 fun_score[site_protection_granular=="NWR"])$p.value,
        "U vs No Take" = wilcox.test(fun_score[site_protection_granular=="U"],
                                     fun_score[site_protection_granular=="No Take"])$p.value,
        "U vs Limited Take" = wilcox.test(fun_score[site_protection_granular=="U"],
                                          fun_score[site_protection_granular=="Limited Take"])$p.value,
        "NWR vs No Take" = wilcox.test(fun_score[site_protection_granular=="NWR"],
                                       fun_score[site_protection_granular=="No Take"])$p.value,
        "NWR vs Limited Take" = wilcox.test(fun_score[site_protection_granular=="NWR"],
                                            fun_score[site_protection_granular=="Limited Take"])$p.value,
        "No Take vs Limited Take" = wilcox.test(fun_score[site_protection_granular=="No Take"],
                                                fun_score[site_protection_granular=="Limited Take"])$p.value
      )
    )
  )



siteprotection.plot1 <- ggplot(data = wt.index.scores.4, aes(x = site_protection_granular, y = fun_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "darkgreen", alpha = 0.5, na.rm = TRUE) +
  facet_wrap(~ function_) + ylim(c(1,6)) +
  theme_bw() +
  ylab("Ecosystem Function Score") + 
  xlab("Site Protection Status") +
  geom_signif(
    comparisons = list(
      c("U", "NWR"),
      c("U", "No Take"),
      c("U", "Limited Take"),
      c("NWR", "No Take"),
      c("NWR", "Limited Take"),
      c("No Take", "Limited Take")),  # groups to compare
    test = "wilcox.test",                  # or "wilcox.test"
    map_signif_level = function(p) ifelse(p < 0.05, "*", NA),          # <--- converts p-values to *, **, ***
    tip_length = 0.03,                # optional
   step_increase = 0.2, na.rm = TRUE) +   # <--- move + outside the closing parenthesis
  theme(axis.text.x = element_text(size = 16, hjust = 1, angle = 45)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18))

siteprotection.plot1

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/function_vs_siteprotection.jpg", 
       siteprotection.plot1,
       height = 12, width = 12,
       dpi = 300)

distinct.wt.scores <- wt.index.scores.4 %>%
  group_by(masterid) %>%
  summarize(overall_score = mean(overall_score, na.rm = T),
            site_protection_granular = first(site_protection_granular),
            waterbody = first(waterbody),
            stratum = first(stratum),
            .group = "drop")

# site protection vs overall condition - GRANULAR
siteprotection.overallcondition.gran <- ggplot(data = distinct.wt.scores.trim, aes(x = site_protection_granular.x, y = overall_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "darkgreen", alpha = 0.5, na.rm = TRUE) +
  #facet_wrap(~stratum) +
  theme_bw() +
  ylab(" Overall Condition Score") + 
  xlab("Site Protection Status") + ylim(c(1,4.75)) +
  geom_signif(
    comparisons = list(
      c("U", "NWR"),
      c("U", "No Take"),
      c("U", "Limited Take"),
     c("NWR", "No Take"),
     c("NWR", "Limited Take"),
      c("No Take", "Limited Take")),  # groups to compare
   test = "wilcox.test",                  # or "wilcox.test"
   # map_signif_level = function(p) ifelse(p < 0.05, "*", ""),          # <--- converts p-values to *, **, ***
    tip_length = 0.03,                # optional
    step_increase = 0.2, na.rm = TRUE)+   # <--- move + outside the closing parenthesis
  theme(axis.text.x = element_text(size = 18,  hjust = 1, angle = 45)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18))

siteprotection.overallcondition.gran

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/overallcondition_vs_siteprotection.gran.jpg", 
       siteprotection.overallcondition.gran,
       height = 12, width = 12,
       dpi = 300)

#site protection facet by stratum - toggle between granular protection status/ U vs P
siteprotection.overallcondition.stratum.gran <- ggplot(data = distinct.wt.scores.trim, aes(x = site_protection_granular.x, y = overall_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "darkgreen", alpha = 0.5, na.rm = TRUE) +
  facet_wrap(~stratum.x) +
  theme_bw() +
  ylab(" Overall Condition Score") + 
  xlab("Site Protection Status") + ylim(c(1,4.75)) +
  geom_signif(
    comparisons = list(
      c("U", "NWR"),
      c("U", "No Take"),
      c("U", "Limited Take"),
      c("NWR", "No Take"),
      c("NWR", "Limited Take"),
      c("No Take", "Limited Take")),  # groups to compare
    test = "wilcox.test",                  # or "wilcox.test"
  #  map_signif_level = function(p) ifelse(p < 0.05, "*", ""),          # <--- converts p-values to *, **, ***
    tip_length = 0.03,                
    step_increase = 0.2, na.rm = TRUE)+   
  theme(axis.text.x = element_text(size = 18,  hjust = 1, angle = 45)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18))

siteprotection.overallcondition.stratum.gran


ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/overallcondition_vs_siteprotection.stratum.gran.jpg", 
       siteprotection.overallcondition.stratum.gran,
       height = 12, width = 12,
       dpi = 300)

#site protection facet by stratum - U vs P
siteprotection.overallcondition.stratum <- ggplot(data = distinct.wt.scores.trim, aes(x = site_protection, y = overall_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "darkgreen", alpha = 0.5, na.rm = TRUE) +
  facet_wrap(~stratum.x) +
  theme_bw() +
  ylab(" Overall Condition Score") + 
  xlab("Site Protection Status") + ylim(c(1,4.75)) +
 # geom_signif(
  #  comparisons = list(
   ##   c("U", "P")),  # groups to compare
  #  test = "wilcox.test",                  # or "wilcox.test"
  #  map_signif_level = function(p) ifelse(p < 0.05, "*", ""),          # <--- converts p-values to *, **, ***
  #  tip_length = 0.03,                
  #  step_increase = 0.2, na.rm = TRUE)+   
  theme(axis.text.x = element_text(size = 18,  hjust = 1, angle = 45)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18))

siteprotection.overallcondition.stratum


ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/overallcondition_vs_siteprotection.stratum.jpg", 
       siteprotection.overallcondition.stratum,
       height = 12, width = 12,
       dpi = 300)

siteprotection.function <- ggplot(data = scores.dti, aes(x = site_protection, y = fun_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "darkgreen", alpha = 0.5, na.rm = TRUE) +
  facet_wrap(~function_) +
  geom_signif(
    comparisons = list(
      c("U", "P")),
    test = "wilcox.test",                  # or "wilcox.test"
    map_signif_level = function(p) {
      if (p <0.001) "***"
      else if (p < 0.01) "**"
      else if (p < 0.05) "*"
      else NA},           # <--- converts p-values to *, **, ***
    tip_length = 0.03,                
    step_increase = 0.5, na.rm = TRUE) +
  theme_bw() +
  ylab(" Ecosystem Function Score") + 
  xlab("Site Protection Status") + ylim(c(1,4.75))  +
  theme(axis.text.x = element_text(size = 18,  hjust = 1, angle = 45)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18))

siteprotection.function


ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/siteprotection_function.jpg", 
       siteprotection.function,
       height = 10, width = 10,
       dpi = 300)

distinct.wt.scores.trim <- distinct.wt.scores.exp %>%
  filter(masterid != "RESCQ-Site-006")

siteprotection.overallcondition <- ggplot(data = distinct.wt.scores.trim, aes(x = site_protection, y = overall_score)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "darkgreen", alpha = 0.5, size = 4, na.rm = TRUE) +
 # facet_wrap(~stratum.x) +
  theme_bw() +
  ylab(" Overall Condition Score") + 
  xlab("Site Protection Status") + ylim(c(1,4.75)) +
   geom_signif(
    comparisons = list(
     c("U", "P")),  # groups to compare
    test = "wilcox.test",                  # or "wilcox.test"
    map_signif_level = function(p) {
      if (p <0.001) "***"
      else if (p < 0.01) "**"
      else if (p < 0.05) "*"
      else NA},          # <--- converts p-values to *, **, ***
    tip_length = 0.03,                
    step_increase = 0.75, na.rm = TRUE)+   
  theme(axis.text.x = element_text(size = 18,  hjust = 1, angle = 45)) +
  theme(strip.text = element_text(size = 18)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18))

siteprotection.overallcondition


ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/overallcondition_vs_siteprotection.jpg", 
       siteprotection.overallcondition,
       height = 10, width = 10,
       dpi = 300)

#overall index scores vs. chla
chla <-read.csv(here("data", "raw", "chla.csv"))
tss <-read.csv(here("data", "raw", "tss.csv")) %>% select(site_id, Final.TSS.Dry.Weight..mg.) %>%
  rename(TSS_mg = "Final.TSS.Dry.Weight..mg.")

# average percent macroalgae per site
head(percentmacroalgae_all)

avg_perc_macroalgae <- percentmacroalgae_all %>%
  select(site_id, transect, sample_id, variable_label, result) %>%
  group_by(site_id) %>%
  summarize(
    avg_macroalgae = mean(result), .groups = "drop")

distinct.wt.scores.exp <- distinct.wt.scores %>%
  left_join(chla, by = c("masterid" = "site_id"), relationship = "many-to-many") %>%
  left_join(tss, by = c("masterid" = "site_id"), relationship = "many-to-many") %>%
  left_join(station.info, by = c("masterid" = "site_id"), relationship = "many-to-many") %>%
  left_join(avg_perc_macroalgae, by = c("masterid" = "site_id"), relationship = "many-to-many")

chla_model <- lm(log10(chla_conc) ~ overall_score, data = distinct.wt.scores.exp)
summary(chla_model)

#distance to inlet models - overall + by group
dti_model <- lm(overall_score ~ distance_to_inlet, data = distinct.wt.scores.exp) 
summary(dti_model)
dti_model_est <- lm(overall_score ~ distance_to_inlet*stratum.x, data = distinct.wt.scores.exp)
summary(dti_model_est)


depth_model <- lm(overall_score ~ central_depth_mhw, data = distinct.wt.scores.exp)
summary(depth_model)

# percent macroalgae model
macroalgae_model <- lm(overall_score ~ avg_macroalgae, data = distinct.wt.scores.exp)
summary(macroalgae_model)

macroalgae_plot <- distinct.wt.scores.exp %>%
  filter(project_id == "B23-SAV") %>%
  ggplot() +
  geom_point(aes(x = avg_macroalgae, y = overall_score), size = 4, shape = 21, fill = "grey20", color = "black", alpha = 0.7) +
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  ylab("Overall Index Score") + xlab(expression(paste("Average percent macroalgae (%)"))) +
  geom_smooth(aes(x = avg_macroalgae, y = overall_score), method = "lm", se = TRUE) +
  stat_regline_equation(aes(x = avg_macroalgae, y = overall_score),
                        label.x.npc = "center",
                        label.y.npc = 0.95,
                        size = 6)+
  stat_cor(aes(x = avg_macroalgae, y = overall_score),
           method = "pearson",
           label.x.npc = "center",
           label.y.npc = 0.85, 
           size = 6)

macroalgae_plot

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/percmacroalgae_vs_overallscore.jpg", 
       macroalgae_plot,
       height = 8, width = 8,
       dpi = 300)

#data is right-skewed - log transform and model still not significant
chla_plot <- ggplot(data = distinct.wt.scores.exp, aes(x = log10(chla_conc), y =overall_score)) +
  geom_point(size = 4, shape = 21, fill = "grey20", color = "black", alpha = 0.7) +
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  ylab("Overall Index Score") + xlab(expression(paste("log10-Chlorophyll (", mu, "g/L)"))) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_regline_equation(label.x.npc = "left",
                        label.y.npc = 0.95,
                        size = 6)+
  stat_cor(method = "pearson",
           label.x.npc = "left",
           label.y.npc = 0.85, 
           size = 6)
chla_plot

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/log10_chla_vs_overallscore.jpg", 
       chla_plot,
       height = 10, width = 10,
       dpi = 300)

#try chla vs water quality function
wq.scores.4 <- wt.index.scores.4 %>%
  select(-site_id) %>%
  group_by(masterid, function_) %>%
  filter(function_ == "wq") %>%
  distinct() %>%
  left_join(chla, by = c("masterid" = "site_id"), relationship = "many-to-many") %>%
  left_join(tss, by = c("masterid" = "site_id"), relationship = "many-to-many")

#chla vs water quality function plot
tss_plot_wq <- ggplot(data = wq.scores.4, aes(x = TSS_mg, y =fun_score)) +
  geom_point(size = 4, shape = 21, fill = "grey20", color = "black", alpha = 0.7) +
  theme_bw() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  ylab("Water Quality Function Score") + xlab("TSS (mg)") +
  geom_smooth(method = "lm", se = TRUE) +
  stat_regline_equation(label.x.npc = "left",
                        label.y.npc = 0.95,
                        size = 6)+
  stat_cor(method = "pearson",
           label.x.npc = "left",
           label.y.npc = 0.85,
           size = 6)
tss_plot_wq

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/tss_vs_wq_score.jpg", 
       tss_plot_wq,
       height = 10, width = 10,
       dpi = 300)


#overall index scores vs. tss
  
#overall index scores vs. distance to inlet
scores.dti <- wt.index.scores.4 %>%
  select(-site_id) %>%
  group_by(masterid) %>%
  distinct()

overall.dti.plot <- ggplot(scores.dti, aes(y = overall_score, x = distance_to_inlet, color = stratum)) +
  geom_point(aes(color =stratum), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, fill = "lightblue")+
  scale_color_manual(values = col_strata) +
  theme_bw() +
  theme(axis.title = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16, hjust = 1, angle = 45)) +
  xlab("Distance to inlet (km)") + ylab("Overall Index Score")

overall.dti.plot
library(cowplot)
dti.together.plot <- ggplot(scores.dti, aes(y = overall_score, x = distance_to_inlet)) +
  geom_point(color = "grey20", alpha = 0.4, size = 3) +
  geom_smooth(method = "lm", se = TRUE, fill = "darkblue", alpha = 0.3)+
  theme_bw() +
  theme(axis.title = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16, hjust = 1, angle = 45)) +
  xlab("Distance to inlet (km)") + ylab("Overall Index Score")
dti.together.plot

dti.cowplot <- plot_grid(dti.together.plot, overall.dti.plot, ncol = 1)
dti.cowplot

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/distance_to_inlet_v_overall.jpg", 
       overall.dti.plot,
       height = 6, width = 8,
       dpi = 300)
ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/distance_to_inlet_v_cowplot.jpg", 
       dti.cowplot,
       height = 6, width = 8,
       dpi = 300)

overall.macroalgae.bystratum <- ggplot(scores.dti, aes(y = overall_score, x = distance_to_inlet, color = stratum)) +
  geom_point(aes(color =stratum), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, fill = "lightblue")+
  scale_color_manual(values = col_strata) +
  theme_bw() +
  theme(axis.title = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16, hjust = 1, angle = 45)) +
  xlab("Distance to inlet (km)") + ylab("Overall Index Score")


#water quality function vs. distance to inlet
scores.wq <- wt.index.scores %>%
  select(-site_id) %>%
  group_by(masterid, function_) %>%
  filter(function_ == "wq") %>%
  distinct()

#WQ vs distance plot - by category
wq.plot1 <- ggplot(scores.wq, aes(x = fun_cond, y = distance_to_inlet)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = fun_cond), width = 0.2, alpha = 0.6, size = 3) +
  scale_color_manual(values = cols_condition) +
  theme_bw() +
  theme(axis.title = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16, hjust = 1, angle = 45)) +
  ylab("Distance to inlet (km)") + xlab("Water Quality Function Condition")
  
wq.plot1

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/distance_to_inlet_v_WQ_cond.jpg", 
       wq.plot1,
       height = 10, width = 10,
       dpi = 300)

# WQ plot vs distance - by function score
wq.plot2 <- ggplot(scores.wq, aes(x = fun_score, y = distance_to_inlet)) +
  geom_jitter(aes(color = fun_cond), width = 0.2, alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE)+
  scale_color_manual(values = cols_condition) +
  theme_bw() +
  theme(axis.title = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16, hjust = 1, angle = 45)) +
  ylab("Distance to inlet (km)") + xlab("Water Quality Function Score")
wq.plot2

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/distance_to_inlet_v_WQ_score.jpg", 
       wq.plot2,
       height = 10, width = 10,
       dpi = 300)

col_strata <- c(estuary = "#330066", small_embayment = "#cc0066",
                large_embayment ="#ffcccc", NA = "cornsilk3")

wq.plot3 <- ggplot(scores.wq, aes(x = fun_score, y = distance_to_inlet, color = stratum)) +
  geom_jitter(aes(color = stratum), width = 0.2, alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, fill = "lightblue")+
  scale_color_manual(values = col_strata) +
  theme_bw() +
  theme(axis.title = element_text(size = 18))+
  theme(axis.text.x = element_text(size = 16, hjust = 1, angle = 45)) +
  ylab("Distance to inlet (km)") + xlab("Water Quality Function Score")
wq.plot3

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/distance_to_inlet_v_WQ_stratum.jpg", 
       wq.plot3,
       height = 10, width = 10,
       dpi = 300)
#MAPS
library(ggmap)
register_google(key= "AIzaSyDnD2qRjJLIPHaB8QNg5EtLsv-rv3PxGCc")
#center map around data
center_sav <-c(lon =-117.6, lat = 33.3)
north_sav <-c(lon = -118.1, lat = 33.75)
mid_sav <-c(lon = -117.5, lat = 33.25)
south_sav <-c(lon = -117.25, lat = 32.75)

# base maps
basemap9 <-get_map(location = center_sav, zoom = 9, smaptype = "satellite")
sandiegomap <- get_map(location = south_sav, zoom = 11, maptype = "terrain")

#water quality function
fun_scores_wq <- wt.index.scores %>%
  distinct() %>%
  filter(project_id =="B23-SAV") %>%
  group_by(masterid, function_) %>%
  filter(function_ == "wq")

## water quality function map
wq_condition_map_sd <- ggmap(sandiegomap) +
  geom_point(data = fun_scores_wq, aes(x = nominal_lon, y = nominal_lat, fill = fun_cond), 
             color = "black", pch = 24, size = 4, alpha = 0.75)+
  theme_void() +
  scale_fill_manual(values = cols_condition) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  guides(fill = guide_legend(title = "Water Quality Eelgrass Condition Score", position = "top")) 

wq_condition_map_sd

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/final/wq.condition.map.sd.jpg", 
       wq_condition_map_sd,
       height = 12, width = 12,
       dpi = 400)


### overall condition map
ggsave("BIGHT-overall-condition-map.tiff",
       dpi = 300, 
       width = 10,
       height = 10,
       overall_condition_map)

#nmds by waterbody
head(wt.index.scores.4)

#prepare function data - remove all rows with NA or NaN
eelgrass.nmds.data <- wt.index.scores.4 %>%
  filter(project_id == "B23-SAV") %>%
  select(masterid, waterbody, function_, fun_score) %>%
  distinct() %>%
  pivot_wider(names_from = function_, values_from = fun_score) %>%
  filter(if_all(where(is.numeric), ~ !is.na(.) & !is.nan(.)))

#calculate distance matrix and run nmds
dist.matrix <-vegdist(eelgrass.nmds.data[,-c(1,2)], method = "euclidean")
function.nmds <- metaMDS(dist.matrix, k=2)

#extract scores
nmds.scores <- as.data.frame(scores(function.nmds))
nmds.scores <-nmds.scores %>%
  bind_cols(eelgrass.nmds.data %>% select(masterid, waterbody))

nmds.scores$selection <- c(rep("probabilistic", 47),
                           rep("targeted", 11))

#calculate centroids
centroids <- nmds.scores %>%
  group_by(waterbody) %>%
  summarize(NMDS1 = mean(NMDS1),
            NMDS2 = mean(NMDS2))

#calculate vectors
function.envfit <- envfit(function.nmds, eelgrass.nmds.data[,-c(1,2)], permutations = 999)

#extract arrow coordinates
vectors <- as.data.frame(scores(function.envfit, display = "vectors"))
vectors$functions <-rownames(vectors)

#keep only significant - all of them
vectors$pval <-function.envfit$vectors$pvals
vectors.sig <-vectors %>%
  filter(pval <= 0.05)

#adjust for visibility
arrow.mult <- 1.5

vectors.sig <- vectors.sig %>%
  mutate(NMDS1 = NMDS1 * arrow.mult,
         NMDS2 = NMDS2 * arrow.mult)


#build nmds plot - based on ecosystem function score
function.score.nmds <- ggplot(data = nmds.scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(fill = waterbody), color = "grey20", alpha = 0.9, size = 5, shape = 21) +
  stat_ellipse(aes(group = waterbody, color = waterbody),
               type = "t",
               linewidth = 0.8, linetype = "dashed") +
  geom_segment(data = vectors.sig,
               aes(x = 0, y = 0,
                   xend = NMDS1,
                   yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black",
               linewidth = 0.8) +
  geom_text(data = vectors.sig,
            aes(x = NMDS1,
                y = NMDS2,
                label = functions),
            size = 5, position = "nudge",
            fontface = "bold") +
  #geom_point(data = centroids,
            # aes(x = NMDS1, y = NMDS2, fill = waterbody),
            # size = 6, color = "black", shape = 24, stroke = 1.2, show.legend = F) +
  theme_bw() +
 theme(panel.grid = element_blank(),
axis.title = element_text(size = 16, face = "bold"),
axis.text = element_text(size = 14),
legend.key.size = unit(0.4, "cm"),
legend.title = element_text(size = 18),
legend.text = element_text(size = 12)) +
  labs(subtitle = paste("Stress =", round(function.nmds$stress, 3))) +
  guides(fill = guide_legend(ncol = 1))

function.score.nmds



ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/ecosystem.function.nmds.jpg", 
       function.score.nmds,
       height = 12, width = 16,
       dpi = 400)

#highlight targeted sites
function.score.nmds.target <- ggplot(data = nmds.scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(fill = selection), color = "grey20", alpha = 0.9, size = 5, shape = 21) +
  scale_fill_manual(values = c("grey50", "turquoise")) +
  stat_ellipse(aes(group = waterbody, color = waterbody),
               type = "t",
               linewidth = 0.8, linetype = "dashed", show.legend = FALSE) +
  geom_segment(data = vectors.sig,
               aes(x = 0, y = 0,
                   xend = NMDS1,
                   yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black",
               linewidth = 0.8) +
  geom_text(data = vectors.sig,
            aes(x = NMDS1,
                y = NMDS2,
                label = functions),
            size = 5, position = "nudge",
            fontface = "bold") +
  #geom_point(data = centroids,
  # aes(x = NMDS1, y = NMDS2, fill = waterbody),
  # size = 6, color = "black", shape = 24, stroke = 1.2, show.legend = F) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14)) +
  labs(subtitle = paste("Stress =", round(function.nmds$stress, 3)))

function.score.nmds.target

nmds.cowplot <-plot_grid(function.score.nmds, function.score.nmds.target, ncol = 1)


ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/nmds.cowplot.jpg", 
       nmds.cowplot,
       height = 14, width = 15,
       dpi = 400)

# nmds plot with centroids and ellipses
function.score.nmds.centroid <- ggplot(data = nmds.scores, aes(x = NMDS1, y = NMDS2)) +
 # geom_point(color = "grey20", alpha = 0.8, size = 3, shape = 21) +
  stat_ellipse(aes(group = waterbody, color = waterbody),
               type = "t",
               linewidth = 0.8, linetype = "dashed", show.legend = FALSE) +
  geom_point(data = centroids,
             aes(x = NMDS1, y = NMDS2, fill = waterbody),
             size = 5, alpha = 0.8, color = "black", shape = 24, stroke = 1.2, show.legend = F) +
  ggrepel::geom_text_repel(data = centroids, aes(x = NMDS1, y = NMDS2, label = waterbody),
            size = 5, box.padding = 0.5, point.padding = 0.5, segment.color = "grey50") +
  theme_bw() +
  labs(subtitle = paste("Stress =", round(function.nmds$stress, 3)), size = 6) 
 
function.score.nmds.centroid

ggsave("/Users/jilltupitza/Library/CloudStorage/OneDrive-SCCWRP/eelgrass_index/figures/exploratory/ecosystem.function.nmds.centroid.jpg", 
       function.score.nmds.centroid,
       height = 12, width = 16,
       dpi = 400)
CR <-c("#d12028", "#d8006f", "deeppink", "pink", "#dce2f0", "lightyellow", "lightyellow3", "#33CCFF", "#18a7b7")

# make metadata of site selection
site.selection.pts <- nmds.scores %>%
  select(masterid, waterbody, selection) %>%
  left_join(station.info %>% select(site_id, nominal_lat, nominal_lon, stratum), by = c("masterid" = "site_id"),
            relationship = "many-to-many") 

write.csv(site.selection.pts, "site.selection.pts.csv")  
