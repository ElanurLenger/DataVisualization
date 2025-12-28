# DataVisualization
  Earthquakes in Turkey  (1915-2021)
Dataset: https://www.kaggle.com/code/atasaygin/turkey-earthquake-analysis-1915-2020


```{r}
library(ggplot2)
library(readr)
```
Annual Number of Earthquakes (1915-2021) - Critical Years
```{r}

deprem_data$Yil <- substr(deprem_data$`Olus tarihi`, 1, 4)
yillik_ozet <- as.data.frame(table(deprem_data$Yil))
colnames(yillik_ozet) <- c("Yil", "DepremSayisi")
yillik_ozet$Yil <- as.numeric(as.character(yillik_ozet$Yil))

ggplot(yillik_ozet, aes(x = Yil, y = DepremSayisi)) +
  geom_line(color = "darkblue", linewidth = 1.2) + 
  geom_point(color = "red", size = 2) +
  geom_text(aes(label = ifelse(DepremSayisi > 500, as.character(DepremSayisi), "")),
            vjust = -1.2,
            size = 5,
            fontface = "bold",
            check_overlap = TRUE) +
  scale_x_continuous(limits = c(1915, 2025), breaks = seq(1915, 2025, by = 10)) +
  scale_y_continuous(limits = c(0, 1150)) +
  labs(title = "Annual Number of Earthquakes (1915-2021) - Critical Years",
       x = NULL, 
       y = NULL) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", size = 13),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(20, 40, 20, 20) 
  )
```
<img width="3600" height="1800" alt="yillik_analiz" src="https://github.com/user-attachments/assets/572c16b3-a20b-47f9-bea1-31fd1f569b41" />



Relationship Between Earthquake Depth and Magnitude
```{r}


ggplot(deprem_data, aes(x = Derinlik, y = xM)) +
  geom_point(alpha = 0.3, color = "darkblue", size = 2) +
  theme_minimal() +
  labs(title = "Relationship Between Earthquake Depth and Magnitude",
       x = "Depth (km)",
       y = "Magnitude (Mw)") +
  scale_y_continuous(
    limits = c(0, 8.5), 
    breaks = 0:8,
    expand = c(0, 0)
  ) +
  coord_fixed(ratio = 20) + 
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.5, color = "grey90")
  )
```
<img width="3000" height="2100" alt="derinlik_siddet" src="https://github.com/user-attachments/assets/a31f6622-2820-4e70-9403-f3b304930c16" />





Earthquake Density Map of Türkiye
```{r}


library(ggplot2)
library(sf)
library(rnaturalearth)
turkiye_sinir <- ne_countries(scale = "medium", country = "Turkey", returnclass = "sf")
deprem_sf <- st_as_sf(deprem_data, coords = c("Boylam", "Enlem"), crs = 4326)
deprem_turkiye_ici <- st_intersection(deprem_sf, turkiye_sinir)
ggplot() +
  geom_sf(data = turkiye_sinir, fill = "white", color = "black", linewidth = 0.7) +
  geom_sf(data = deprem_turkiye_ici, color = "darkred", alpha = 0.05, size = 0.8) +
  coord_sf(xlim = c(25.5, 45), ylim = c(35.5, 42.5)) +
  theme_minimal() +
  labs(title = "Earthquake Density Map of Türkiye",
       subtitle = "Strictly filtered within national terrestrial borders",
       x = NULL, y = NULL) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),

    axis.text = element_blank(),    
    axis.ticks = element_blank(),    
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # -----------------------------------------------------
    
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
```


<img width="3600" height="2400" alt="harita" src="https://github.com/user-attachments/assets/9c856164-7ce7-411a-9dc5-167fafadff27" />


Earthquake Magnitude Analysis by Regions
```{r}

ggplot(bolge_analiz, aes(x = reorder(Bolge, Max_Siddet), y = Max_Siddet)) +
  geom_segment(aes(x = reorder(Bolge, Max_Siddet), xend = Bolge, y = 0, yend = Max_Siddet), 
               color = "gray80", linewidth = 1.2) +
  geom_point(aes(color = Ort_Siddet, size = Max_Siddet)) +
  scale_color_gradient(low = "yellow", high = "red", name = "Avg. Mag.") +
  scale_size_continuous(range = c(5, 12), name = "Max Mag.") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Earthquake Magnitude Analysis by Regions",
    subtitle = "Ranked by historical maximum magnitude (Mw)",
    x = NULL, 
    y = "Maximum Magnitude (Mw)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(face = "bold", size = 12, margin = margin(b = 20)),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.title.x = element_text(face = "bold", size = 13, margin = margin(t = 10)),

    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9, face = "bold"),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "right"
  )

```

<img width="3000" height="2400" alt="bolge_analizi_" src="https://github.com/user-attachments/assets/160dba85-cf48-4ce4-ae00-208bfd488aa0" />


Top 10 High-Risk Turkish Cities by Earthquake Frequency
```{r}


library(dplyr)
library(ggplot2)

ggplot(en_cok_deprem_10_il, aes(x = reorder(Sehir, -count), y = count, fill = count)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = count), vjust = -0.3, size = 5.5, fontface = "bold") +
  scale_fill_gradient(low = "#FFCC33", high = "#E23E57") +
  ylim(0, 1200) + 
  theme_minimal() +
  labs(title = "Top 10 High-Risk Turkish Cities by Earthquake Frequency",
       subtitle = "Confirmed urban areas based on historical seismic data",
       x = NULL, 
       y = "Total Earthquakes") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, face = "bold", size = 14),
    axis.title.x = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5),
    panel.grid.minor = element_blank()
  )
```
<img width="3000" height="2100" alt="grafigim" src="https://github.com/user-attachments/assets/1b933767-b089-4ced-bffa-3bb41ef506c6" />



