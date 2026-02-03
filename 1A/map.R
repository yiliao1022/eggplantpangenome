library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

mydata <- read.table("./input/map.txt",header = TRUE, sep = "\t")

land <- ne_download(
  scale = 50,
  type = "land",
  category = "physical",
  returnclass = "sf"
)

lat_lines <- c(80, 60, 40, 20, 0, -20, -40, -60)
lon_lines <- c(-160, -120, -80, -40, 0, 40, 80, 120, 160)

mp <- ggplot() +
  geom_sf(data = land,
          fill = "#F2E6C9",
          color = NA) + 
  geom_hline(yintercept = lat_lines,
             linetype = "dashed",
             color = "grey",
             size = 0.25) +
  geom_vline(xintercept = lon_lines,
             linetype = "dashed",
             color = "grey",
             size = 0.25) +
  geom_point(
    data = mydata,
    aes(x = x, y = y,
        color = population1,
        shape = population1),
    size = 1.5
  ) +
  scale_color_manual(
    breaks = c("out", "C1", "C2", "C3", "C4"),
    values = c("black", "#EF6F6F", "#495D90","#37A95A", "#B083B5")
  ) +
  scale_shape_manual(
    breaks = c("out", "C1", "C2", "C3", "C4"),
    values = c(16, 16, 15, 18, 17)
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 90),
    expand = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "#B5E1FF", colour = "black"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = c(0.13, 0.10)
  )

mp
ggsave("map.pdf",mp,width = 10,height = 6,dpi = 300)
