library(tidyverse)
library(sf)
library(patchwork)
library(vegan)
library(ggfortify)
library(plotly)

cpad = st_read(here::here("data", "processed_data", "cpad_human.shp"))%>%
  mutate(log_size = log10(shape_area)) %>%
  mutate(size_bin = case_when(
    log_size <= 4 ~ 1,
    log_size > 4 & log_size < 5 ~ 2,
    log_size >= 5 ~ 3
  )) %>% 
  filter(sg_year == 2019) %>%
  mutate(size_bin = as.factor(size_bin))

pca_prep_data = cpad %>% 
  st_drop_geometry() %>% 
  select(pa_id, access, shape_area, size_bin, layer, gHM_mean, HFP_mean, strava, sg_count) %>% 
  drop_na() %>% 
  filter(!is.infinite(strava))

pca_data = pca_prep_data %>% 
  select(-pa_id, -access, -layer, -size_bin) 

pca_data2 = as.data.frame(scale(pca_data, scale = apply(pca_data, 2, sd, na.rm = TRUE)))

pca_corr = cor(pca_data)


# PCA ---------------------------------------------------------------------

pca = prcomp(pca_data2)
summary(pca)

autoplot(pca, data = pca_prep_data) 

explained_variance_ratio <- summary(pca)[["importance"]]['Proportion of Variance',]
explained_variance_ratio <- 100 * explained_variance_ratio
components <- pca[["x"]]
components <- data.frame(components)
components <- cbind(components, pca_prep_data$pa_id)
components <- cbind(components, pca_prep_data$access)%>% 
  as.data.frame()
components$PC3 <- -components$PC3
components$PC2 <- -(components$PC2)
components$PC4 <- -components$PC4
components$PC5 <- -(components$PC5)

ggplot(data = components) +
  geom_point(aes(PC1, PC2, color = pca_prep_data$access))+
  xlim(0, 5) +
  ylim(-0.5,0.1)
                    
axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4,
            titlefont=list(size=13))

### NEED TO EDIT
fig <- components %>%
  plot_ly() %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label=paste('PC 1 (',toString(round(explained_variance_ratio[1],1)),'%)',sep = ''), values=~PC1),
      list(label=paste('PC 2 (',toString(round(explained_variance_ratio[2],1)),'%)',sep = ''), values=~PC2),
      list(label=paste('PC 3 (',toString(round(explained_variance_ratio[3],1)),'%)',sep = ''), values=~PC3),
      list(label=paste('PC 4 (',toString(round(explained_variance_ratio[4],1)),'%)',sep = ''), values=~PC4),
      list(label=paste('PC 5 (',toString(round(explained_variance_ratio[5],1)),'%)',sep = ''), values=~PC5)
    ),
    color = ~pca_prep_data$access
  ) %>%
  style(diagonal = list(visible = FALSE)) %>%
  layout(
    legend=list(title=list(text='color')),
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis,
    xaxis3=axis,
    xaxis4=axis,
    xaxis5=axis,
    xaxis6=axis,
    yaxis2=axis,
    yaxis3=axis,
    yaxis4=axis,
    yaxis5=axis,
    yaxis6=axis
  )

fig


fig2 <- plot_ly(components, x = ~PC1, y = ~PC2, z = ~PC3, color = ~pca_prep_data$access) %>%
  add_markers(size = 12)


fig2 <- fig2 %>%
  layout(
    scene = list(bgcolor = "#e5ecf6")
  )

fig2


# NMDS --------------------------------------------------------------------

set.seed(42)
nmds <- metaMDS(pca_data, distance = "euclidian", na.rm=TRUE)
nmds

filled_scores <- as.data.frame(scores(nmds)$sites)

filled_scores$access_type <- pca_prep_data$access
filled_scores$suid_nma <- pca_prep_data$pa_id
filled_scores$layer <- pca_prep_data$layer
filled_scores$size_bin <- pca_prep_data$size_bin

filled_scores = filled_scores %>% 
  mutate(access_type = as.factor(access_type),
         suid_nma = as.factor(suid_nma),
         layer = as.factor(layer),
         size_bin = as.factor(size_bin))

species_scores <- as.data.frame(nmds[["species"]])

rownames(species_scores) = c("Shape Area", "Global Human Modification",
                             "Human Footprint Index", "Strava", "Safegraph",
                             "Access Type")

species_scores <- species_scores %>% 
  rownames_to_column(var = "var")

nmds_plot <- ggplot(filled_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  #stat_ellipse(geom = "polygon", aes(color = _________, fill = ______), linewidth = 0.25, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(color = access_type), alpha = 0.3)+
  theme_bw(base_size = 16) +
  labs(x = "nMDS1", y = "nMDS2", color = "Access") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  geom_segment(data = species_scores, aes(x = 0, xend = MDS1, y = 0, yend = MDS2), arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.5, colour = "grey20") +
  ggrepel::geom_label_repel(data = species_scores, aes(x = MDS1, y = MDS2, label = var), fill = "white", cex = 3, direction = "both", segment.size = .25) +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("text", x = 0.17, y = 0.25, label = "Stress: ____")

nmds_plot
