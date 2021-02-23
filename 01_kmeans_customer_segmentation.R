# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----


library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 CUSTOMER TRENDS ----
# - GOAL: Mine Customer Purchase History for similarity to other "like" customers
# - TECHNIQUES: K-Means Clustering, UMAP 2D Projection

# 1.1 Get Customer Trends ----

# We convert to customer trends by : 1) Aggregation within customer-product groups, then ----
# 2) normalizing within customer groups to get percentages of product purchases by customer ----
 customer_trends_tbl <- bike_orderlines_tbl %>% 
    
    select(bikeshop_name, price, model, category_1, category_2, frame_material, quantity) %>%
    
    # Summarization and group by 
    group_by(bikeshop_name, price, model, category_1, category_2, frame_material) %>%
    
    #   _Step 1 => Aggregate to customer-product groups 
    summarise(quantity_purchased = sum(quantity)) %>% ungroup() %>%
    
    #   _step 2 => normalizing within customer groups to get percentages of product purchases by customer
    #   without creating a new data_frame group_by() + mutate () 
    group_by(bikeshop_name) %>%
    mutate(prop_of_total = quantity_purchased / sum(quantity_purchased)) %>%
    ungroup()

# 1.2 Convert to User-Item Format (e.g. Customer-Product) ----

customer_products_tbl <- customer_trends_tbl %>% select(bikeshop_name, model, prop_of_total) %>%
    spread (key = model, value = prop_of_total, fill = 0)

# 2.0 MODELING: K-MEANS CLUSTERING ----

# 2.1 Performing K-Means ----
#   to perform K-means, every columns involved should be numerical (remove the customers columns) ----

kmeans_obj <- customer_products_tbl %>% 
    select(-bikeshop_name) %>% 
    kmeans(centers = 5, nstart = 100)

#   __kmeans_obj$centers => centers output : matrix of cluster location in k * m_items space (5*97)
#   __kmeans_obj$cluster => the group that each product is attached to one product 

kmeans_obj$centers 


# 2.2 Tidying a K-Means Object ----
broom::tidy(kmeans_obj) %>% glimpse()

#   __broom::glance() => get the metrics from the kmeans algo (output = tibble) ----
broom::glance(kmeans_obj)

#   ..............................Important........................... ----
#   __broom::augment(kmeans_obj, tibble_data that was used to produce the kmeans_obj) ----
#   __select(bikeshop_name, .cluster(factor)) (object created with the augment function))

broom::augment(kmeans_obj, customer_products_tbl) %>%
    select(bikeshop_name, .cluster)

# 2.3 How many centers (customer groups) to use? ----

# STEP1 => test the function for a given number of center ----
center <- 3

kmeans_mapper <- function(centers = 3) {
    
    customer_products_tbl %>%
        select(-bikeshop_name) %>%
        kmeans(centers = centers, nstart = 100)
}

3 %>% kmeans_mapper() %>% glance()

# STEP2 => map the function to many elements (row-wise = mutate() + map() )----

kmeans_mapped_tbl <- tibble(centers = 1:15) %>% 
    mutate(k_means = map(centers, kmeans_mapper))  %>%
    mutate(glance = map(k_means, glance))
    
 
# STEP3 => Unnest the glance column to liberate the nested metrics contains in glance ----
kmeans_mapped_tbl %>% unnest (glance) %>%
    select(centers, tot.withinss)

# 2.4 Skree Plot ----

kmeans_mapped_tbl %>% unnest (glance) %>%
    select(centers, tot.withinss) %>%
    
    ggplot(aes(centers, tot.withinss)) + 
    geom_point(color = "#2c3e50", size = 3) +
    geom_line(color = "#2c3e50", size = 1) + 
    ggrepel::geom_label_repel(aes(label = centers), color = "#2c3e50") + 
    
    # Formatting 
    theme_tq() + 
    labs(
        title = "Skree Plot of the Bike shop customers", 
        subtitle = "Measures the distance each of the customer are from the closes K-Means center \nAuthor : Ralph D. Tasing", 
        caption = "Conclusion: Based on the skree plot, I select 4 clusters to segment the customer") +
   
    theme(legend.position = "none",
    plot.title = element_text(face = "bold", size = 18), 
    plot.caption = element_text(face = "bold.italic", size = 10))

# 3.0 VISUALIZATION: UMAP ----

# 3.1 Use UMAP to get 2-D Projection ----

umap_obj <- customer_products_tbl %>%
    select(-bikeshop_name) %>%
    umap()

#   __umap_obj$layout() => display embedding (x , y) components from applying Umap  ----
umap_results_tbl <- umap_obj$layout %>% as_tibble() %>% set_names(c("x", "y")) %>%
    bind_cols(customer_products_tbl %>% select(bikeshop_name))

umap_results_tbl %>% 
    ggplot(aes(x,y)) + 
    geom_point() + 
    geom_label_repel(aes(label = bikeshop_name), size=3)

# 3.2 Use K-Means to Add Cluster Assignments, Goal = associated each couple to his cluster ----
umap_results_tbl

kmeans_mapped_tbl #is from applying map function => column are List element 

#   __pluck() => extract element from a List (~ slice for list obj) ----
#   from the skree plot we see that 4 clusters is enough that's why pluck(4)

kmeans_4_obj <- kmeans_mapped_tbl %>% pull(k_means) %>% pluck(4)

kmeans_4_clusters_tbl <-  kmeans_4_obj %>% augment(customer_products_tbl) %>%
    select(bikeshop_name, .cluster)

#   __left_joint(Umap_results & kmeans_results)

umap_JOINT_kmeans_tbl <- umap_results_tbl %>% left_join(kmeans_4_clusters_tbl)

# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----

customer_segmentation_2d <- umap_JOINT_kmeans_tbl %>%
    mutate(label_text = str_glue("Customer: {bikeshop_name}
                                 Cluster : {.cluster}")) %>%
    ggplot(aes(x, y, color = .cluster)) + 
    
    # Geometries 
    geom_point() + 
    geom_label_repel(aes(label = label_text), size = 3) + 
    
    # Formatting
    theme_tq() +
    scale_color_discrete() + 
    #scale_color_tq() +
    labs(
        title = "Customer Segmentation Bike shop: 2D projection", 
        subtitle = "UMAP 2D Projection with K-Means Cluster Assignement \nAuthor : Ralph D. Tasing", 
        caption = "Conclusion : 4 customer Segments (customers that have similar purchasing behavior) Identified using 2 algorithms
        
        ") +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 18), 
          plot.caption = element_text(face = "bold.italic", size = 10))




# 4.0 ANALYZE PURCHASING TRENDS ----

customer_trends_tbl %>% 
    pull(price) %>%
    
    # quantile() => define de boxplot prob (0 - 25 - 50 - 75 - 100)
    quantile(probs = c(0, 0.33, 0.66, 1))

cluster_trends_tbl <- customer_trends_tbl %>% left_join(umap_JOINT_kmeans_tbl) %>% 
    mutate(price_bin = case_when (
        price <= 2240 ~ "low", 
        price <= 4260 ~ "medium", 
        TRUE ~ "high" )) %>%
    select(.cluster, model, contains("price"), category_1:quantity_purchased) %>%
    
    # Aggregate quantity purchased by cluster and product attributes
    
    #   __group_by_at() => group_by(for a large range of column) ----
    group_by_at(.vars = vars(.cluster:frame_material)) %>%
    summarise(total_quantity = sum(quantity_purchased)) %>%
    ungroup() %>%
    
    # Calculate Proportion of Total qantity (by cluster)
    group_by(.cluster) %>%
    mutate(prop_of_total = total_quantity / sum(total_quantity)) %>%
    ungroup()

# Cluster 1 - Low/medium price, Road model preference (viewing the trend insight) ----

cluster_trends_tbl %>% 
    filter(.cluster == 1) %>%
    arrange(desc(prop_of_total)) %>%
    mutate(cum_prop = cumsum(prop_of_total)) %>%
    view()

get_cluster_trend <- function(cluster = 1){
    
    cluster_trends_tbl %>% 
        filter(.cluster == cluster) %>%
        arrange(desc(prop_of_total)) %>%
        mutate(cum_prop = cumsum(prop_of_total)) %>%
        view()
    
}

#   Cluster 2 - Low/medium price, Mountain Model Preference, Aluminum Frame 
get_cluster_trend(cluster = 2)

#   Cluster 3 - High End Price, Mountain Preference, Carbon Frame
get_cluster_trend(cluster = 3)

#   Cluster 4 - High End Price, Road preference, Carbon Frame
get_cluster_trend(cluster = 4)



#   UPDATE VISUALIZATION BY ADDING QUALITATTIF INDICE LINK TO EACH CLUSTER ----

#   _create a tibble of qualitatif description = recode (dplyr)

#mutate(cluster_label = recode(.cluster,
 #                             `1` = "High/Medium Mountain - Carbon",
  #                            `2` = "Low/Medium Mountain - Aluminum",
   #                           `3` = "High/Medium Road - Carbon",
    #                          `4` = "Medium/Low Road - Mixed"))

cluster_label_tbl <- tibble(
    .cluster = 1:4, 
    .cluster_label = c(
        "low/medium price,ROAD", 
        "Low/medium Price,MOUNTAIN, Aluminium Frame", 
        "High End Price,MOUNTAIN, Carbon Frame", 
        "High End Price,ROAD, Carbon Frame") 
) %>%
    
# COnvert .cluster (integer) toward factor 
mutate(.cluster = as_factor(.cluster))


#umap_JOINT_kmeans_tbl <- umap_JOINT_kmeans_tbl %>% left_join(cluster_label_tbl)

# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----

umap_JOINT_kmeans_tbl %>% left_join(cluster_label_tbl) %>%
    mutate(label_text = str_glue("Customer: {bikeshop_name}
                                 Cluster : {.cluster}
                                 {.cluster_label}")) %>%
    ggplot(aes(x, y, color = .cluster)) + 
    
    # Geometries 
    geom_point() + 
    geom_label_repel(aes(label = label_text), size = 3) + 
    
    # Formatting
    theme_tq() +
    scale_color_discrete() + 
    #scale_color_tq() +
    labs(
        title = "Customer Segmentation Bike shop: 2D projection", 
        subtitle = "UMAP 2D Projection with K-Means Cluster Assignement \nAuthor : Ralph D. Tasing", 
        caption = "Conclusion : 4 customer Segments (customers that have similar purchasing behavior) 
        Identified using unsupervised Learning techniques
        
        ") +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 18), 
          plot.caption = element_text(face = "bold.italic", size = 8))



