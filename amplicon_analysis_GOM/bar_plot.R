# bar_plot.r

#DSH08
DSH08 = tax_glom(DSH08, "Class")
DSH08 <- transform_sample_counts(DSH08, function(x) x/sum(x))
temp <- c("Acidobacteria", "Subgroup_22", "Acidimicrobiia", "Nitrospira", "Gemmatimonadetes", "Anaerolineae", "Planctomycetacia", "Phycisphaerae", "Alphaproteobacteria", "Nitrososphaeria", "Gammaproteobacteria", "Deltaproteobacteria")
DSH08_p <- plot_bar(DSH08, x="depth", fill = "Class")
pd_DSH08 <- DSH08_p$data %>% 
  as_tibble %>%
  mutate(Class = as.character(Class)) %>%
  replace_na(list(Class = "unknown")) 
Class_abun_DSH08 <- pd_DSH08 %>%
  group_by(Class) %>%
  summarize(Abundance = sum(Abundance)) %>%
  arrange(Abundance)
Class_levels_DSH08 <- Class_abun_DSH08$Class
pd0_DSH08 <- pd_DSH08 %>%
  mutate(Class = factor(Class, temp))
D08 <- ggplot(pd0_DSH08, aes(x = depth, y = Abundance, fill = Class )) + geom_bar(stat="identity") + ggtitle("DSH08 1100m") + scale_x_discrete(name = "Depth (cm) ", limits = c(0, 5, 10, 15, 20))

#DSH09

DSH09 = tax_glom(DSH09, "Class")
DSH09 <- transform_sample_counts(DSH09, function(x) x/sum(x))

DSH09_p <- plot_bar(DSH09, x="depth", fill = "Class")
pd_DSH09 <- DSH09_p$data %>% 
  as_tibble %>%
  mutate(Class = as.character(Class)) %>%
  replace_na(list(Class = "unknown")) 
pd0_DSH09 <- pd_DSH09 %>%
  mutate(Class = factor(Class, temp))
D09 <- ggplot(pd0_DSH09, aes(x = depth, y = Abundance, fill = Class )) + geom_bar(stat="identity") + ggtitle("DSH09 2290m") + scale_x_discrete(name = "Depth (cm) ", limits = c(0, 5, 10, 15, 20))

#DSH10

DSH10 = tax_glom(DSH10, "Class")
DSH10 <- transform_sample_counts(DSH10, function(x) x/sum(x))

DSH10_p <- plot_bar(DSH10, x="depth", fill = "Class")
pd_DSH10 <- DSH10_p$data %>% 
  as_tibble %>%
  mutate(Class = as.character(Class)) %>%
  replace_na(list(Class = "unknown")) 
pd0_DSH10 <- pd_DSH10 %>%
  mutate(Class = factor(Class, temp))
D10 <- ggplot(pd0_DSH10, aes(x = depth, y = Abundance, fill = Class )) + geom_bar(stat="identity") + ggtitle("DSH10 1500m") + scale_x_discrete(name = "Depth (cm) ", limits = c(0, 5, 10, 15, 20))

#IXW250

IXW250 = tax_glom(IXW250, "Class")
IXW250 <- transform_sample_counts(IXW250, function(x) x/sum(x))

IXW250_p <- plot_bar(IXW250, x="depth", fill = "Class")
pd_IXW250 <- IXW250_p$data %>% 
  as_tibble %>%
  mutate(Class = as.character(Class)) %>%
  replace_na(list(Class = "unknown")) 
pd0_IXW250 <- pd_IXW250 %>%
  mutate(Class = factor(Class, temp))
I25 <- ggplot(pd0_IXW250, aes(x = depth, y = Abundance, fill = Class )) + geom_bar(stat="identity") + ggtitle("IXW250 583m") + scale_x_discrete(name = "Depth (cm) ", limits = c(0, 5, 10, 15, 20))

#IXW500

IXW500 = tax_glom(DSH08, "Class")
IXW500 <- transform_sample_counts(IXW500, function(x) x/sum(x))

IXW500_p <- plot_bar(IXW500, x="depth", fill = "Class")
pd_IXW500 <- IXW500_p$data %>% 
  as_tibble %>%
  mutate(Class = as.character(Class)) %>%
  replace_na(list(Class = "unknown")) 
pd0_IXW500 <- pd_IXW500 %>%
  mutate(Class = factor(Class, temp))
I50 <- ggplot(pd0_IXW500, aes(x = depth, y = Abundance, fill = Class )) + geom_bar(stat="identity") + ggtitle("IXW500 1010m") + scale_x_discrete(name = "Depth (cm) ", limits = c(0, 5, 10, 15, 20))

#IXW750

IXW750 = tax_glom(IXW750, "Class")
IXW750 <- transform_sample_counts(IXW750, function(x) x/sum(x))

IXW750_p <- plot_bar(IXW750, x="depth", fill = "Class")
pd_IXW750 <- IXW750_p$data %>% 
  as_tibble %>%
  mutate(Class = as.character(Class)) %>%
  replace_na(list(Class = "unknown")) 
pd0_IXW750 <- pd_IXW750 %>%
  mutate(Class = factor(Class, temp))
I75 <- ggplot(pd0_IXW750, aes(x = depth, y = Abundance, fill = Class )) + geom_bar(stat="identity") + ggtitle("IXW750 1440m") + scale_x_discrete(name = "Depth (cm) ", limits = c(0, 5, 10, 15, 20))
