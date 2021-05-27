#::::::::::::::::::::::: Insighs to scenarios ::::::::::::::::::::::::::::::::::

node_failure_df <- node_failure_mat %>%
  as.data.frame() %>% as_tibble() %>%
  setNames(nodes_data[[1]]$id) %>%
  mutate(scn = 1:n()) %>%
  gather(key = id, value = disuse, -scn) %>%
  arrange(scn) %>%
  mutate(id = as.numeric(id))

pipe_failure_df <- pipe_failure_mat %>%
  as.data.frame() %>% as_tibble() %>%
  setNames(pipes_data[[1]]$id) %>%
  mutate(scn = 1:n()) %>%
  gather(key = id, value = disuse, -scn) %>%
  arrange(scn) %>%
  mutate(id = as.numeric(id))

p1a <- nodes_data[[1]] %>%
  filter(id %in% snodes) %>%
  ggplot(aes(x = as.factor(id), y = prFail)) +
  theme_light() +
  geom_bar(fill = "gray50", stat = "identity") +
  labs(x = "ids", y = "likeliohood of failure") +
  scale_y_continuous(limits = c(0,.1))  

p1b <- node_failure_df %>%
  filter(id %in% snodes) %>%
  ggplot(aes(x = as.factor(id), y = disuse)) +
  theme_light() +
  geom_histogram(fill = "gray50", stat = "identity") +
  labs(x = "ids", y = "count of failures")

p1c <- node_failure_df %>%
  group_by(scn) %>%
  summarize(disuse = sum(disuse)) %>%
  mutate(disuse = as.factor(disuse)) %>% 
  ggplot(aes(x = disuse)) +
  theme_light() +
  geom_histogram(fill = "gray50", stat = "count")


p1 <- plot_grid(p1a, p1b, p1c, nrow = 3, align = "v", axis ="t")

p2a <- pipes_data[[1]] %>%
  ggplot(aes(x = as.factor(id), y = prFail)) +
  theme_light() +
  geom_bar(fill = "gray50", stat = "identity") +
  labs(x = "ids", y = "likeliohood of failure") +
  scale_y_continuous(limits = c(0,.1))  

p2b <- pipe_failure_df %>%
  ggplot(aes(x = as.factor(id), y = disuse)) +
  theme_light() +
  geom_histogram(fill = "gray50", stat = "identity") +
  labs(x = "ids", y = "count of failures")

p2c <- pipe_failure_df %>%
  group_by(scn) %>%
  summarize(disuse = sum(disuse)) %>%
  mutate(disuse = as.factor(disuse)) %>% 
  ggplot(aes(x = disuse)) +
  theme_light() +
  geom_histogram(fill = "gray50", stat = "count") +
  labs(x = "Concurrent failures", y = "count")

p2 <- plot_grid(p2a, p2b, p2c, nrow = 3, align = "v", axis ="t")

p <- plot_grid(p1, p2, ncol = 2, align = "v", axis ="t")
ggsave(filename =paste0(outPath, "failures_info.png"), 
       height = 8, width = 12)