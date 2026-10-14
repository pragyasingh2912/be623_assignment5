setwd("~/be623_assignment5")
library(dplyr)
library(ggplot2)

circ_df <- data.frame(
  Sample = paste0("S", 1:12),
  Gene = c(
    rep("PER2", 6),
    rep("BMAL1", 6)
  ),
  ZT = c(
    0, 4, 8, 12, 16, 20,
    0, 4, 8, 12, 16, 20
  ),
  Expression = c(
    1.8, 3.2, 6.5, 9.4, 7.1, 3.6,
    8.6, 5.9, 2.7, 1.5, 3.4, 6.8
  )
)

str(circ_df)
per2_rows <- circ_df[circ_df$Gene == "PER2", ]
mean(per2_rows$Expression)
median(per2_rows$Expression)
sd(per2_rows$Expression)
write.csv(circ_df, "circadian_expression.csv", row.names = FALSE)
circ_reloaded <- read.csv("circadian_expression.csv")
identical(circ_df$Expression, circ_reloaded$Expression)

per2_peak <- circ_df %>%
  filter(Gene == "PER2") %>%
  arrange(desc(Expression))

bmal1_peak <- circ_df %>%
  filter(Gene == "BMAL1") %>%
  arrange(desc(Expression))

filter(circ_df, Gene == "PER2") %>%
  arrange(desc(Expression))
filter(circ_df, Gene == "BMAL1") %>%
  arrange(desc(Expression))

circ_df2 <- circ_df %>%
  mutate(dark = ZT >= 12)
circ_df2 %>%
  filter(dark == TRUE) %>%
  group_by(Gene) %>%
  summarise(
    mean_expression = mean(Expression),
    n_samples = n()
  )
circ_df2 %>%
  filter(dark == FALSE) %>%
  group_by(Gene) %>%
  summarise(
    mean_expression = mean(Expression),
    n_samples = n()
  )

rhythm_plot <- ggplot(circ_df, aes(x = ZT, y = Expression, colour = Gene)) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("PER2" = "blue", "BMAL1" = "red")) +
  labs(
    title = "Circadian Gene Expression Across the Day",
    x = "Zeitgeber time (h)",
    y = "Expression",
    colour = "Gene"
  ) +
  theme_minimal()
print(rhythm_plot)
ggsave("rhythm_plot.png", plot = rhythm_plot)

expression_boxplot <- ggplot(circ_df, aes(x = Gene, y = Expression, fill = Gene)) +
  geom_boxplot() +
  scale_fill_manual(values = c("PER2" = "blue", "BMAL1" = "red")) +
  labs(
    title = "Expression Distribution by Gene",
    x = "Gene",
    y = "Expression"
  ) +
  theme_minimal()
print(expression_boxplot)
ggsave("expression_boxplot.png", plot = expression_boxplot)
getwd()
