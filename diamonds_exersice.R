ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)

ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat))+
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, color = cut))+
  geom_freqpoly(binwidth = 0.1)

ggplot(data = faithful, mapping = aes( x = eruptions))+
  geom_histogram(bindwith = 0.25)

ggplot(data = diamonds, mapping = aes(x = y))+
  geom_histogram(bindwith = 0.5)

ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x = y), bindwith = 0.5)+
  coord_cartesian(xlim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  arrange(y)

diamonds2 <- diamonds %>% 
  mutate(ifelse(y <3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y ))+
  geom_point(na.rm = TRUE)

ggplot(data = diamonds, mapping = aes(x = price))+
  geom_freqpoly(mapping = aes(color = cut), bindwith = 500)

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..))+
  geom_freqpoly(mapping = aes(color = cut), bindwith = 500)

ggplot(data =  diamonds, mapping = aes(x = cut, y = price))+
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy))+
  geom_boxplot()

ggplot(data = mpg)+
  geom_boxplot(mapping = aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ))+
  coord_flip()

ggplot(data = diamonds)+
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut) %>% 
  ggplot(mapping = aes(x = color, y = cut))+
  geom_tile(mapping = aes(fill = n))

summary(select(diamonds, x, y, z))

ggplot(diamonds)+
  geom_histogram(mapping = aes(x = x), bindwith = 0.01)

ggplot(diamonds)+
  geom_histogram(mapping = aes(x = y), bindwith = 0.01)

ggplot(diamonds)+
  geom_histogram(mapping = aes(x = z), bindwith = 0.01)


filter(diamonds, x == 0 | y == 0 | z == 0)

diamonds %>% 
  arrange(desc(y)  ) %>% 
  head()

diamonds %>% 
  arrange(desc(z)  ) %>% 
  head()
ggplot(diamonds,aes(x =  x, y = y))+
  geom_point()

ggplot(diamonds,aes(x =  x, y = z))+
  geom_point()

ggplot(diamonds,aes(x =  y, y = z))+
  geom_point()

filter(diamonds, x > 0, x < 10) %>% 
  ggplot()+
  geom_histogram(mapping = aes(x = x), bindwith = 0.01)+
  scale_x_continuous(breaks = 1:10)

filter(diamonds, y > 0, y < 10) %>% 
  ggplot()+
  geom_histogram(mapping = aes(x = y), bindwith = 0.01)+
  scale_x_continuous(breaks = 1:10)

filter(diamonds, z > 0, z < 10) %>% 
  ggplot()+
  geom_histogram(mapping = aes(x = z), bindwith = 0.01)+
  scale_x_continuous(breaks = 1:10)

summarise(diamonds, mean(x > y), mean(x > z), mean( y > z))

ggplot(data = diamonds, mapping = aes(x = carat , y = price))+
  geom_point(alpha = 1/100)

ggplot(data = smaller, mapping = aes(x = carat, y = price))+
  geom_bin2d()

ggplot(data = smaller, mapping = aes(x = carat, y = price))+
  geom_hex()
  
ggplot(data = smaller, mapping = aes(x = carat, y = price))+
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price))+
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

ggplot(filter(diamonds, price < 2500), aes(x = price))+
  geom_histogram(bindwith = 10, center = 0)

ggplot(filter(diamonds), aes(x = price))+
  geom_histogram(bindwith = 100, center = 0)

  diamonds %>% 
    mutate(ending = price %% 10 ) %>% 
      ggplot(aes(x = ending))+
    geom_histogram(bindwith = 1, center = 0)
  
  diamonds %>% 
    mutate(ending = price %% 100) %>% 
    ggplot(aes(x = ending))+
    geom_histogram(bindwith = 1)
  
diamonds %>% 
  mutate(ending = price %% 1000) %>% 
  filter(ending >= 500, ending <= 800) %>% 
  ggplot(aes(x = ending))+
  geom_histogram(bindwith = 1)

ggplot(data = diamonds, mapping = aes(x = x, y = y))+
  geom_point()+
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

ggplot(data = faithful)+
  geom_point(mapping = aes(x = eruptions, y = waiting))



mod <- lm(log(price) ~log(carat), data = diamonds)

diamonds3 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds3, mapping = aes(x = carat, y = resid))+
  geom_point()

ggplot(data = diamonds3, mapping = aes(x = cut, y = resid))+
  geom_boxplot()

diamonds %>% 
  filter(carat >= 0.9, carat <= 1.1) %>% 
  count(carat) %>% 
  print(n = Inf)

ggplot(faithful,aes(eruptions))+
  geom_freqpoly(bindwith = 0.25)

diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n))+
  geom_tile()

ggplot(diamonds)+
  geom_histogram(aes(x = price))+
  coord_cartesian(xlim = c(100, 5000), ylim = c(0, 3000))

ggplot(diamonds)+
  geom_histogram(aes(x = price))+
  xlim(100, 5000)+
  ylim(0, 3000)

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3| y >20, NA, y))

ggplot(diamonds2,aes(x = y))+
  geom_histogram()

diamonds %>% 
  mutate(cut = if_else(runif(n()) < 0.1, NA_character_, as.character(cut))) %>% 
  ggplot()+
  geom_bar(aes(x =cut))

mean(c(0, 1, 2, NA), na.rm = TRUE)
sum(c( 0, 1, 2, NA), na.rm = TRUE)


ggplot(diamonds, aes(x = carat, y = price))+
  geom_point()

ggplot(diamonds, aes(x =carat, y = price))+
  geom_boxplot(aes(group = cut_width(carat, 0.1)),orientation = "x")

diamonds %>% 
  mutate(color = fct_rev(color)) %>% 
  ggplot(aes(x = color, y =  price))+
  geom_boxplot()

ggplot(diamonds, aes(x = clarity, y = price))+
  geom_boxplot()

ggplot(diamonds, aes(x = cut, y = carat))+
  geom_boxplot()
ggplot(data = mpg)+
  geom_boxplot(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy))+
  coord_flip()

ggplot(data = mpg)+
  geom_boxplot(mapping = aes(y = reorder(class, hwy, FUN = median),x = hwy), orientation = "y")

ggplot(diamonds, aes(x = cut, y = price))+
  geom_lv()

ggplot(diamonds, aes(x = price , y = ..density..))+
  geom_freqpoly(aes(color = cut), bindwith = 500)

ggplot(diamonds, aes(x =  price))+
  geom_histogram()+
  facet_wrap(~cut, ncol = 1, scales = "free_y")

ggplot(diamonds, aes(x = cut, y= price ))+
  geom_violin()+
  coord_flip()

ggplot(mpg)+
  geom_quasirandom(aes( 
  x = reorder(class, hwy, FUN = median),
  y = hwy
  ))

ggplot(mpg)+
  geom_quasirandom(aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ),
  method = "tukey"
  )

ggplot(mpg)+
  geom_quasirandom(aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ),
  method = "tukeyDense"
  )

ggplot(mpg)+
  geom_quasirandom(aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ),
  method = "frowney"
  
  )

ggplot(mpg)+
  geom_quasirandom(aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ),
  method = "smiley"
  
  )

ggplot(mpg)+
  geom_beeswarm(aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ))

diamonds %>% 
  count(color, cut) %>% 
  group_by(color) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = color, y = cut))+
  geom_tile(aes(fill = prop))

diamonds %>% 
  count(color, cut) %>% 
  group_by(cut) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = color, y = cut))+
  geom_tile(aes(fill = prop))

diamonds %>% 
  count(color, cut) %>% 
  ggplot(aes(x = cut, y = color))+
  geom_tile(aes(fill = n))

ggplot(diamonds, aes(color = cut_number(carat, 5), x = price))+
  geom_freqpoly()+
  labs(x = "price", y ="count", color = "carat")

ggplot(diamonds,aes(color =cut_width(carat, 1, boundary = 0), x = price))+
  geom_freqpoly()+
  labs(x = "price", y ="count", color = "carat")


ggplot(diamonds,aes(color = cut_number(price, 10), y = carat))+
  geom_boxplot()+
  coord_flip()+
  xlab("price")

ggplot(diamonds, aes(color = cut_width(price, 2000, boundary = 0),y = carat))+
  geom_boxplot(varwidth = TRUE)+
  coord_flip()+
  xlab("price")

ggplot(diamonds,aes(x = carat, y = price))+
  geom_hex()+
  facet_wrap(~cut, ncol = 1)

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, color = cut))+
  geom_boxplot()
ggplot(diamonds, aes(color = cut_number(carat, 5), y = price, x = cut))+
  geom_boxplot()

ggplot(diamonds)+
  geom_point(aes(x = x, y = y))+
  coord_cartesian(xlim = c(4,11), ylim = c(4,11))

