source("limpieza.R")

# Gráfico de blancos y nulos nacional
df %>% 
select(c(3,6,  11:25)) %>%
select(-inscritos) %>% 
  mutate(
    votos_validos = cc + fpv + mts + ucs + mas_ipsp + x21f + pdc + mnr + pan_bol,
    emitidos = votos_validos  + blancos + nulos,
    blancos_por = blancos/emitidos*100,
    nulos_por = nulos/emitidos*100
  ) %>% 
  group_by(eleccion) %>% 
  summarise(
    `Votos blancos` = sum(blancos)/sum(emitidos),
    `Votos nulos` = sum(nulos)/sum(emitidos)
  ) %>%  
  gather(key, value, -eleccion) %>% 
  mutate(eleccion = str_replace(eleccion, "Diputados Uninominales", "Diputados\nUninominales")) %>% 
  ggplot(aes(eleccion, value, fill = eleccion)) +
  geom_col(color = NA, alpha = 1) + 
  facet_wrap(~key) + 
  hrbrthemes::theme_ipsum_rc(base_family = "Source Code Pro", grid = "Y") + 
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 10), 
    strip.text = element_text(face = "bold"),
    plot.subtitle = element_text(family = "Source Code Pro"),
    plot.title = element_text(family = "Source Code Pro"), 
    plot.caption = element_text(family = "Source Code Pro Light")
  ) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = paste0(round(value*100, 0), "%")), 
            family = "Source Code Pro", color = "white", vjust = 1.5, 
            size = 5) + 
  labs(
    title = "No desperdicies tu voto votando BLANCO a los uninominales",
    subtitle = "Elección nacional 2019",
    y = "",
    x = "",
    caption = "ralv-jcbr"
  ) + 
  scale_fill_manual(values = c("#E01F52",  "#222438")) -> blancos_nulos_nal

# desperdicio blanco y nulo por depto
df %>% 
  select(c(3,6,  11:25)) %>%
  select(-inscritos) %>% 
  mutate(
    votos_validos = cc + fpv + mts + ucs + mas_ipsp + x21f + pdc + mnr + pan_bol,
    emitidos = votos_validos  + blancos + nulos,
    blancos_por = blancos/emitidos*100,
    nulos_por = nulos/emitidos*100
  ) %>% 
  group_by(eleccion, departamento) %>% 
  summarise(
    `Votos blancos` = sum(blancos)/sum(emitidos),
    `Votos nulos` = sum(nulos)/sum(emitidos)
  ) %>%  
  gather(key, value, -eleccion, -departamento) %>% 
  mutate(
    eleccion = str_replace(eleccion, "Diputados Uninominales", "Diputados\nUninominales"),
    eleccion = str_replace(eleccion, "Presidente y Vicepresidente", "Presidente\nVicepresidente")
  ) %>% 
  filter(key == "Votos blancos") %>% 
  ggplot(aes(eleccion, value, fill = eleccion)) +
  geom_col(color = NA, alpha = 1) + 
  facet_wrap(~departamento) + 
  hrbrthemes::theme_ipsum_rc(base_family = "Source Code Pro", grid = "Y") + 
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 10), 
    strip.text = element_text(face = "bold", vjust = 1),
    plot.subtitle = element_text(family = "Source Code Pro"),
    plot.title = element_text(family = "Source Code Pro"),
    plot.caption  = element_text(family = "Source Code Pro Light"),
  ) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = paste0(round(value*100, 0), "%")), 
            family = "Source Code Pro", color = "black", 
            vjust = -0.1, hjust = 0.5, size = 4) + 
  labs(
    title = "No desperdicies tu voto votando BLANCO a los uninominales",
    subtitle = "Elección nacional 2019",
    y = "",
    x = "",
    caption =  "ralv-jcbr"
  ) + 
  scale_fill_manual(values = c("#E01F52",  "#222438")) -> blancos_nulos_dpal



# blancos unis vs presi
df %>%
  select(c(3,6,  11:25)) %>%
  select(-inscritos) %>%
  mutate(
    votos_validos = cc + fpv + mts + ucs + mas_ipsp + x21f + pdc + mnr + pan_bol,
    emitidos = votos_validos  + blancos + nulos,
    blancos_por = blancos/emitidos*100,
    nulos_por = nulos/emitidos*100
  ) %>%
  select(departamento, municipio, codigo_mesa, eleccion, blancos_por) %>%
  group_split(eleccion) %>%
  map(., ~arrange(., blancos_por) %>%
        mutate(num = 1:nrow(.))) %>%
  bind_rows() %>%
  mutate(
    num = as.double(num),
    num = case_when(
      eleccion == "Presidente y Vicepresidente" ~ 0,
      T ~ num
    )
  ) %>%
  mutate(total_1 = if_else(eleccion == "Diputados Uninominales", -blancos_por, blancos_por)) %>%
  ggplot(aes(x = fct_reorder(codigo_mesa, num, .desc = F), y = total_1, group = eleccion, fill = eleccion)) +
  geom_bar(stat = "identity", width = 0.75, color = NA) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-100, 100, 10),
                      labels = abs(seq(-100, 100, 10)), 
                     sec.axis = dup_axis()) +
  hrbrthemes::theme_ipsum_rc(grid = "X", base_family = "Source Code Pro") +
  theme(
    axis.text.y = element_blank(),
    legend.position = "top",
    plot.subtitle = element_text(family = "Source Code Pro"),
    plot.caption  = element_text(family = "Source Code Pro Light"),
  ) +
  labs(
    title = "No desperdicies tu voto votando BLANCO a los uninominales",
    subtitle = "Cada raya es una mesa de las 33 mil mesas\nElección nacional 2019",
    fill = "Porcentaje de voto blanco por mesa",
    x = "",
    y = "Porcentaje (%) de voto blanco", 
    caption = "ralv-jcbr"
  )  +
  scale_fill_manual(values = c("#E01F52",  "#222438")) -> blancos_mesas

# 
# 
# # con facet
# df %>% 
#   select(c(3,6,  11:25)) %>%
#   select(-inscritos) %>% 
#   mutate(
#     votos_validos = cc + fpv + mts + ucs + mas_ipsp + x21f + pdc + mnr + pan_bol,
#     emitidos = votos_validos  + blancos + nulos,
#     blancos_por = blancos/emitidos*100,
#     nulos_por = nulos/emitidos*100
#   ) %>% 
#   select(departamento, municipio, codigo_mesa, eleccion, blancos_por) %>% 
#   group_split(eleccion) %>% 
#   map(., ~arrange(., blancos_por) %>% 
#         mutate(num = 1:nrow(.))) %>% 
#   bind_rows() %>% 
#   mutate(
#     num = as.double(num),
#     num = case_when(
#       eleccion == "Presidente y Vicepresidente" ~ 0,
#       T ~ num
#     )
#   ) %>%  
#   mutate(total_1 = if_else(eleccion == "Diputados Uninominales", -blancos_por, blancos_por)) %>% 
#   ggplot(aes(x = fct_reorder(codigo_mesa, num, .desc = F), y = total_1, group = eleccion, fill = eleccion)) +
#   geom_bar(stat = "identity", width = 0.75, color = NA) +
#   coord_flip() +
#   scale_y_continuous(breaks = seq(-100, 100, 10), 
#                      labels = abs(seq(-100, 100, 10))) + 
#   hrbrthemes::theme_ipsum_rc(grid = "X", base_family = "Source Code Pro") + 
#   theme(
#     axis.text.y = element_blank(), 
#     legend.position = "top",
#     plot.subtitle = element_text(family = "Source Code Pro")
#   ) +
#   labs(
#     title = "NO DESPERDICIES TU VOTO",
#     subtitle = "Cada raya es una mesa de las 33 mil mesas\nElección nacional 2019",
#     fill = "Porcentaje de voto blanco por mesa",
#     x = "",
#     y = "Porcentaje (%) de voto blanco"
#   )  +
#   scale_fill_manual(values = c("#E01F52",  "#222438")) +
#   facet_wrap(~departamento, scales = "free")
#   
# 
# voto del mas ipsp
df_vertical_porcentaje %>%
  filter(partido == "mas_ipsp") %>%
  mutate(total_1 = if_else(eleccion == "Diputados Uninominales", -votos, votos)) %>%
  group_split(eleccion) %>%
  map(., ~arrange(., votos) %>%
        mutate(num = 1:nrow(.))) %>%
  bind_rows() %>%
  mutate(
    num = as.double(num),
    num = case_when(
      eleccion == "Presidente y Vicepresidente" ~ 0,
      T ~ num
    )
  ) %>%
  ggplot(aes(x = fct_reorder(codigo_mesa, num, .desc = T), y = total_1, group = eleccion, fill = eleccion)) +
  geom_bar(stat = "identity", width = 0.75, color = NA) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-100, 100, 10),
                     labels = abs(seq(-100, 100, 10)),
                     sec.axis = dup_axis()) +
  hrbrthemes::theme_ipsum_rc(grid = "X", base_family = "Source Code Pro") +
  theme(
    axis.text.y = element_blank(),
    legend.position = "top",
    plot.subtitle = element_text(family = "Source Code Pro"),
    plot.caption = element_text(family = "Source Code Pro Light"),
  ) +
  scale_fill_manual(values = c("blue",  "black")) +
  labs(
    title = "MAS-IPSP: VOTO EN LÍNEA",
    subtitle = "Cada raya es una mesa de las 33 mil mesas\nElección nacional 2019",
    fill = "Porcentaje de voto a favor del MAS-IPSP",
    x = "",
    y = "Porcentaje (%) de voto",
    caption = "ralv-jcbr"
  ) -> mas_ipsp


# para los otros dpartidos
df_vertical_porcentaje %>%
  filter(partido == "bdn") %>%
  mutate(total_1 = if_else(eleccion == "Diputados Uninominales", -votos, votos)) %>%
  group_split(eleccion) %>%
  map(., ~arrange(., votos) %>%
        mutate(num = 1:nrow(.))) %>%
  bind_rows() %>%
  mutate(
    num = as.double(num),
    num = case_when(
      eleccion == "Presidente y Vicepresidente" ~ 0,
      T ~ num
    )
  ) %>%
  ggplot(aes(x = fct_reorder(codigo_mesa, num, .desc = T), y = total_1, group = eleccion, fill = eleccion)) +
  geom_bar(stat = "identity", width = 0.75, color = NA) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-100, 100, 10),
                     labels = abs(seq(-100, 100, 10)),
                     sec.axis = dup_axis()) +
  hrbrthemes::theme_ipsum_rc(grid = "X", base_family = "Source Code Pro") +
  theme(
    axis.text.y = element_blank(),
    legend.position = "top",
    plot.subtitle = element_text(family = "Source Code Pro"),
    plot.caption = element_text(family = "Source Code Pro Light"),
  ) +
  scale_fill_manual(values = c("red",  "#27AE60")) +
  labs(
    title = "BOLIVIA DIJO NO: VOTO CRUZADO",
    subtitle = "Cada raya es una mesa de las 33 mil mesas\nElección nacional 2019",
    fill = "Porcentaje de voto a favor de Bolivia dijo NO",
    x = "",
    y = "Porcentaje (%) de voto",
    caption = "ralv-jcbr"
  ) -> bdn

# bnd scz
df_vertical_porcentaje %>%
  filter(partido == "bdn") %>%
  filter(departamento == "Santa Cruz") %>%
  mutate(total_1 = if_else(eleccion == "Diputados Uninominales", -votos, votos)) %>%
  group_split(eleccion) %>%
  map(., ~arrange(., votos) %>%
        mutate(num = 1:nrow(.))) %>%
  bind_rows() %>%
  mutate(
    num = as.double(num),
    num = case_when(
      eleccion == "Presidente y Vicepresidente" ~ 0,
      T ~ num
    )
  ) %>% 
  ggplot(aes(x = fct_reorder(codigo_mesa, num, .desc = T), y = total_1, group = eleccion, fill = eleccion)) +
  geom_bar(stat = "identity", width = 0.75, color = NA) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-100, 100, 10),
                     labels = abs(seq(-100, 100, 10)),
                     sec.axis = dup_axis()) +
  hrbrthemes::theme_ipsum_rc(grid = "X", base_family = "Source Code Pro") +
  theme(
    axis.text.y = element_blank(),
    legend.position = "top",
    plot.subtitle = element_text(family = "Source Code Pro"),
    plot.caption = element_text(family = "Source Code Pro Light"),
  ) +
  scale_fill_manual(values = c("red",  "#27AE60")) +
  labs(
    title = "BOLIVIA DIJO NO: VOTO CRUZADO EN SANTA CRUZ",
    subtitle = "Cada raya es una mesa de las 8552 mesas\nElección nacional 2019",
    fill = "Porcentaje de voto a favor de Bolivia dijo NO",
    x = "",
    y = "Porcentaje (%) de voto",
    caption = "ralv-jcbr"
  ) -> bdn_scz


# scatters  bdn
df_vertical_porcentaje %>% 
  filter(partido == "bdn") %>% 
  select(departamento, codigo_mesa, eleccion, votos) %>% 
  spread(eleccion, votos) %>% 
  janitor::clean_names() %>% 
  ggplot(aes(diputados_uninominales, presidente_y_vicepresidente)) +
  geom_point(color = "red", alpha = 0.2, size = 0.7) + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~departamento, scales = "free") +
  hrbrthemes::theme_ipsum_rc(base_family = "Source Code Pro") +
  labs(
    title = "BOLIVIA DIJO NO: MIENTRAS MÁS PLANA LA TENDENCIA MÁS VOTO CRUZADO",
    subtitle = "Cada punto es una mesa de las 33 mil\nElección nacional 2019",
    x = "Porcentaje (%) de voto a diputados uninominales",
    y = "Porcentaje (%) de voto a Presidente",
    caption = "ralv-jcbr"
  )  + 
  theme(
    plot.subtitle = element_text(family = "Source Code Pro"),
    plot.caption = element_text(family = "Source Code Pro Light"),
  ) -> scatter_bdn







# # idem con facet 
# df_vertical_porcentaje %>% 
#   filter(partido == "mas_ipsp") %>% 
#   mutate(total_1 = if_else(eleccion == "Diputados Uninominales", -votos, votos)) %>% 
#   group_split(eleccion) %>% 
#   map(., ~arrange(., votos) %>% 
#         mutate(num = 1:nrow(.))) %>% 
#   bind_rows() %>% 
#   mutate(
#     num = as.double(num),
#     num = case_when(
#       eleccion == "Presidente y Vicepresidente" ~ 0,
#       T ~ num
#     )
#   ) %>%  
#   ggplot(aes(x = fct_reorder(codigo_mesa, num, .desc = T), y = total_1, group = eleccion, fill = eleccion)) +
#   geom_bar(stat = "identity", width = 0.75, color = NA) +
#   coord_flip() +
#   scale_y_continuous(breaks = seq(-100, 100, 10), 
#                      labels = abs(seq(-100, 100, 10))) + 
#   hrbrthemes::theme_ipsum_rc(grid = "X", base_family = "Source Code Pro") + 
#   theme(
#     axis.text.y = element_blank(), 
#     legend.position = "top",
#     plot.subtitle = element_text(family = "Source Code Pro")
#   ) +
#   scale_fill_manual(values = c("blue",  "black")) +
#   labs(
#     title = "MAS-IPSP: VOTO EN LÍNEA",
#     subtitle = "Cada raya es una mesa de las 33 mil mesas\nElección nacional 2019",
#     fill = "Porcentaje de voto a favor del MAS-IPSP",
#     x = "",
#     y = "Porcentaje (%) de voto"
#   ) +
#   facet_wrap(~departamento)
# 
# 
# # scatter de bdb vs bdn y bdn vs 
# df_vertical_porcentaje %>% 
#   filter(partido == "bdn") %>% 
#   select(departamento, codigo_mesa, eleccion, votos) %>% 
#   mutate(tipo = "A") %>% 
#   spread(eleccion, votos) %>% 
#   janitor::clean_names() -> temp
# 
# 
# df_vertical_porcentaje %>% 
#   filter(partido %in% c("bdn", "cc")) %>% 
#   group_split(partido) -> temp1
#   
# temp1[[1]] %<>% filter(eleccion == "Diputados Uninominales")
# temp1[[1]] %<>% filter(partido == "bdn")
# temp1[[2]] %<>% filter(eleccion == "Presidente y Vicepresidente")
# temp1[[2]] %<>% filter(partido == "cc")
# 
# bind_rows(temp1) %>% 
#   select(departamento, codigo_mesa, eleccion, votos) %>% 
#   mutate(tipo = "B") %>% 
#   spread(eleccion, votos) %>% 
#   janitor::clean_names() -> temp2
#   
# bind_rows(temp, temp2) %>% 
#   filter(departamento == "Santa Cruz") %>% 
#   ggplot(aes(presidente_y_vicepresidente, diputados_uninominales)) +
#   geom_point(alpha = 0.1) +
#   facet_wrap(~tipo) + 
#   hrbrthemes::theme_ipsum_rc() + 
#   geom_smooth(method = "lm")
#   
# 
# 
# 
# temp %>% 
#   ggplot(aes(diputados_uninominales, presidente_y_vicepresidente)) +
#   geom_point(alpha = 0.1)
# 
# 
# 
# 
# 
#   
# 
#   
#   