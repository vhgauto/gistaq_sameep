
pred_turb |>
    filter(is.na(turb) & !is.na(.pred)) |>
    select(fecha, .pred) |>
    write_tsv("datos/turb_pred.tsv")

pr <- read_tsv("datos/turb_pred.tsv")

ggplot(data = pr, aes(fecha, .pred)) +
    geom_line(alpha = .7, color = "lightgrey") +
    geom_point() +
    geom_smooth(formula = y ~ x, method = "loess", se = FALSE) +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%m\n%Y") +
    theme_bw() +
    theme(aspect.ratio = 1,
          panel.grid.minor = element_blank())


predict(workflow_final, gee_test_new, type = "conf_int")

#################################

rf_refit <- randomForest::randomForest(turb ~ ., data = turb_train2, 
                                       mtry = best_mtry,
                                       nodesize = beat_min_n,
                                       ntree = 1000,
                                       keep.inbag=TRUE)

# estimación del testing split
turb_test2 <- turb_test |> 
  mutate(mes = month(fecha)) |> 
  select(-fecha)

# predicciones, de todos los árboles
pred.rf <- predict(rf_refit, turb_test2, predict.all = TRUE)

# R^2
tibble(turb = turb_test$turb,
       pred = pred.rf$aggregate) |>
  lm(turb ~ pred, data = _) |>
  summary() |>
  broom::glance() |>
  select(r.squared) |>
  pull()
# R^2 =  0.9170002

# figura SAMEEP vs RF (c/intervalos)
# límites superior e inferior
pred_inf <- map_dbl(.x = 1:nrow(turb_test2),
                    ~ mean(pred.rf$individual[.x, ]) -
                    1.96 * sd(pred.rf$individual[.x, ]))

pred_sup <- map_dbl(.x = 1:nrow(turb_test2),
                    ~ mean(pred.rf$individual[.x, ]) +
                    1.96 * sd(pred.rf$individual[.x, ]))

# en un único tibble: fecha, turb, pred, pred_sup & pred_inf
# del test split
turb_nuevos <- tibble(fecha = turb_test$fecha,
       turb = turb_test$turb,
       pred = pred.rf$aggregate,
       pred_inf = pred_inf,
       pred_sup = pred_sup)

# R^2
r2 <- lm(turb ~ pred, data = turb_nuevos) |>
  summary() %>%
  .$r.squared |>
  round(digits = 3) |>
  sub(pattern = "\\.", replacement = ",", x = _)

# máx eje vertical
turb_m <- max(sameep_tidy$turb) |> round(digits = -2)

# máx eje horizontal
fecha_m <- max(gee_tidy$fecha) |> ceiling_date(unit = "month")

# etiquta R^2
etq <- tibble(x = ymd(20211001),
              y = turb_m,
              label = glue("R<sup>2</sup> = {r2}<br>{{randomForest}}"))

fecha_turb <- read_tsv("datos/datos_nuevos.tsv") |>
  distinct(fecha) |>
  pull(fecha)




final_res %>%
  collect_predictions() |>
  lm(turb ~ .pred, data = _) |>
  summary() # R2: 0.9031

# predicción VS fecha -----------------------------------------------------

# extraigo el workflow del último entrenamiento, ya afinado
workflow_final <- final_res |>
  extract_workflow()

# creo el set de datos GIS para predecir turb
# formado a partir de: test split + datos SIN turb
fecha_turb <- read_tsv("datos/datos_nuevos.tsv") |>
  distinct(fecha) |>
  pull(fecha)

gee_new <- gee_tidy |>
  filter(fecha > max(sameep_tidy$fecha))

gee_test_new <- turb_test |>
  select(-turb) |>
  bind_rows(gee_new)

# aplico el workflow al nuevo dataset
pred_new <- predict(workflow_final, gee_test_new, type = "conf_int")
