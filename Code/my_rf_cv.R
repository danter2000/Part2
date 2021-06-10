#' Random forest cross validation
#'
#' This function returns the average misclassification rate of the penguins data mass variable.
#'
#' @param k The number of fold we wish to split our data into.
#' @keywords inference
#'
#' @return A single numeric averaged misclassification error from the number of
#'   folds \code{k}.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {

  penguins <- NewPackageLecture9::my_penguins

  penguins <- penguins %>% na.omit

  pen_num_forest <- penguins %>% dplyr::select(bill_length_mm,
                                               bill_depth_mm,
                                               flipper_length_mm,
                                               body_mass_g)

  fold_forest <- sample(rep(1:k, length = nrow(pen_num_forest)))

  mse_err <-rep(NA, k)

  for (i in 1:k) {

    x_forest_train <-  pen_num_forest[which(fold_forest != i), ]

    x_forest_test <- pen_num_forest[which(fold_forest == i), ]

    model_forest <- randomForest::randomForest(body_mass_g ~ bill_length_mm +
                                                 bill_depth_mm +
                                                 flipper_length_mm, data = x_forest_train,
                                               ntree = 100)

    mass_predict <- predict(model_forest, x_forest_test[, -4])

    mse_err[i] <- mean((mass_predict - x_forest_test$body_mass_g)^2)

  }

  pen_err <- list("Error" = mean(mse_err))

  return(pen_err)

}

globalVariables(c("bill_length_mm",
                  "bill_depth_mm",
                  "flipper_length_mm",
                  "body_mass_g"))
