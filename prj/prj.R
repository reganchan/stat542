library(magrittr)

setwd("~/prj")
assert_libraries <- function(libraries) {
  libraries <- strsplit(libraries, ";")[[1]]
  assertthat::assert_that(all(libraries %in% installed.packages()[,1]))
}
assert_libraries("dplyr;functional;jsonlite;textclean;gridExtra;tidytext;text2vec;glmnet;quanteda;randomForest")

coalesce <- function(v) {
  v[is.na(v)] <- ""
  return(v)
}

load_json_data <- function(file_name) {
  json <- jsonlite::read_json('winemag-data_first150k.json', simplifyVector = TRUE) 
  to_factor <- functional::Compose(as.character, coalesce, as.factor)
  col_func <- c(as.numeric, as.character, as.numeric, rep(c(to_factor), 7))
  for(colnum in 1:ncol(json)) {
    json[, colnum] <- col_func[[colnum]]( json[, colnum] )
  }
  
  return(json[, c(1:5, 9:10)])
}

json <- load_json_data("winemag-data_first150k.json")

n <- nrow(json)
set.seed(0)
rows.train <- sample(1:n, n * 0.2)

vars.train <- model.matrix(~designation+variety+country+winery, json[rows.train, ])
data.train <- json %>%
  quanteda::tokens(remove_numbers=TRUE, remove_punct=TRUE, remove_punct=TRUE, remove_separators=TRUE, remove=stop_words) %>%
  quanteda::dfm(verbose=TRUE) %>%
  quanteda::dfm_trim(min_termfreq=10) %>%
  quanteda::convert(to="data.frame") %>%
  cbind(vars.train) %>%
  as.matrix()

y.train <- json$points[rows.train]
glmnet_cv <- glmnet::cv.glmnet(x=vars.train, y=y.train, family='gaussian', alpha=1)
rf_model <- randomForest::randomForest(x=data.train, y=y.train)

# 
# fruity_pinot_noir <- quanteda::kwic(corp, quanteda::phrase("fruity pinot noir"))

text2vec_code <- function(json) {
  clean_description <- function(description) {
    description %>%
      textclean::replace_contraction() %>%
      textclean::replace_non_ascii() #%>%
  }
  
  description <- clean_description(json$description)
  n <- nrow(json)
  set.seed(0)
  rows.train <- sample(1:n, n * 0.9)
  json.train <- json[rows.train,]
  json.test  <- json[-rows.train,]
  
  it.train <- text2vec::itoken(json.train$description,
                               preprocessor = tolower, 
                               tokenizer = text2vec::word_tokenizer,
                               progressbar = FALSE)
  
  x.docvars <- within(json.train, rm(description))
  
  dtm.train <- it.train %>%
    text2vec::create_vocabulary() %>%
    text2vec::vocab_vectorizer() %>%
    text2vec::create_dtm(it.train, .)
  x.train <- cbind(convert(dtm.train, to="data.frame"), x.docvars)
  
  NFOLDS <- 4
  t1 <- Sys.time
  glmnet_classifier <- glmnet::cv.glmnet(x = dtm.train, y = json.train$points, 
                                         family = 'gaussian', 
                                         # L1 penalty
                                         alpha = 1,
                                         # interested in the area under ROC curve
                                         type.measure = "auc",
                                         # 5-fold cross-validation
                                         nfolds = NFOLDS,
                                         # high value is less accurate, but has faster training
                                         thresh = 1e-3,
                                         # again lower number of iterations for faster training
                                         maxit = 1e3)
  print(difftime(Sys.time(), t1, units = 'sec'))
}

