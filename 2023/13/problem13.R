library(tidyverse)

input <- readr::read_file("13/input") %>% str_remove("\\r\\n$")
d <- str_split(input, "\\r\\n\\r\\n") |>
  map(~str_split(.x, "\\r\\n"))
d <- d[[1]]


check_reflection <- function(xx, dup_ind, type) {
  split1 <- xx[1:(dup_ind-1)]
  split2 <- xx[dup_ind:length(xx)]
  l1 <- length(split1)
  l2 <- length(split2)

  if (l1>l2) {
    .diff <- l1-l2
    split1 <- split1[(1+.diff):l1]
  }
  if (l2>l1) {
    split2 <- split2[1:l1]
  }

  isreflection <- all(split1==rev(split2))

  if (isreflection) {
    print(str_glue("Is reflection across {type}, count = {dup_ind-1}"))
    if (type == "cols") return(dup_ind-1)
    if (type == "rows") return((dup_ind-1)*100)
  }
}

get_count <- function(.X, type = c("cols", "rows")) {
  type <- match.arg(type)
  if (type == "cols") xx <- colSums(.X)
  if (type == "rows") xx <- rowSums(.X)

  # index of first occurence of repeated number
  # dup_ind <- first(which(c(FALSE, diff(xx)==0)))

  dup_inds <- which(c(FALSE, diff(xx)==0))

  # if no repeating number detected at all
  if (length(dup_inds)==0) return(NULL)

  valid_inds <- c()
  for (j in dup_inds) {
    print(j)
    # check if vector before is equal!!!
    if (type == "cols") {
      if (!all(X[,j-1]==X[,j])) next
    }
    if (type == "rows") {
      if (!all(X[j-1,]==X[j,])) next
    }
    valid_inds <- c(valid_inds, j)
  }

  if (is.null(valid_inds)) {
    print("No valid inds")
    return(NULL)
  } else {
    res <- map(1:length(valid_inds),
               ~check_reflection(xx, valid_inds[.x], type = type)
    ) %>% unlist()

    return(res)
  }
}


length(d)
i <- 3
resvec <- c()
for (i in seq_along(d)) {
  x <- d[[i]] |> str_split("")
  X <- unlist(x) |>
    matrix(nrow=length(x), ncol=length(x[[1]]), byrow = T)

  X[X=="."] <- 0
  X[X=="#"] <- 1
  X <- matrix(as.numeric(X), ncol=length(x[[1]]))

  cc <- X %>% get_count("cols")
  rr <-  X %>% get_count("rows")
  if (!is.null(cc) & !is.null(rr)) warning(str_glue("Both are not null {i}"))
  if (is.null(cc) & is.null(rr)) warning(str_glue("Both are null {i}"))

  resvec <- c(resvec, cc, rr)
}

sum(resvec)
# 33356

# test
x <- d[[1]] |> str_split("")
X <- unlist(x) |>
  matrix(nrow=length(x), ncol=length(x[[1]]), byrow = T)

X[X=="."] <- 0
X[X=="#"] <- 1
X <- matrix(as.numeric(X), ncol=length(x[[1]]))



X %>% get_count("cols")
X %>% get_count("rows")



#

xx <- colSums(X)
xx <- rowSums(X)
# 4 2 5 2 3 3 2 5 2
# splitta mellan 3 och 3 och fortsätt splitta
# om alla har en match är odet ok
sort(xx)

dup_inds <- which(c(FALSE, diff(xx)==0))

# if (first(dup_inds) == 2) {
#   # first ind cannot be reflection
#   if (length(dup_inds) > 1) {
#     dup_ind <- dup_inds[2]
#   } else {
#     return(NULL)
#   }
#
# } else {
#   dup_ind <- first(dup_inds)
# }

dup_inds <- c(3,44,46,45,6)
type <- "rows"
valid_inds <- c()
for (j in dup_inds) {
  # check if vector before is equal!!!
  # check if vector before is equal!!!
  if (type == "cols") {
    if (!all(X[,j-1]==X[,j])) next
  }
  if (type == "rows") {
    if (!all(X[j-1,]==X[j,])) next
  }
  valid_inds <- c(valid_inds, j)
}

if (is.null(valid_inds)) {
  print("No valid inds")
  return(NULL)
} else {
  res <- map(1:length(valid_inds),
      ~check_reflection(xx, valid_inds[.x], "rows")
  ) %>% unlist()

  return(res)
}

valid_inds




# index of first occurence of repeated number


dup_inds %>% get_num_rows_before(type = type) %>% max()

map(dup_inds, ~ get_num_rows_before(.x, type="rows")) %>% unlist() %>% max()
