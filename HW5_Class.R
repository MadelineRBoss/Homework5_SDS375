## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

#-------------------------------------------------------------------------------

#set validation
setValidity(Class = "sparse_numeric",
            method = function(object){
              if(length(object@value) != length(object@pos))
                return("Amount of values and positions are unequal")
              if(length(object@pos) > object@length)
                return("Too many values for length of sprase vector")
              if(any(object@pos > object@length))
                return("A number has a greater position value than the actual space")
              if(any(object@pos < 1))
                return("A number has an invalid position")
              if (length(object@value) == 0 || any(is.na(object@value))) {
                return("Slot 'value' is empty or contains NA values")
              }
              TRUE
            })
#-------------------------------------------------------------------------------

setAs(from = "numeric", 
      to = "sparse_numeric",
      def = function(from) {
        pos <- which(from != 0)
        value <- from[pos]            # keep as numeric
        vec_length <- as.integer(length(from))
        new("sparse_numeric", value = value, pos = pos, length = vec_length)
      })

setAs(from = "sparse_numeric",
      to = "numeric",
      def = function(from){
        value <- from@value
        pos   <- from@pos
        len   <- from@length
        
        # initialize vector of zeros
        vec <- numeric(len)
        
        # fill in non-zero values
        for (i in seq_along(value)){
          vec[pos[i]] <- value[i]
        }
        
        vec   # return numeric vector
      })



#-------------------------------------------------------------------------------

# Define Generic for parse_add
setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))

setMethod("sparse_add", 
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            
            double_pos <- intersect(x@pos, y@pos)
            
            x_vals_match <- x@value[x@pos %in% double_pos]
            y_vals_match <- y@value[y@pos %in% double_pos]
            
            x_values_no_match <- x@value[!(x@pos %in% double_pos)]
            y_values_no_match <- y@value[!(y@pos %in% double_pos)]
            x_pos_no_match <- x@pos[!(x@pos %in% double_pos)]
            y_pos_no_match <- y@pos[!(y@pos %in% double_pos)]
            
            add_vals <- x_vals_match + y_vals_match
            add_vals <- c(add_vals, x_values_no_match, y_values_no_match)
            new_pos <- c(double_pos, x_pos_no_match, y_pos_no_match)
            
            new_sparse <- new("sparse_numeric",
                              value = add_vals,
                              pos = new_pos,
                              length = max(x@length, y@length))
            sort(new_sparse)
          })
#-------------------------------------------------------------------------------

# Define Generic for parse_mult
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

# Define Method for parse_mult
setMethod("sparse_mult", 
          signature(x = "sparse_numeric", y = "sparse_numeric"),
            function(x, y){
            double_pos <- intersect(x@pos, y@pos)
            
            # Find matching indices in each object
            x_vals <- x@value[x@pos %in% double_pos]
            y_vals <- y@value[y@pos %in% double_pos]
            
            # Multiply the values where positions match
            prod_vals <- x_vals * y_vals
            
            # Construct new sparse_numeric
            new("sparse_numeric", 
                value = prod_vals,
                pos = double_pos,
                length = max(x@length, y@length))
          })

#-------------------------------------------------------------------------------

# Define Generic for parse_add
setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))

# Define Method for parse_add
setMethod("sparse_sub", 
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y){
            double_pos <- intersect(x@pos, y@pos)
            
            # Find matching indices in each object
            x_vals_match <- x@value[x@pos %in% double_pos]
            y_vals_match <- y@value[y@pos %in% double_pos]
            
            # Find non matching indices
            x_values_no_match = x@value[!(x@pos %in% double_pos)]
            y_values_no_match = y@value[!(y@pos %in% double_pos)] * -1
            x_pos_no_match = x@pos[!(x@pos %in% double_pos)]
            y_pos_no_match = y@pos[!(y@pos %in% double_pos)] 
            
            # dd the values where positions match
            sub_vals <- x_vals_match - y_vals_match
            invalid_vals = c()
            for (val in 1:length(sub_vals)){
              if (sub_vals[val] == 0){
                invalid_vals = append(invalid_vals, val)
              }
                
            }
            sub_vals = sub_vals[-invalid_vals]
            double_pos = double_pos[-invalid_vals]
            
            #append values that don't have matches
            sub_vals <- c(sub_vals, x_values_no_match, y_values_no_match)
            new_pos <- c(double_pos, x_pos_no_match, y_pos_no_match)
            
            
            # Construct new sparse_numeric
            new("sparse_numeric", 
                value = sub_vals,
                pos = new_pos,
                length = max(x@length, y@length))
          })

#-------------------------------------------------------------------------------

# Define Generic for sparse_crossprod
setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

# Define Method for sparse_crossprod
setMethod("sparse_crossprod", 
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x,y){
            
            double_pos <- intersect(x@pos, y@pos)
            
            # Find matching indices in each object
            x_vals <- x@value[x@pos %in% double_pos]
            y_vals <- y@value[y@pos %in% double_pos]
            
            # Multiply the values where positions match
            prod_vals <- x_vals * y_vals
            
            final_value <- 0
            
            for (v in prod_vals){
              final_value = final_value + v
            }
            
            # Construct new sparse_numeric
            new("numeric", c(final_value))
          })

#-------------------------------------------------------------------------------
#make alts
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

#-------------------------------------------------------------------------------
setMethod("show", "sparse_numeric", 
          function(object){
            cat("The sparse vector of size", object@length, "has the following values: \n")
            for(v in 1:length(object@value)){
              cat("position:", object@pos[v], 
                  ", value:", object@value[v])
              
            }
          })

#-------------------------------------------------------------------------------
setMethod("plot", 
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y) {
            
            double_pos <- intersect(x@pos, y@pos)
            plot_value = c()
            for(l in 1:object@length){
              if (l %in% double_pos){
                plot_value = c(plot_value, 1)
              }
              else{
                plot_value = c(plot_value, 0)
              }
            }
            plot(x = 1:object@length, y = plot_value)
          })

#-------------------------------------------------------------------------------
#Make custom method
setMethod("sort", signature(x = "sparse_numeric"),
          function(x, decreasing = FALSE, ...) {
            ord <- order(x@pos, decreasing = decreasing)
            
            new("sparse_numeric",
                value = x@value[ord],
                pos = x@pos[ord],
                length = x@length)
          })

setGeneric("sparse_sum", function(object) standardGeneric("sparse_sum"))
setMethod("sparse_sum", "sparse_numeric",
          function(object){
            sum = 0
            for(v in object@value){
              sum = sum + v
            }
            sum
          })