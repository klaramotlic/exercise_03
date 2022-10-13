array <- c(2,7,1,46,8)
IndexOfMin <- function(array,first,last){
  index <- first
  for (k in (first+1):last){
    if (array[k] < array[index]){
      index <- k
    }
  }
print(index)
}

IndexOfMin(array,1,5)


SelectionSort <- function(a, n){
  for (i in 1:(n-1)){
    j <- IndexOfMin(a, i, n)
    a[i] <- a[j] + a[i]
    a[j] <- a[i] - a[j]
    a[i] <- a[i] - a[j]
  }
print(a)
}

a <- c(4,8,6,10,1,2)
SelectionSort(a,6)

RecursiveSelectionSort<- function(a, first, last){
  if (first < last){
    index <- IndexOfMin(a, first, last)
    a[first] <- a[index] + a[first]
    a[index] <- a[first] - a[index]
    a[first] <- a[first] - a[index]
    a <- RecursiveSelectionSort(a, first + 1, last)
  }
  
return(a)
}
RecursiveSelectionSort(a, 1,6)


