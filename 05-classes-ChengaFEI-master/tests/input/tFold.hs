foldr =
  let foldr' f b xs = if xs == [] 
                        then b 
                        else let h = head xs in 
                             let t = tail xs in 
                               f h (foldr' f b t) 
  in 
     foldr'

,

add = 
  let add' x y = x + y in 
      add'

,

myList =   
  [1, 2, 3, 4, 5]


