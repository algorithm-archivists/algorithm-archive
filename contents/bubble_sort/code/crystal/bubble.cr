def bubble_sort(arr)
  arr = arr.dup 
  (0 ... arr.size).each do 
    (0 ... arr.size-1).each do |k| 
      if arr[k] > arr[k+1]
        arr[k+1],arr[k] = arr[k],arr[k+1]
      end
    end
  end 
  arr
end

def main 
  number = 10.times.map{rand(0..1_000)}.to_a 
  pp "The array before sorting is #{number}"
  number = bubble_sort number 
  pp "The array after  sorting is #{number}"
end 

main
