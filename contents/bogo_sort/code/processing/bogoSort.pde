int[] test_array = new int[5];

void setup() {
  size(400,400);
  stroke(255);
  fill(0);
  
  randomArray(test_array,0,100);
}

void draw(){
  background(255);
  
  int w = width/test_array.length , minimum = min(test_array) , maximum = max(test_array);
  for(int i=0;i<test_array.length;i++){
    rect(w*i,height,w,-map(test_array[i],minimum,maximum,5,height-5));
  }
  
  if (isSorted(test_array)){
    noLoop();
    println("Finished in "+ millis()/1000 +" seconds.");
  }else{
    randomize(test_array);
  }
}


void randomArray(int[] array , int min , int max){
  for(int i=0;i<array.length;i++){
    array[i] = (int) random(min,max);
  }
}

void swap(int[] array, int i, int j) {
  int temp = array[i];
  array[i] = array[j];
  array[j] = temp;
}


void randomize(int[] array) {
  for (int i = array.length-1; i>=1 ; i--) {
    int j = (int) random(0, i);
    swap(array,i,j);
  }
}

Boolean isSorted(int[] array){
  for(int i=0;i<array.length-1;i++){
    if (array[i]>array[i+1]) return false;
  }
  return true;
}

void bogoSort(int[] array){
  while (! isSorted(array)){
    randomize(array);
  }
}
