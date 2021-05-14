class MinMaxStack {
    constructor(){
        this.minMaxStack =[];
        this.stack = [];
    }
        peek(){
            return this.stack[this.stack.length -1];
        }

        pop(){
            this.minMaxStack.pop();
            return this.stack.pop();
        }

         push(number){
             const newMinMax = { min:number, max:number };
             if(this.minMaxStack.length){
                 const lastMinMax = this.minMaxStack[this.minMaxStack.length-1];
                 newMinMax.min = Math.min(lastMinMax.min, number);
                 newMinMax.max = Math.max(lastMinMax.max, number);
             }
             this.minMaxStack.push(newMinMax);
             this.stack.push(number);
         }

         getMin(){
             return this.minMaxStack[this.minMaxStack.length-1].min;
         }

         getMax(){
            return this.minMaxStack[this.minMaxStack.length-1].max;
        }
};