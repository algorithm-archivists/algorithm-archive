function solveEuler(timeStep, n){
  const result = [];
  if (n != 0) {
    result[0] = 1;
    for (let i = 1; i < n; ++i) {
      result[i] = result[i-1] - 3 * result[i-1] * timeStep;
    }
  } 
  return result;
}

function checkResult(result, threshold, timeStep) {
  let approx = true;
  for (let i = 0; i < result.length; ++i){
    const solution = Math.exp(-3 * i * timeStep);
    if (Math.abs(result[i] - solution) > threshold) {
      console.log(result[i] + " " + solution);
      approx = false;
    }
  }
  return approx;
}

const timeStep = 0.01;
const n = 100;
const threshold = 0.01;

const result = solveEuler(timeStep, n)
const approx = checkResult(result, threshold, timeStep)

if (approx) {
  console.log("All values within threshold");
} else {
  console.log("Value(s) not in threshold");
}
