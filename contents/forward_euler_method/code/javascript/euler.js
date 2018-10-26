function forwardEuler(timeStep, n) {
  const arr = [1];
  for (let i = 1; i <= n; i++) {
    arr[i] = arr[i - 1] - 3 * arr[i - 1] * timeStep;
  }
  return arr;
}

function checkEuler(arr, timeStep, threshold) {
  let isApprox = true;
  arr.forEach((_value, i) => {
    const solution = Math.exp(-3 * timeStep * i);

    if (Math.abs(arr[i] - solution) > threshold) {
      console.log(arr[i], solution);
      isApprox = false;
    }
  });
  return isApprox;
}

function main() {
  const timeStep = 0.01;
  const threshold = 0.01;
  const n = 100;
  const eulerResult = forwardEuler(timeStep, n);
  const checkResult = checkEuler(eulerResult, timeStep, threshold);
  console.log(checkResult);
}

main();
