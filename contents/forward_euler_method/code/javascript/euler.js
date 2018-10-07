function forwardEuler(time_step, n) {
  const arr = [1];
  for (let i = 1; i <= n; i++) {
    arr[i] = arr[i - 1] - 3 * arr[i - 1] * time_step;
  }
  return arr;
}

function checkEuler(arr, time_step, threshold) {
  const is_approx = true;
  arr.forEach(function callback(value, i) {
    const solution = Math.exp(-3 * time_step * i);

    if (Math.abs(arr[i] - solution) > threshold) {
      console.log(arr[i], solution);
      is_approx = false;
    }
  });
  return is_approx;
}

function main() {
  const time_step = 0.01;
  const threshold = 0.01;
  const n = 100;
  var euler_result = forwardEuler(time_step, n);
  var check_result = checkEuler(euler_result, time_step, threshold);
  console.log(check_result);
}

main();
