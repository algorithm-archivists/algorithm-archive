function forward_euler(time_step, n) {
    var arr = [];
    arr[0] = 1;
    for (var i = 1; i <= n; i++) {
        arr[i] = arr[i - 1] - 3 * arr[i - 1] * time_step;
    }
    return arr;
}

function check_euler(arr, time_step, threshold) {
    var is_approx = true;
    for (var i = 1; i <= arr.length; i++) {
        var solution = Math.exp(-3 * time_step * i);

        if (Math.abs(arr[i] - solution) > threshold) {
            alert(arr[i], solution);
            is_approx = false;
        }
    }
    return is_approx;
}

function main() {
     time_step = 0.01;
     threshold = 0.01;
     n = 100;
    var euler_result = forward_euler(time_step, n);
    var check_result = check_euler(euler_result, time_step, threshold);
    alert(check_result);
}

main();
