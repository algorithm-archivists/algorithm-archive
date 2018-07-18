import math


def forward_euler(time_step, n):
    result = [0] * n
    result[0] = 1
    for i in range(1, n):
        result[i] = result[i - 1] - 3 * result[i - 1] * time_step
    return result


def check(result, threshold, time_step):
    approx = True
    for i in range(len(result)):
        solution = math.exp(-3 * i * time_step)
        if abs(result[i] - solution) > threshold:
            print(result[i], solution)
            approx = False
    return approx


def main():
    time_step = 0.01
    n = 100
    threshold = 0.01

    result = forward_euler(time_step, n)
    approx = check(result, threshold, time_step)
    print("All values within threshold") if approx else print("Value(s) not in threshold")

main()
