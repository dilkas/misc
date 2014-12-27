import math

def mean(sample):
    return sum(sample) / len(sample)

def standard_deviation(sample):
    return (sum((xi - mean(sample)) ** 2 for xi in sample) / len(sample)) ** 0.5

def iqr(d):
    return q3(d) - q1(d)

def q1(d):
    d = sorted(d)
    n = len(d)
    r = n % 4
    return (d[n // 4] + d[n // 4 - 1]) / 2 if r < 2 else d[n // 4]

def q3(d):
    d = sorted(d)
    n = len(d)
    r = n % 4
    return ((d[3 * n // 4] + d[3 * n // 4 + (-1) ** (r + 1)]) / 2
            ) if r < 2 else d[3 * n // 4]

def count_outliers(d):
    counter = 0
    for x in d:
        if x < q1(d) - 1.5 * standard_deviation(d) or (
            x > q3(d) + 1.5 * standard_deviation(d)):
            counter += 1
    return counter

def absolute_to_relative(d):
    return [x / sum(d) for x in d]

def z(x, miu, sigma):
    return (x - miu) / sigma

def integral(x, accuracy):
    return sum((-1)**k * x**(2 * k + 1) / (2 * k + 1) / 2**k /
               math.factorial(k) for k in range(0, accuracy)) / math.sqrt(2 * math.pi)

def between(x1, x2, accuracy):
    return integral(x2, accuracy) - integral(x1, accuracy)

def less(x, accuracy):
    return 0.5 + integral(x, accuracy)

def more(x, accuracy):
    return 0.5 + integral(-x, accuracy)

print(less(-2.33, 100))
