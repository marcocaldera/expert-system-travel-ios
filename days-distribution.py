class Test:
    # https://www.geeksforgeeks.org/count-ways-to-distribute-m-items-among-n-people/
    def __init__(self):
        city_count = 3
        duration = 5

        self.create_distribution(city_count=city_count, days=0, duration=duration, distribution=[])

    def create_distribution(self, city_count, days, duration, distribution):
        # sarebbe: giorni totali - giorni già disitributi in disitrbution (days) - il numero di città che ancora non hanno giorni -1
        max_alloc = duration - days - (city_count - 1)

        if city_count is 1:
            print(distribution + [max_alloc])
            return

        for a in range(1, max_alloc + 1):
            self.create_distribution(city_count=city_count - 1,
                                     days=days + a,
                                     duration=duration,
                                     distribution=distribution + [a])

    # Python code for calculating number of ways
    # to distribute m mangoes amongst n people
    # where all mangoes and people are identical

    # function used to generate binomial coefficient
    # time complexity O(m)
    @staticmethod
    def binomial_coefficient(n, m):
        res = 1

        if m > n - m:
            m = n - m

        for i in range(0, m):
            res *= (n - i)
            res /= (i + 1)

        return res

    # helper function for generating no of ways
    # to distribute m mangoes amongst n people
    def calculate_ways(self, m, n):
        # ways -> (n + m-1)C(n-1)
        ways = self.binomial_coefficient(n + m - 1, n - 1)
        return int(ways)


if __name__ == '__main__':
    Test()

    # (m+n-1)!/((n-1)!*(m)!)
    # (4 * 3 * 2) / (2 * 2) = 6
    # (5*4*3*2)/(3*2*2) = 10

    # m numero di giorni da distribuire (tolti quelli che sicuramente sono da distribuire, ovvero sicuramente uno per tappa)
    # n tra quante tappe distribuire i giorni
    # m = 2
    # n = 3
    #
    # result = Test().calculate_ways(m, n)
    # print(result)
