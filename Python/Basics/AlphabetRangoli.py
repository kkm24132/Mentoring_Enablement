# Creation of alphabet Rangoli method

#!/usr/bin/env python
import string

# Get user input and set mid_position as n-1 if user input is n
n = int(input())
mid_position = n - 1

# Build the lines above the mid position
for i in range(n - 1, 0, -1):
    row = ['-'] * (2 * n - 1)
    for j in range(n - i):
        row[mid_position - j] = row[mid_position + j] = string.ascii_lowercase[j + i]
    print('-'.join(row))

# Build the mid line and all lines below it
for i in range(0, n):
    row = ['-'] * (2 * n - 1)
    for j in range(0, n - i):
        row[mid_position - j] = row[mid_position + j] = string.ascii_lowercase[j + i]
    print('-'.join(row))
