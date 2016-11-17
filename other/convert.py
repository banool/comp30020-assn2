#!/usr/local/bin/python3

num = 1

puzzleFName = "samples/puzzle" + str(num)
output = "samples/converted" + str(num)


with open(puzzleFName, "r") as f:
    content = f.read().splitlines()

print("{}x{}".format(len(content), len(content[0])))

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

i = 0
converted = []
for line in content:
    new = []
    for j in range(0, len(line)):
        if line[j] == "_":
            new.append(alphabet[i] + alphabet[j])
        else:
            new.append(line[j])
    converted.append(new)
    i += 1

print(converted)