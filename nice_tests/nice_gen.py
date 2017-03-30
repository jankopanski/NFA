from random import choices, randint, sample
from string import ascii_uppercase

numberOfStates = randint(0, 20)
states = range(1, numberOfStates + 1)
print(numberOfStates)
if numberOfStates:
    print(sample(states, randint(0, numberOfStates)))
    print(sample(states, randint(0, numberOfStates)))
else:
    print([])
    print([])
numberOfTransitions = randint(0, 2 * numberOfStates ** 2)
for state in states:
    characters = ''.join(sample(ascii_uppercase, k=randint(0, len(ascii_uppercase))))
    destinations = sample(states, k=randint(0, numberOfStates))
    if characters and destinations:
        print(state, characters, *destinations)
word = ''.join(choices(ascii_uppercase, k=randint(0, 100)))
print(word)
