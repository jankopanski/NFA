from random import randint, choice, choices
from string import ascii_uppercase

numberOfStates = randint(0, 20)
states = range(1, numberOfStates + 1)
print(numberOfStates)
if numberOfStates:
    print(choices(states, k=randint(0, 2 * numberOfStates)))
    print(choices(states, k=randint(0, 2 * numberOfStates)))
else:
    print([])
    print([])
numberOfTransitions = randint(0, 2 * numberOfStates ** 2)
for _ in range(numberOfTransitions):
    state = choice(states)
    characters = ''.join(choices(ascii_uppercase, k=randint(1, 2 * 26)))
    destinations = choices(states, k=randint(1, 2 * numberOfStates))
    print(state, characters, *destinations)
word = ''.join(choices(ascii_uppercase, k=randint(0, 100)))
print(word)
