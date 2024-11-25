import requests
import random

# The URL of your Flask app
url = "http://localhost:5000/guess"


def make_guess(guess):
    # Prepare the JSON payload
    payload = {"guess": guess}

    # Send POST request
    response = requests.post(url, json=payload)

    # Print the response
    print(f"Guess: {guess}")
    print(f"Status Code: {response.status_code}")
    print(f"Response: {response.json()}")
    print()


# Make 5 random guesses
for _ in range(12):
    # Generate a random guess between 1 and 11
    guess = random.randint(1, 11)
    make_guess(guess)

# Try an invalid guess (string instead of integer)
make_guess("invalid")

# Try an out-of-range guess
make_guess(15)
