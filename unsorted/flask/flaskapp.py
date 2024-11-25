# Write a python code which creates a server which guesses a number between 1 and 11, and then responds to https requests to say if they got the guess right or wrong.
# It should be a server that can handle http requests and understand json data.

import random
from flask import Flask, request, jsonify

app = Flask(__name__)

# Generate a random number between 1 and 11
secret_number = random.randint(1, 11)


@app.route('/guess', methods=['POST'])
def guess_number():
    data = request.get_json()

    if 'guess' not in data:
        return jsonify({"error": "Please provide a 'guess' in your request"}), 400

    user_guess = data['guess']

    if not isinstance(user_guess, int) or user_guess < 1 or user_guess > 11:
        return jsonify({"error": "Guess must be an integer between 1 and 11"}), 400

    if user_guess == secret_number:
        return jsonify({"result": "Correct! You guessed the right number."}), 200
    else:
        return jsonify({"result": "Wrong guess. Try again!"}), 200


if __name__ == '__main__':
    app.run(debug=True)
