// Evaluate the cumsum of an array of integers
// by Julien Heremans

int cumsum(int *numbers, int num_numbers) {
    int i;
    int sum = 0;
    for (i = 0; i < num_numbers; i++) {
        sum += numbers[i];
        numbers[i] = sum;

    }
    return 1;
}