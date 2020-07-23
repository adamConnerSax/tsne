from sklearn.datasets import load_digits

output_file = 'scikit_learn_digits_data.txt'

print('loading digits...')
digits = load_digits()

print('writing to {0}...'.format(output_file))
with open(output_file, 'w') as f:
    for d in digits.data:
        f.write(str(list(d)))
        f.write('\n')

print('done.')