prep_files = ['test_file1.tsv', 'test_file2.tsv', 'test_file3.tsv', 'test_file4.tsv', 'test_file5.tsv']


with open('en_features.tsv', 'a') as overal_file:
    for file in prep_files:
        with open(file, 'r') as f:
            lines = f.readlines()
            for line in lines:
                overal_file.write(line)
