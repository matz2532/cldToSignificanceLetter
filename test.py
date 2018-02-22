import pandas as pd 
import math

df = pd.read_csv('input.csv', index_col=0)
LETTERS = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']



def extr(df):
    """
        Do some magic stuff
    """
    return df

def cld_summary(df, x):
    """
        Do some more magic stuff
    """
    insert_absorb(data=df, logicals=x)

    pass


def insert_absorb(data, logicals, letters=LETTERS):
    """
        Even more magic code
    """
    mask = data[logicals]

    if not any(data['reject H0']):
        groups = get_letters(1)
        data[groups[0]] = 1
    else:
        comps = data[['product_1', 'product_2']][mask]
        print(comps)
        print("There are differences")
    pass


def get_letters(n, letters=LETTERS, sep='.'):
    """
        Put a sensiible docstring here
    """
    complete = math.floor(n/len(letters))
    partial = n % len(letters)
    separ = ''
    lett = []

    if complete > 0:
        for i in range(0, complete):
            lett.extend([separ + letter for letter in letters])
            separ = separ + sep

    if partial > 0:
        lett.extend([separ + letter for letter in letters[0:partial]])

    return lett


if __name__ == "__main__":
    cld_summary(df, 'reject H0')
    #get_letters(10, letters=LETTERS[0:4])
