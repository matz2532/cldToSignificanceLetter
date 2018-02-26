import pandas as pd 
import numpy as np
import math

df = pd.read_csv('input.csv', index_col=0)
LETTERS = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 
           'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']



def cld_summary(df, x):
    """
        Do some more magic stuff
    """
    df = insert(data=df)
    dups = absorb(df=df)
    if len(dups) > 0:
        df = df.drop(dups, axis=1)
    else:
        pass

    pass


def insert(data):
    """
        Insert phase of algortihm.  Duplicate columns for significant differences and then swap boolean values around
    """
    unique = list(set(list(data['product_1'])))
    initial_col = [1] * len(unique)
    df = pd.DataFrame(initial_col, columns=['initial_col'])
    df['product'] = unique
    df = df.set_index('product')

    # get the significant differences.
    signifs = data[data['reject H0'] == True]

    if not any(data['reject H0']):  # case wheres theres no significant differences we end up with one group.
        groups = get_letters(1)
        df[groups[0]] = 1
        df = df.drop(labels='initial_col', axis=1)
        return df
    else:           # every other case
        #do the insert
        print('Inserting new columns')
        for i, signif in enumerate(signifs['product_1']):
            signif_1 = signif
            signif_2 = signifs['product_2'].iloc[i]

            # get the columns to duplicate from df by testing that signif_1 row and signif_2 row are both equal to 1
            test = df.loc[[signif_1, signif_2]]
            cols_to_dup = []
            for col in test.columns:
                if test[col].sum() == 2:
                    cols_to_dup.append(col)
                else:
                    continue

            # duplicate the dataframes
            df1 = df[cols_to_dup]
            df2 = df[cols_to_dup]
            df3 = df[df.columns].drop(labels=cols_to_dup, axis=1)

            #set the 1's and 0's
            df1.loc[signif_1] = 1
            df1.loc[signif_2] = 0

            df2.loc[signif_1] = 0
            df2.loc[signif_2] = 1


            #merge the duplicate dataframes back to the original with any unduped columns
            df = df1.merge(df2, left_index=True, right_index=True)
            df = df.merge(df3, left_index=True, right_index=True)
            letters = get_letters(n=len(df.columns))
            df.columns = letters
            print(df)
            new_cols = []  #  list of the new columns to check for duplication.

    return df


def absorb(df):
    """
        Check each new column against all old columns.  If its a duplicate we can drop it i.e. "absorb" it.
    """
    groups = df.columns.to_series().groupby(df.dtypes).groups
    print(groups)
    dups = []
    for t, v in groups.items():
        dcols = df[v].to_dict(orient="list")

        vs = list(dcols.values())
        ks = list(dcols.keys())
        lvs = len(vs)

        for i in range(lvs):
            for j in range(i+1,lvs):
                if vs[i] == vs[j]: 
                    dups.append(ks[i])
                    break
    print(dups)
    return dups  


def sweep():
    """
        Implement the 'sweep' method to reduce number of 1's
    """


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
