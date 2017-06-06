# haskell61850

A Haskell wrapper for libiec61850

## Using this libary

Add the following lines to your stack.yaml

    extra-include-dirs:
    - /usr/local/include/libiec61850

    extra-lib-dirs:
    - /usr/local/lib

    packages:
    - '.'
    - location:
        git: https://github.com/tpasternak/iechaskell.git
        commit: <commit-id-here>
      extra-dep: true
