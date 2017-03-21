general-games
=============

[![Stackage LTS version](https://www.stackage.org/package/general-games/badge/lts)](https://www.stackage.org/package/general-games) [![Stackage nightly version](https://www.stackage.org/package/general-games/badge/nightly)](https://www.stackage.org/package/general-games) [![Hackage version](https://img.shields.io/hackage/v/general-games.svg?label=Hackage)](https://hackage.haskell.org/package/general-games) [![Build Status](https://travis-ci.org/cgorski/general-games.svg?branch=master)](https://travis-ci.org/cgorski/general-games)

* [Description](#description)
* [Getting Started](#getting-started)
* [Contribute](#contribute)
* [License](#license)

## Description

Library providing framework for simulating outcomes of a variety of games, including Poker.

## Getting Started
Suppose we want a full deck of standard playing cards:

```haskell
deck = fullDeck :: [PlayingCard]
deck
>>> [Ace of Clubs,Two of Clubs,Three of Clubs,Four of Clubs,Five of Clubs,Six of Clubs,Seven of Clubs,Eight of Clubs,Nine of Clubs,Ten of Clubs,Jack of Clubs,Queen of Clubs,King of Clubs,Ace of Diamonds,Two of Diamonds,Three of Diamonds,Four of Diamonds,Five of Diamonds,Six of Diamonds,Seven of Diamonds,Eight of Diamonds,Nine of Diamonds,Ten of Diamonds,Jack of Diamonds,Queen of Diamonds,King of Diamonds,Ace of Hearts,Two of Hearts,Three of Hearts,Four of Hearts,Five of Hearts,Six of Hearts,Seven of Hearts,Eight of Hearts,Nine of Hearts,Ten of Hearts,Jack of Hearts,Queen of Hearts,King of Hearts,Ace of Spades,Two of Spades,Three of Spades,Four of Spades,Five of Spades,Six of Spades,Seven of Spades,Eight of Spades,Nine of Spades,Ten of Spades,Jack of Spades,Queen of Spades,King of Spades]
```


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/cgorski/general-games/issues).


## License

`general-games` is released under the [MIT License](https://opensource.org/licenses/MIT), and Copyright 2017 [Christopher A. Gorski](https://www.cgorski.org/).

