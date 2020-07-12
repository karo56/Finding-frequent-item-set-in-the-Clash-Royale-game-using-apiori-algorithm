# Finding-frequent-item-set-in-the-Clash-Royale-game-using-apiori-algorithm



Records in this data set describe decks of cards used in a popular collectible card video game Clash Royale. These decks were obtained using RoyaleAPI.com service, from games which took place in January 2019. Each record consists of five values:

-a timestamp of the game (column timestamp)
-arena ID (column arena_id – higher the arena, more skilled/experienced a player is)
-outcome of a game (column has_won, 1 – the player won, 0 the player lost)
-a player ID (column tag)
-a list of exactly eight cards in the player’s deck separated by “_” signs (column player_deck) 

Dataset contains 51 million records.

My code analyzes the set, i.a. finding interesting relationships between the cards, card combos which have high win-rates or the popularity of cards over time.I mainly used the apiori algorithm for analysis.
