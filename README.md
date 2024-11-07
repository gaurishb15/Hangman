[Hangman-README.md](https://github.com/user-attachments/files/17657779/Hangman-README.md)
Hangman Game Algorithm - TrexQuant Hiring Challenge
This repository contains the solution to the Hangman game challenge, completed as part of the TrexQuant hiring process for the Global Alpha Researcher role. The objective was to design a Hangman-playing algorithm with an accuracy rate exceeding 50%.

Project Overview
The Hangman game is a word-guessing game where players try to identify a hidden word by guessing individual letters. This project implements a statistically-driven approach to predict letters in the word, leveraging probabilistic models and n-gram patterns for effective guessing.

Methodology
Intuition and Approach
The algorithm predicts letters based on patterns in English words:

Pattern-Based Guessing: Utilizes common letter combinations (e.g., "TION," "MENT") and n-grams.
Conditional Probabilities: Assigns probabilities to letters in specific contexts to prioritize likely guesses.
N-Grams: Uses sequences from 1-grams up to 5-grams, focusing on patterns with 1 or 2 blank spaces for optimal accuracy.
Steps in the Algorithm
Initialization: Represent the target word with blanks and add dummy markers for boundaries.
Guessing Letters: Using previously guessed letters, update the pattern and identify new probable letters.
Pattern Breakdown: Decompose the word into various n-gram patterns to analyze conditional probabilities.
Weight Calculation: Calculate scores for each letter based on precomputed frequencies of patterns.
Letter Selection: Choose the letter with the highest score, integrating it into the word pattern.
Iteration: Repeat the process until the word is completed or maximum incorrect guesses are reached.
Precomputed Frequencies
To streamline computation, frequencies of each n-gram pattern in the dictionary were precomputed using build_freq_tables(), enabling rapid probability calculations.

Function Descriptions
guess(word): Main function to determine the most likely letter.
find_score(word, ngram): Breaks down patterns and calculates letter probabilities.
Conditional_Prob1(pat) / Conditional_Prob2(pat): Calculates probability mass functions for patterns with one or two blank spaces.
normalize1(pat) / normalize2(pat): Normalizes frequencies to probabilities.
build_freq_tables(): Builds frequency tables for patterns in the dictionary for constant-time access.
Results
The final model achieved an accuracy rate of 66.5%, exceeding the required 50% threshold. This improvement demonstrates the effectiveness of using statistical analysis and linguistic patterns for predicting letters in the Hangman game.

Conclusion
This Hangman algorithm significantly enhances accuracy through conditional probabilities and pattern recognition, providing a robust solution to the TrexQuant hiring challenge.
