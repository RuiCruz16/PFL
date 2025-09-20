# Functional and Logic Programming

This repository gathers the projects, exercises, and study materials developed within the course **Functional and Logic Programming (PFL)**, taught in the **first semester of the third year** of the degree. The purpose of this collection of work is to apply and consolidate the fundamental concepts of the two main paradigms covered in the course: **functional programming in Haskell** and **logic programming in Prolog**.  

## Project 1 – Haskell  

The first project was developed in **Haskell** and focuses on solving classical **graph problems**:  

- **Shortest Path Problem**: an algorithm was implemented to explore all possible routes between a source city and a destination city, returning not just one but all existing shortest paths. This approach avoids purely greedy solutions and ensures completeness, correctly identifying multiple optimal solutions whenever they exist.  

- **Traveling Salesman Problem (TSP)**: a solution was implemented using **dynamic programming with bitmasking**, allowing for the efficient computation of the minimum path that visits all cities exactly once and returns to the starting city.  

In addition to these two main problems, the project also includes several **auxiliary functions** essential for graph manipulation and analysis:  
- verification of graph connectivity,  
- calculation of path distances,  
- identification of the most connected cities,  
- and test cases with different predefined roadmaps.  

With these tools, the project provides a solid foundation for exploring **graph optimization problems**, approaching classical challenges from graph theory through functional programming.  

## Project 2 – Prolog  

The second project was developed in **Prolog** and consists of the implementation of the game **Blackstone**, a two-player board game. This game was enriched with several functionalities and variations:  

- **Support for different board sizes** (6x6, 8x8, and 10x10).  
- **Multiple interaction modes**: player vs player, player vs computer, and computer vs computer.  
- **Different difficulty levels**, ranging from random moves to a **Greedy algorithm** approach, capable of evaluating and prioritizing moves by maximizing the computer’s strategic options while limiting those of the opponent.  
- **Implementation of different rule variants**: Standard, Medium Churn, and High Churn.  

The development required the **complete modeling of the game’s internal state**, the definition of the rules of movement and capture, as well as **user interaction**, practically consolidating the principles of declarative programming in Prolog.  

## Exercises and Exams  

In addition to the two main projects, the repository also contains:  

- a collection of **practical exercises** solved throughout the semester, both in Haskell and Prolog, covering the fundamental topics of the course;  
- a set of **solved past exams**, used as additional practice material to consolidate learning and effectively prepare for the course assessment.  

## How to Run  

### Project 1 – Haskell  
1. Make sure you have **GHC** or **GHCi** installed.  
2. Load the project file into GHCi:  
   ```bash
   ghci TP1.hs
   ```  
3. Test the functions with the provided example graphs (`gTest1`, `gTest2`, `gTest3`). For example:  
   ```haskell
   shortestPath gTest1 "0" "5"
   travelSales gTest2
   ```  

### Project 2 – Prolog  
1. Make sure you have **SICStus Prolog** installed.  
2. Open the Prolog console inside the `src` folder.  
3. Load the game file:  
   ```prolog
   game.pl
   ```  
4. Start the game by calling:  
   ```prolog
   play.
   ```  
5. Choose the desired game mode, board size, and variant according to the menu options.  
