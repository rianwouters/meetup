# Programming in Haskell Study Group

## Why study Haskell

- Become a better programmer. 
    - Even if you don't use Haskell in production.
- Understand the FP constructs supported in you favorite languages.
    - Make better use of them.
- Functional programming is eating the world. 
    - It is better to be prepared.

## About this study group

- Distributed study group.
- Follows the contents of the
book
[Functional Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html).

## Basic way of working

- No master classes.
- Self-paced.
- Follow the [slides](http://www.cs.nott.ac.uk/~pszgmh/Slides.zip).
- Make the exercises:
    - In your own time, at your own pace.
    - In person meetings.
- Discuss/make questions during:
    - In person meetings.
    - [Gitter channel](https://gitter.im/EindhovenHaskellMeetup/course-programming-in-haskell).

## Roadmap

- Part I and II of the book.
- One chapter a week.
- Part III: real-world Haskell applications.

## Contact

- carlos.rodriguez.vega.c@philips.com
- damian.nadales@gmail.com

# Tooling

## Stack

- Install [stack](https://docs.haskellstack.org/en/stable/README/).
- Clone the exercises:
```sh
git clone https://github.com/EindhovenHaskellMeetup/meetup.git
```
- Go to the exercises folder:
```sh
cd courses/programming-in-haskell/pih-exercises
```

- Setup stack:
```sh
stack setup
```
- Run the tests for the exercises!
```sh
stack test
```

## Visual Studio Code

- [Visual Studio Code](https://code.visualstudio.com/download).
- [Plugin](https://marketplace.visualstudio.com/items?itemName=Vans.haskero).
  To use intero, go to the exercises folder (`pih-exercises`) and run:
  ```sh
  stack build intero
  ```
  
# Additional information

## On Haskell

- [Learn you a Haskell](http://learnyouahaskell.com/).
- [Lectures CIS194](http://www.cis.upenn.edu/~cis194/spring13/lectures.html)
- [Haskell is Not For Production and Other Tales](https://youtu.be/mlTO510zO78).
- [A History of Haskell: Being Lazy with Class](https://youtu.be/06x8Wf2r2Mc).

## Using stack

- [Meetup presentation](https://github.com/EindhovenHaskellMeetup/meetup/blob/master/presentations/March-30-2017/getting-started-with-haskell/getting-started-with-haskell.md).

## Generating these slides

See [this README](https://github.com/EindhovenHaskellMeetup/meetup/tree/master/presentations/March-30-2017/getting-started-with-haskell).
