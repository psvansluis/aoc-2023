import { readFileSync } from "fs";
const [example, input] = ["example", "input"].map((file) =>
  readFileSync(`./resources/${file}.txt`).toString()
);

class Card {
  index: number;
  winning: number[];
  youHave: number[];
  constructor(line: string) {
    const [index, rest] = line.split(":");
    this.index = Number(index.substring(5));
    [this.winning, this.youHave] = rest.split("|").map((half) =>
      half
        .split(/\s+/)
        .filter((chunk) => chunk.length > 0)
        .map(Number)
    );
  }

  get intersection(): number[] {
    return this.winning.filter((n) => this.youHave.includes(n));
  }

  get worth(): number {
    return Math.floor(2 ** (this.intersection.length - 1));
  }
}

const sum = (numbers: number[]): number => numbers.reduce((a, b) => a + b, 0);

const parseInput = (input: string): Card[] =>
  input.split("\r\n").map((line) => new Card(line));

/*
 * Part 1
 */
const part1 = (input: string): number =>
  sum(parseInput(input).map((c) => c.worth));
console.log(`Part 1 answers`);
[example, input].forEach((file) => console.log(part1(file)));

/*
 * Part 2
 */
