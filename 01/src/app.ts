import { readFileSync } from "fs";

const DIGITS = {
  one: 1,
  two: 2,
  three: 3,
  four: 4,
  five: 5,
  six: 6,
  seven: 7,
  eight: 8,
  nine: 9,
};

const lines = (document: string): string[] => document.split("\r\n");

const calibrationValue = (line: string): number => {
  const chars: string[] = [...line];
  const isNum = (el) => !isNaN(Number(el));
  return Number(chars.find(isNum) + chars.findLast(isNum));
};

const sum = (numbers: number[]): number =>
  numbers.reduce((acc, el) => acc + el, 0);

const addDigitsToLine = (line: string): string =>
  Object.keys(DIGITS).reduce(
    (acc, word) => acc.replaceAll(word, word + DIGITS[word] + word),
    line
  );

// part 1
const part1 = (resourceName: string) => {
  const input = readFileSync("./resources/" + resourceName).toString();
  const input_lines = lines(input);
  const calibration_values = input_lines.map(calibrationValue);
  const calibration_values_sum = sum(calibration_values);
  return calibration_values_sum;
};
["example.txt", "input.txt"].forEach((resource) => {
  console.log("Part 1 answer for " + resource + ": " + part1(resource));
});

// part 2
const part2 = (resourceName: string) => {
  const input = readFileSync("./resources/" + resourceName).toString();
  const input_lines = lines(input);
  const lines_replaced = input_lines.map(addDigitsToLine);
  const calibration_values = lines_replaced.map(calibrationValue);
  const calibration_values_sum = sum(calibration_values);
  return calibration_values_sum;
};

["example2.txt", "input.txt"].forEach((resource) => {
  console.log("Part 2 answer for " + resource + ": " + part2(resource));
});
