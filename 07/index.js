//@ts-check

const fs = require("fs");
const Game = require("./lib-07/game");
/* 07 */

const example = fs.readFileSync("./resources-07/example.txt").toString();
const input = fs.readFileSync("./resources-07/input.txt").toString();

console.log(new Game(example).totalWinnings);
console.log(new Game(input).totalWinnings);
