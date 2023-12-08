//@ts-check

const fs = require("fs");
const Game = require("./lib-07/game");
/* 07 */

const example = fs.readFileSync("./resources-07/example.txt").toString();
const input = fs.readFileSync("./resources-07/input.txt").toString();

// part 1
console.log(new Game(example, "jack").totalWinnings);
console.log(new Game(input, "jack").totalWinnings);

// part 2
console.log(new Game(example, "joker").totalWinnings);
console.log(new Game(input, "joker").totalWinnings);
