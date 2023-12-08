const JackHand = require("./jackHand");
const JokerHand = require("./jokerHand");

class Game {
  /**
   * @param {string} input
   * @param {string} variant
   */
  constructor(input, variant) {
    const split = input.split("\r\n");
    this.lines = split
      .map((line) => new Line(line, variant))
      .sort((a, b) => a.hand.value - b.hand.value);
  }

  /** @type{Line[]} */ lines;

  /**
   * @returns {number}
   */
  get totalWinnings() {
    return this.lines
      .map((line, index) => (index + 1) * line.bid)
      .reduce((a, b) => a + b, 0);
  }
}

class Line {
  index;
  hand;
  bid;

  constructor(stringLine, variant) {
    const [fst, snd] = stringLine.split(" ");
    this.hand = variant === "jack" ? new JackHand(fst) : new JokerHand(fst);
    this.bid = Number(snd);
  }
}

module.exports = Game;
