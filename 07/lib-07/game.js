const Hand = require("./hand");

class Game {
  /**
   * @param {string} input
   */
  constructor(input) {
    const split = input.split("\r\n");
    this.lines = split
      .map((line) => new Line(line))
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

  constructor(stringLine) {
    const [fst, snd] = stringLine.split(" ");
    this.hand = new Hand(fst);
    this.bid = Number(snd);
  }
}

module.exports = Game;
