type Grid = number[][];

class Sudoku {
  grid: Grid;

  constructor(grid?: Grid) {
    // 初始化：空盘（全 0）或传入已有网格
    this.grid = grid ?? Array.from({ length: 9 }, () => Array(9).fill(0));
  }

  // 检查在 (r,c) 放 num 是否可行
  private isValid(r: number, c: number, num: number): boolean {
    // 行列检查
    for (let i = 0; i < 9; i++) {
      if (this.grid[r][i] === num || this.grid[i][c] === num) return false;
    }
    // 宫内检查
    const br = Math.floor(r / 3) * 3;
    const bc = Math.floor(c / 3) * 3;
    for (let i = 0; i < 3; i++) {
      for (let j = 0; j < 3; j++) {
        if (this.grid[br + i][bc + j] === num) return false;
      }
    }
    return true;
  }

  // 回溯求解；找到一组解就返回 true
  solve(): boolean {
    for (let r = 0; r < 9; r++) {
      for (let c = 0; c < 9; c++) {
        if (this.grid[r][c] === 0) {
          for (let num = 1; num <= 9; num++) {
            if (this.isValid(r, c, num)) {
              this.grid[r][c] = num;
              if (this.solve()) return true;
              this.grid[r][c] = 0;
            }
          }
          return false; // 1–9 都不行，则回溯
        }
      }
    }
    return true; // 没有空位，已完全填满
  }

  // 生成一个谜题盘面：先随机填满再挖洞
  generate(clues: number = 30): void {
    // 1. 先随机填满一个合法完整解
    this.fillDiagonal();
    this.solve();

    // 2. 挖去 (81 - clues) 个格子
    const totalRemovals = 81 - Math.max(Math.min(clues, 81), 17);
    let removed = 0;
    while (removed < totalRemovals) {
      const r = Math.floor(Math.random() * 9);
      const c = Math.floor(Math.random() * 9);
      if (this.grid[r][c] !== 0) {
        this.grid[r][c] = 0;
        removed++;
      }
    }
  }

  // 随机填充每个 3×3 宫的对角宫，帮助加速生成
  private fillDiagonal() {
    for (let k = 0; k < 9; k += 3) {
      this.fillBox(k, k);
    }
  }

  private fillBox(row: number, col: number) {
    const nums = this.shuffle([1,2,3,4,5,6,7,8,9]);
    let idx = 0;
    for (let i = 0; i < 3; i++) {
      for (let j = 0; j < 3; j++) {
        this.grid[row + i][col + j] = nums[idx++];
      }
    }
  }

  // Fisher–Yates 洗牌
  private shuffle(arr: number[]): number[] {
    for (let i = arr.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [arr[i], arr[j]] = [arr[j], arr[i]];
    }
    return arr;
  }

  // 格式化输出到控制台
  print(): void {
    for (let r = 0; r < 9; r++) {
      console.log(this.grid[r].map(n => (n === 0 ? '.' : n)).join(' '));
    }
  }
}

// —— 使用示例 ——
function demo() {
  const sudoku = new Sudoku();
  // 生成一个保留 30 个数字的谜题
  sudoku.generate(30);
  console.log('—— 生成的谜题 ——');
  sudoku.print();

  // 复制一份用来求解
  const puzzle = sudoku.grid.map(row => [...row]);
  const solver = new Sudoku(puzzle);
  solver.solve();
  console.log('—— 求解结果 ——');
  solver.print();
}

demo();
