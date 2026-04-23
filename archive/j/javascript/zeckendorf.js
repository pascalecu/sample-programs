const arg = process.argv[2];

if (process.argv.length !== 3 || !/^\d+$/.test(arg)) {
    console.log("Usage: please input a non-negative integer");
    process.exit(0);
}

let n = parseInt(arg, 10);

if (n === 0) {
    console.log("");
    process.exit(0);
}

let a = 1;
let b = 2;

while (b <= n) {
    [a, b] = [b, a + b];
}

const result = [];

while (n > 0 && a > 0) {
    if (a <= n) {
        result.push(a);
        n -= a;

        const prev = b - a;
        [a, b] = [a - prev, prev];
    } else {
        let prevA = b - a;
        [a, b] = [b - a, a];
    }
}

console.log(result.join(", "));