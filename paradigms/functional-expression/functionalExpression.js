const abstractOperation = operation => (...operands) => (...values) =>
    operation(...(operands.map((operand) => operand(...values)
)));

const variable = name => {
    return (...values) => values[Object.keys(vars).indexOf(name)];
};

const add = abstractOperation((a, b) => a + b);
const subtract = abstractOperation((a, b) => a - b);
const multiply = abstractOperation((a, b) => a * b);
const divide = abstractOperation((a, b) => a / b);
const cnst = value => () => value;
const negate = abstractOperation(a => -a);

const vars = {
    "x": variable("x"),
    "y": variable("y"),
    "z": variable("z")
};

const med3 = abstractOperation((...operands) => {
    operands.sort((a, b) => a - b);
    return operands[Math.floor(operands.length / 2)];
});


const avg5 = abstractOperation((...operands) => {
    return operands.reduce((accumulator, operand) => accumulator + operand, 0) / operands.length;
});

const whiteSpaces = [' ', '\t', '\n', '\r', '\v', '\f', '\uFEFF', '\u00A0'];

const isWhiteSpace = (symbol) => whiteSpaces.includes(symbol);

const splitByWhiteSpaces = (str) => {
    let result = [];
    for (let i = 0; i < str.length; i++) {
        if (!isWhiteSpace(str[i])) {
            let startPosition = i;
            while (i < str.length && !isWhiteSpace(str[i])) {
                i++;
            }
            result.push(str.substring(startPosition, i));
        }
    }
    return result;
};

const pi = cnst(Math.PI);
const e = cnst(Math.E);

const constants = {
    'pi': pi,
    'e': e,
};

const operations = {
    'negate': [negate, 1],
    '+': [add, 2],
    '-': [subtract, 2],
    '*': [multiply, 2],
    '/': [divide, 2],
    'med3': [med3, 3],
    'avg5': [avg5, 5],
};

const parse = expression => {

    let tokens = splitByWhiteSpaces(expression);
    let exprStack = [];

    tokens.forEach(token => {
        if (token in operations) {
            let args = [];

            for (let i = 0; i < operations[token][1]; i++) {
                args.push(exprStack.pop());
            }

            args.reverse();
            exprStack.push(operations[token][0](...args));

        } else if (token in vars) {
            exprStack.push(vars[token]);
        } else if (token in constants) {
            exprStack.push(constants[token]);
        } else {
            exprStack.push(cnst(parseInt(token)));
        }
    });

    return exprStack[0];
};
