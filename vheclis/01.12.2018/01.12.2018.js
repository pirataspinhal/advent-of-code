'use strict'
// https://adventofcode.com/2018/day/1 
const fs = require('fs');

/*
 *  The path to the input file should be passed by argument, as the first argument,
 *  which is accessible at the index 2 of process.argv array
 */
const filePathIndex = 2;
const filePath = process.argv[filePathIndex];

const lineReader = require('readline').createInterface({
    input: fs.createReadStream(filePath),
});

let finalFrequency = 0;
let repeatedFrequency = -1;
const arrayOfNumbersToChangeFrequency = [];
const arrayOfFrequencies = [0];

function processFrequency(numberToChangeFrequency, frequency) {
    frequency += numberToChangeFrequency;
    if (arrayOfFrequencies.includes(frequency)) {
        repeatedFrequency = frequency;
    }
    arrayOfFrequencies.push(frequency);
    return frequency;
}

function getNextIndexValueLimitedByArrayLength(index, array) {
    return (index + 1) % array.length;
}

lineReader.on('line', (line) => {
    const numberToChangeFrequency = parseInt(line);
    arrayOfNumbersToChangeFrequency.push(numberToChangeFrequency);
    finalFrequency = processFrequency(numberToChangeFrequency, finalFrequency);
});

lineReader.on('close', () => {
    let frequencyToTestRepeat = finalFrequency;
    for(let i = 0;
        repeatedFrequency === -1;
        i = getNextIndexValueLimitedByArrayLength(i, arrayOfNumbersToChangeFrequency)) {
        frequencyToTestRepeat =
            processFrequency(arrayOfNumbersToChangeFrequency[i], frequencyToTestRepeat);
    }
    console.log('Frequency after changes: ' + finalFrequency);
    console.log('Repeated frequency: ' + repeatedFrequency);
});
