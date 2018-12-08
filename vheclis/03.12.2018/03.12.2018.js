'use strict'
// https://adventofcode.com/2018/day/3

const fs = require('fs');

/*
 *  The path to the input file should be passed by argument, as the first argument,
 *  which is accessible at the index 2 of process.argv array
 */
const filePathIndex = 2;
const filePath = process.argv[filePathIndex];
const clothMatrix = {};
let overlapCount = 0;
let rightClothIDArray = [];

const lineReader = require('readline').createInterface({
    input: fs.createReadStream(filePath),
});

function isClothBeingUsedAtThisPosition(clothMatrix, yAxiosPoint, xAxiosPoint) {
    return clothMatrix[yAxiosPoint][xAxiosPoint] !== undefined;
}

function isYAxiosAlreadyInUse(clothMatrix, yAxiosPoint) {
    return clothMatrix[yAxiosPoint] !== undefined
}


lineReader.on('line', (line) => {
    const regexpToGetInput = /#(\d*)\D*(\d*),(\d*)\D*(\d*)x(\d*).*/g;
    const userInput = regexpToGetInput.exec(line);

    const id = parseInt(userInput[1]);
    const inchesFromLeft = parseInt(userInput[2]);
    const inchesFromTop = parseInt(userInput[3]);
    const inchesWide = parseInt(userInput[4]);
    const inchesTall = parseInt(userInput[5]);

    rightClothIDArray.push(id);
    for(let xAxiosPoint = inchesFromLeft; xAxiosPoint < inchesFromLeft + inchesWide; xAxiosPoint += 1) {
        for(let yAxiosPoint = inchesFromTop; yAxiosPoint < inchesFromTop + inchesTall; yAxiosPoint += 1) {
            if(!isYAxiosAlreadyInUse(clothMatrix, yAxiosPoint)) {
                clothMatrix[yAxiosPoint] = {};
                clothMatrix[yAxiosPoint][xAxiosPoint] = id;
            } else if(isClothBeingUsedAtThisPosition(clothMatrix, yAxiosPoint, xAxiosPoint)) {
                const indexOfOlderId = rightClothIDArray.indexOf(clothMatrix[yAxiosPoint][xAxiosPoint]);
                if (indexOfOlderId !== -1) {
                    rightClothIDArray.splice(indexOfOlderId, 1);
                }
                const indexOfActualId = rightClothIDArray.indexOf(id);
                if (indexOfActualId !== -1) {
                    rightClothIDArray.splice(indexOfActualId, 1);
                }
                clothMatrix[yAxiosPoint][xAxiosPoint] = 'X';         
            } else {
                clothMatrix[yAxiosPoint][xAxiosPoint] = id;
            }
        }
    }
});

lineReader.on('close', () => {
    for(let indexLine in clothMatrix) {
        if(clothMatrix.hasOwnProperty(indexLine)) {
            for(let indexColumn in clothMatrix[indexLine]) {
                if(clothMatrix[indexLine][indexColumn] == 'X'
                    && clothMatrix[indexLine].hasOwnProperty(indexColumn)) {
                    overlapCount += 1;
                }
            }
        }
    }
    console.log('Oh boy, its ugly but works. Count: ' + overlapCount);
    console.log('Right Cloth Id: ' + rightClothIDArray);
});
