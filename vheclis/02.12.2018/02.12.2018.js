'use strict'
// https://adventofcode.com/2018/day/2 
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

let countOfDoubles = 0;
let countOfTriples = 0;
const arrayOfStrings = [];

/**
 * @param {String} string 
 * @param {Number} index 
 * @returns {String}
 */
function getStringWithoutCharacterOnIndex(string, index) {
    if (index === 0) {
        return string.slice(1);
    } else if (index === (string.length - 1)) {
        return string.slice(0, -1);
    } 
    return string.slice(0, index) + string.slice(index + 1);
}

lineReader.on('line', function (line) {
    const arrayOfCharacters = line.split('');
    const objectCountCharacters = {};
    arrayOfCharacters.forEach(character => {
        if (objectCountCharacters[character] !== undefined) {
            objectCountCharacters[character] += 1;
        } else {
            objectCountCharacters[character] = 1;
        }
    });

    let shouldCountDouble = false;
    let shouldCountTriple = false;
    for(let index in objectCountCharacters) {
        if (objectCountCharacters.hasOwnProperty(index)) {
            if (objectCountCharacters[index] === 2) {
                shouldCountDouble = true;
            } else if (objectCountCharacters[index] === 3) {
                shouldCountTriple = true;
            }
        }
    }

    if (shouldCountDouble) {
        countOfDoubles += 1;
    }

    if (shouldCountTriple) {
        countOfTriples += 1;
    }
    arrayOfStrings.push(line);
});

lineReader.on('close', function () {
    console.log('Doubles: ' + countOfDoubles + ', Triples: ' + countOfTriples);
    console.log('Checksum: ' + countOfDoubles * countOfTriples);
    let commonLetters = '';

    // I'm not proud of the following lines yet, hehe
    arrayOfStrings.forEach(stringId => {
        arrayOfStrings.forEach(stringIdToCompare => {
            if (stringId !== stringIdToCompare){
                let diffCharactersCount = 0;
                let diffIndex;
                for(let i = 0; i < stringId.length; i += 1) {
                    if (stringId[i] !== stringIdToCompare[i]) {
                        diffCharactersCount += 1;
                        diffIndex = i;
                    }
                }
                if (diffCharactersCount === 1) {
                    commonLetters = getStringWithoutCharacterOnIndex(stringId, diffIndex);
                }
            }
        });
    });
    console.log('The common letters are: ' + commonLetters);
});
