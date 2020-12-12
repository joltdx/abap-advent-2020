const fs = require("fs");
const path = require("path");

const day = process.argv[2];
console.log("# Day " + day);

const files = fs.readdirSync("output").filter(n => n.startsWith("zcl_advent2020_convert") && n.endsWith(".clas.js"));
if (files.length !== 1) {
  throw "Converter class not found";
}

const req = require("@abaplint/runtime");
global.abap = new req.ABAP();
const clas = require("." + path.sep + "output" + path.sep + files[0]);

const inputFile = "." + path.sep + "input" + path.sep + "day" + day + ".txt";
const outputFile = "." + path.sep + "input" + path.sep + "day" + day + "_abap_string.txt";
const input = fs.readFileSync(inputFile).toString();

const className = Object.keys(clas)[0];
const instance = new clas[className]();
const methodName = "convert_to_abap_string";
console.log("Class: " + className.toUpperCase());
console.log("Method: " + methodName.toUpperCase());
console.log("Input: " + inputFile);
console.log("Output: " + outputFile);

const result = instance[methodName]( {input: input} );
const output = abap.console.get();
if (output && output !== "") {
  console.dir(output);
}

fs.writeFile(outputFile, result.get(), function (err) {
  if (err) return console.log(err);
});
