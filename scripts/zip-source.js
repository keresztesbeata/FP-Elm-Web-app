import fs from 'fs';
import archiver from 'archiver';
import child_process from 'child_process';
import * as readline from 'node:readline';


const cmd = process.platform === "win32" ? "npx.cmd elm-test --report json" : "npx elm-test --report json";
const opts = process.platform === "win32" ? {shell: true} : {};

const testResult = child_process.spawnSync(cmd, opts);

if (!fs.existsSync("./logs")) {
    fs.mkdirSync("./logs");
}

fs.writeFileSync("./logs/test-logs.json", testResult.stdout);

if (testResult.status !== 0 || testResult.error) {
    const rl = readline.createInterface({input: process.stdin, output: process.stdout});

    let msg = "";
    if (testResult.error) {
        msg = `Failed to spawn test process: ${testResult.error.message}\nAre you sure you want to submit your assignment? [N/y]\n`;
    } else {
        msg = `Test process returned status: ${testResult.status}\nNot all tests passed. Are you sure you want to submit your assignment? [N/y]\n`;
    }

    rl.question(msg, answer => {
        if (answer.toLowerCase() === "y" || answer.toLowerCase() === "yes") {
            console.log("Ok, zipping files");
            archiveFiles();
        } else {
            console.log("Aborting");
            process.exit(0);
        }
        rl.close();
    });
} else {
    console.log(`Test process returned status: ${testResult.status}`);
    archiveFiles();
}

function archiveFiles() {
    const output = fs.createWriteStream('src.zip');
    const archive = archiver('zip');

    output.on('close', function () {
        console.log('Zipped source files');
        console.log("Size: " + archive.pointer());
    });

    archive.on('error', function(err){
        throw err;
    });

    archive.pipe(output);

    archive.directory('./src/', 'src');
    archive.file('elm.json');
    archive.file('logs/test-logs.json');
    archive.directory('./tests', 'tests');

    archive.finalize();
}