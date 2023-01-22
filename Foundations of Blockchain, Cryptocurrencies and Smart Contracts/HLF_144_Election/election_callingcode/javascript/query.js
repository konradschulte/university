/*
 * SPDX-License-Identifier: Apache-2.0
 */

'use strict';

// Define a function for user input
const readline = require('readline');

function readInput() {
    const inter = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
    });

    return new Promise(resolve => inter.question("Which vote do you want to query for (possible is either key of vote (VOTE0, VOTE01, VOTE02, ...) or ALL)? ", answer => {
        inter.close();
        resolve(answer);
    }))
}

const { FileSystemWallet, Gateway } = require('fabric-network');
const path = require('path');

const ccpPath = path.resolve(__dirname, '..', '..', 'first-network', 'connection-org1.json');

async function main() {
    try {

        // Create a new file system based wallet for managing identities.
        const walletPath = path.join(process.cwd(), 'wallet');
        const wallet = new FileSystemWallet(walletPath);
        console.log(`Wallet path: ${walletPath}`);

        // Check to see if we've already enrolled the user.
        const userExists = await wallet.exists('user1');
        if (!userExists) {
            console.log('An identity for the user "user1" does not exist in the wallet');
            console.log('Run the registerUser.js application before retrying');
            return;
        }

        // Create a new gateway for connecting to our peer node.
        const gateway = new Gateway();
        await gateway.connect(ccpPath, { wallet, identity: 'user1', discovery: { enabled: true, asLocalhost: true } });

        // Get the network (channel) our contract is deployed to.
        const network = await gateway.getNetwork('mychannel');

        // Get the contract from the network.
        const contract = network.getContract('election');
	
	// Ask user which vote he wants to query (either one or all)
	const name = await readInput()
	
	if (name.toString() === 'ALL') {
	    var result = await contract.evaluateTransaction('queryAllVotes');
	} else {
	    var result = await contract.evaluateTransaction('queryVote',name.toString());
	}

        // Evaluate the specified transaction.
        //const result = await contract.evaluateTransaction('queryAllVotes');
        console.log(`Query result for "${name.toString()}" is: ${result.toString()}`);

    } catch (error) {
        console.error(`Failed to evaluate transaction: ${error}`);
        process.exit(1);
    }
}

main();
