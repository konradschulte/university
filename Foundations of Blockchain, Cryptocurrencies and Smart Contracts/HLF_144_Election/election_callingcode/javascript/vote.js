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

    return new Promise(resolve => inter.question("Who do you want to vote for (possible is socialist, progressive, liberal, conservative)? ", answer => {
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

        // cast Vote
	const name = await readInput();
	await contract.submitTransaction('castVote', name.toString());
        console.log('Vote ("' + name.toString() + '") has been casted!');

        // Disconnect from the gateway.
        await gateway.disconnect();

    } catch (error) {
        console.error(`Failed to cast vote: ${error}`);
        process.exit(1);
    }
}

main();
