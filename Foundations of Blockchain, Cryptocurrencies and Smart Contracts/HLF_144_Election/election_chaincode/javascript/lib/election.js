'use strict';

const {Contract} = require('fabric-contract-api');

class Vote extends Contract {

    async initLedger(ctx) {
        console.info('============= START : Initialize Ledger ===========');
        
	const SHA256 = require('crypto-js/sha256');
	const timestamp = 'Dec 20 2022, 20:00:00';
	const decision = '-';
	const previous_hash = '0';

	const votes = [
            {
                timestamp,
                decision,
                previous_hash,
                hash: SHA256(timestamp + decision + previous_hash).toString()
            }
        ];

        for (let i = 0; i < votes.length; i++) {
            votes[i].docType = 'vote';
            await ctx.stub.putState('VOTE' + i, Buffer.from(JSON.stringify(votes[i])));
            console.info('Added <--> ', votes[i]);
        }
        console.info('============= END : Initialize Ledger ===========');
    }


    // Query for a specific vote
    async queryVote(ctx, voteNumber) {
        const voteAsBytes = await ctx.stub.getState(voteNumber); // get the vote from chaincode state
        if (!voteAsBytes || voteAsBytes.length === 0) {
            throw new Error(`${voteNumber} does not exist`);
        }
        console.log(voteAsBytes.toString());
        return voteAsBytes.toString();
    }

    // Cast a vote
    async castVote(ctx, decision) {
	
	const SHA256 = require('crypto-js/sha256');
	
	// Get current time for timestamp
	const currentTime = new Date();
	const timestamp = currentTime.toString().substring(4,24);

	// Get length of blockchain
	const startKey = 'VOTE0';
        const endKey = 'VOTE999';
        const iterator = await ctx.stub.getStateByRange(startKey, endKey);
        let chain = [];
	let check = true
        while (check === true) {
            const res = await iterator.next();

            if (res.value && res.value.value.toString()) {
                console.log(res.value.value.toString('utf8'));

                const Key = res.value.key;
                let Record;
                try {
                    Record = JSON.parse(res.value.value.toString('utf8'));
                } catch (err) {
                    console.log(err);
                    Record = res.value.value.toString('utf8');
                }
                chain.push({Key, Record});
            }
            if (res.done) {
                console.log('end of data');
                await iterator.close();
                console.info(chain);
         	check = false;
            }
        }
	// Get number of previous string by taking length of blockchain
	const block_number = chain.length;
	// Create new key based on previous block key
	var voteNumber = ''
	if (block_number > 0 && block_number < 10) {
	    voteNumber = 'VOTE0'+block_number.toString();
	} else {
	    voteNumber = 'VOTE'+block_number.toString();
	}
	// Get hash from previous block
	const previous_number = block_number-1;
	var previous_voteNumber = ''
        if (previous_number > 0 && previous_number < 10) {
            previous_voteNumber = 'VOTE0'+previous_number.toString();
        } else {
            previous_voteNumber = 'VOTE'+previous_number.toString();
        }
        const voteAsBytes = await ctx.stub.getState(previous_voteNumber);
        const json = JSON.parse(voteAsBytes);
        const previous_hash = json.hash;

	const vote = {
	    timestamp,
            docType: 'vote',
            decision,
            previous_hash,
            hash: SHA256(timestamp + decision + previous_hash).toString(),
        };

        await ctx.stub.putState(voteNumber, Buffer.from(JSON.stringify(vote)));
    }

    // Query all votes
    async queryAllVotes(ctx) {
        const startKey = 'VOTE0';
        const endKey = 'VOTE999';

        const iterator = await ctx.stub.getStateByRange(startKey, endKey);

        const allResults = [];
        while (true) {
            const res = await iterator.next();

            if (res.value && res.value.value.toString()) {
                console.log(res.value.value.toString('utf8'));

                const Key = res.value.key;
                let Record;
                try {
                    Record = JSON.parse(res.value.value.toString('utf8'));
                } catch (err) {
                    console.log(err);
                    Record = res.value.value.toString('utf8');
                }
                allResults.push({Key, Record});
            }
            if (res.done) {
                console.log('end of data');
                await iterator.close();
                console.info(allResults);
                return JSON.stringify(allResults);
            }
        }
    }

    // Make a poll of the current election
    async makePoll(ctx) {
	const startKey = 'VOTE0';
        const endKey = 'VOTE999';
        const iterator = await ctx.stub.getStateByRange(startKey, endKey);
	let chain = [];
        let check = true
        while (check === true) {
            const res = await iterator.next();

            if (res.value && res.value.value.toString()) {
                console.log(res.value.value.toString('utf8'));

                const Key = res.value.key;
                let Record;
                try {
                    Record = JSON.parse(res.value.value.toString('utf8'));
                } catch (err) {
                    console.log(err);
                    Record = res.value.value.toString('utf8');
                }
                chain.push({Key,Record}); // removed KEY
            }
            if (res.done) {
                console.log('end of data');
                await iterator.close();
                console.info(chain);
                check = false;
            }
	}

	var total = chain.length - 1
	var socialist = 0;
        var progressive = 0;
        var liberal = 0;
        var conservative = 0;
        for (let i = 0; i < chain.length; i++) {
  	    if (chain[i].Record.decision === 'socialist') {
		socialist += 1;
	    }
	    if (chain[i].Record.decision === 'progressive') {
                progressive += 1;
            }
	    if (chain[i].Record.decision === 'liberal') {
                liberal += 1;
            }
	    if (chain[i].Record.decision === 'conservative') {
                conservative += 1;
            }
	}
	var invalid = total - socialist - progressive - liberal - conservative

	const result = 'Recent Poll Results: Socialist Party: ' + socialist.toString() + '; Progressive Party: ' + progressive.toString() + '; Liberal Party: ' + liberal.toString() + '; Conservative Party: ' + conservative.toString() + '; Invalid Votes: ' + invalid.toString() + '. Total votes casted: ' + total.toString()
	return result
    }	

}

module.exports = Vote;
