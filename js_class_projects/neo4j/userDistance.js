const { driver, session, oneToManyUserDistanceLengthRequest } = require('./src/shortestPath')

const AWS = require('aws-sdk');
var url = require('url');
const URL1 = require('url').Url;
exports.handler = (event, context, callback) => {
  console.log("EVENT " + JSON.stringify(event))
    //var url = "https://search-movie2-elastic-search-545njwupxfaqkigpqncm5xbqm4.us-east-1.es.amazonaws.com/users/user/_search"
    var header = "Content-Type: application/json"
    var jsonQuery = { "query":{"bool":{"must":[{"term":{"genre":"comedy"}}, {"bool":{"filter":{"geo_distance":{"distance":"3000km","location":{"lat":40,"lon":-70}}}}}]}}}
  //  curl -XGET 'https://search-movie2-elastic-search-545njwupxfaqkigpqncm5xbqm4.us-east-1.es.amazonaws.com/users/user/_search' -H 'Content-Type: application/json' -d' { "query":{"bool":{"must":[{"term":{"genre":"comedy"}}, {"bool":{"filter":{"geo_distance":{"distance":"3000km","location":{"lat":40,"lon":-70}}}}}]}}}'
	var currentUserId
	if (!event || !event['userId'] ) {
	    console.log(" App ElasticSearch query is not present.")
	    currentUserId = "robot01"
	} else {
		currentUserId = event['userId']
	}
	if (!event || !event['esQuery'] ) {
	    console.log(" App ElasticSearch query is not present.")
	} else {
		if (!event['esQuery']['query'] || !event['esQuery']['query']['bool'] || !event['esQuery']['query']['bool']['must'] || event['esQuery']['query']['bool']['must'].length == 0 ) {
	    	console.log(" App ElasticSearch query is not complete.")
		} else {
			jsonQuery = event['esQuery']
		}
	}
    var querystring = require('querystring');
    const https = require("https");
    const callback_url =
    'https://search-movie2-elastic-search-545njwupxfaqkigpqncm5xbqm4.us-east-1.es.amazonaws.com/users/user/_search';
    var callbackURLparsed = url.parse(callback_url);
    var api_agent = 'My node Server';
    var filteredUsers = [];
    try{
       // postData = { "query":{"bool":{"must":[{"term":{"genre":"comedy"}}, {"bool":{"filter":{"geo_distance":{"distance":"3000km","location":{"lat":40,"lon":-70}}}}}]}}};
        postData = jsonQuery
        postBody = querystring.stringify(postData);
        //init your options object after you call querystring.stringify because you  need
        // the return string for the 'content length' header
        options = {
            host: callbackURLparsed.hostname,
            port: callbackURLparsed.port,
            path: callbackURLparsed.path,
            method: 'POST',
            json: true,
            headers : {
                //'Content-Type': 'application/x-www-form-urlencoded',
                'Content-Type': 'application/json',
                //'Accept': 'application/json',
                'User-agent':  api_agent ,
                'Origin': 'https://search-movie2-elastic-search-545njwupxfaqkigpqncm5xbqm4.us-east-1.es.amazonaws.com',
                'Referer': callbackURLparsed.protocol + '//' + callbackURLparsed.host
                // 'Content-Length': postBody.length
            }
        };

        var postreq = https.request(options, function (response) {
            //Handle the response
            console.log('using hostname: ' + callbackURLparsed.hostname);
            console.log('using port: ' + callbackURLparsed.port);
            console.log('using path: ' + callbackURLparsed.path);
            console.log('using headers: ' + options.headers.Referer + ' , ' + options.headers.Accept );
            console.log('In response');
           // response.setEncoding('utf8');
            console.log("BEFORE RESPONSE ON: " + response);
            var body = '';
            response.on('data', function(chunk) {
                body += chunk;
            });
            response.on('end', function() {
                let objectENData = body.toString('utf-8');
                console.log("cbresponse to string " + objectENData);
                var dataEnd = JSON.parse(objectENData)
                console.log("RAW DATA: " + dataEnd);
                
            //    if (!dataEnd['hits'] || !dataEnd['sentiment']['document'] || !dataEnd['sentiment']['document']['label']) {
                if (!dataEnd['hits'] || !dataEnd['hits']['hits']) {
                    filteredUsers = 'Couldn\'t extract from elastic search';
                } else {
                    for (i = 0; i < dataEnd['hits']['hits'].length; i++) { 
                        filteredUsers.push(dataEnd['hits']['hits'][i]['_source']['id'])
                        filteredUser =  dataEnd['hits']['hits'][i]['_source']['id'];
                        filteredUserName =  dataEnd['hits']['hits'][i]['_source']['user_name'];
                        console.log(" USERID " + filteredUser + " USERNAME " + filteredUserName);
                    }
                }
                console.log("List of users " + JSON.stringify(filteredUsers))
  				filteredUsers.push("robot07")
  				filteredUsers.push("robot02")
  				filteredUsers.push("robot04")
                queryBody = {"userId":currentUserId, "friendslist": filteredUsers}
                console.log(" Creating users distance query with list")  
  				oneToManyUserDistanceLengthRequest(queryBody)
    			.fork(
			      err => {
			        console.error(err)
        			driver.close()
		        	session.close()
        			callback({ status: 'fail' }, null)
  			    },
 			     data => {
 			       console.log(data)
 			       driver.close()
 			       session.close()
 			       callback(null, data)
  			    }
  		  )
                
            });
        });
        postreq.write(JSON.stringify(postData));
        postreq.end();
    }catch(e) {
		console.log(e);
		callback(e);
	}
}

